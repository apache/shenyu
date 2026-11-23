/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.springboot.starter.client.common.lifecycle;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.disruptor.subcriber.ShenyuClientURIExecutorSubscriber;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.availability.AvailabilityChangeEvent;
import org.springframework.boot.availability.ReadinessState;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;

import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

/**
 * Bridges Spring application readiness and graceful shutdown to the registered business
 * instance's serving state through the repository SPI the server side already accepts
 * (#8042 contract): {@code ready} releases a previous refusal, {@code notReady} refuses
 * new work while staying alive (never offline), and {@code draining} at context close
 * lets the gateways stop selecting this instance while in-flight work still completes —
 * the context-close event precedes the web server's graceful shutdown and the existing
 * JVM offline hooks, which remain the offline backstop.
 *
 * <p>Updates are emitted only at real state transitions (repeated Spring events are
 * idempotent), for exactly the URIs the registration flow registered, passed through
 * unchanged so namespace/protocol/identity binding stays intact. A failing update is
 * logged and never blocks or aborts shutdown, and it never degrades into an offline.
 * Heartbeats are stopped before draining so no renewal races the drain; repositories
 * without the capability keep their default no-ops, and the Admin-side markers make
 * heartbeats unable to clear a drain anyway.</p>
 *
 * <p>This is an opt-in adapter ({@code shenyu.register.lifecycle-bridge.enabled=true});
 * shipped behavior changes nothing by default.</p>
 */
public class ServingStateLifecycleBridge implements ApplicationListener<AvailabilityChangeEvent<?>> {

    private enum State {
        INIT, READY, NOT_READY, DRAINING
    }

    private enum ReadinessTarget {
        READY, NOT_READY
    }

    private static final Logger LOG = LoggerFactory.getLogger(ServingStateLifecycleBridge.class);

    private final ShenyuClientRegisterRepository repository;

    private final Supplier<List<URIRegisterDTO>> registeredUrisProvider;

    private final Runnable renewalStopper;

    private final AtomicReference<State> state = new AtomicReference<>(State.INIT);

    /**
     * Instantiates the bridge against the shared registration publisher.
     *
     * @param repository the register repository the serving-state updates go through
     */
    public ServingStateLifecycleBridge(final ShenyuClientRegisterRepository repository) {
        this(repository,
                () -> {
                    ShenyuClientURIExecutorSubscriber subscriber =
                            ShenyuClientRegisterEventPublisher.getInstance().getUriSubscriber();
                    return Objects.nonNull(subscriber) ? subscriber.registeredUris() : List.of();
                },
                () -> {
                    ShenyuClientURIExecutorSubscriber subscriber =
                            ShenyuClientRegisterEventPublisher.getInstance().getUriSubscriber();
                    if (Objects.nonNull(subscriber)) {
                        subscriber.shutdown();
                    }
                });
    }

    ServingStateLifecycleBridge(final ShenyuClientRegisterRepository repository,
                                final Supplier<List<URIRegisterDTO>> registeredUrisProvider,
                                final Runnable renewalStopper) {
        this.repository = repository;
        this.registeredUrisProvider = registeredUrisProvider;
        this.renewalStopper = renewalStopper;
    }

    /**
     * Application ready: the accepted readiness boundary, emitted as {@code ready}.
     *
     * @param event the ready event
     */
    @EventListener
    public void onApplicationReady(final ApplicationReadyEvent event) {
        transitionTo(ReadinessTarget.READY);
    }

    /**
     * Spring readiness changes: {@code notReady} refuses new work while preserving
     * liveness, {@code ready} releases a previous refusal.
     *
     * @param event the availability change event
     */
    @Override
    public void onApplicationEvent(final AvailabilityChangeEvent<?> event) {
        if (event.getState() instanceof ReadinessState) {
            transitionTo(((ReadinessState) event.getState()) == ReadinessState.ACCEPTING_TRAFFIC
                    ? ReadinessTarget.READY : ReadinessTarget.NOT_READY);
        }
    }

    /**
     * Context close (graceful shutdown start, while the JVM may stay alive): stop renewal
     * work, then request draining for every registered URI. In-flight request draining and
     * the final offline remain the responsibilities of the web server lifecycle and the
     * existing JVM hooks.
     *
     * @param event the context closed event
     */
    @EventListener
    public void onContextClosed(final ContextClosedEvent event) {
        // a real transition only: compareAndSet(DRAINING, DRAINING) would "succeed" on
        // repeated close events and drain twice
        if (!state.compareAndSet(State.INIT, State.DRAINING)
                && !state.compareAndSet(State.READY, State.DRAINING)
                && !state.compareAndSet(State.NOT_READY, State.DRAINING)) {
            return;
        }
        stopHeartbeats();
        for (URIRegisterDTO uri : registeredUris()) {
            emit(uri, "draining", () -> repository.draining(uri));
        }
    }

    private void transitionTo(final ReadinessTarget target) {
        if (State.DRAINING == state.get()) {
            return;
        }
        if (target == ReadinessTarget.READY) {
            // READY is allowed from INIT (first readiness) and NOT_READY (recovery);
            // a repeated READY event emits nothing
            if (!state.compareAndSet(State.INIT, State.READY)
                    && !state.compareAndSet(State.NOT_READY, State.READY)) {
                return;
            }
        } else {
            // NOT_READY only makes sense once the instance has been ready
            if (!state.compareAndSet(State.READY, State.NOT_READY)) {
                return;
            }
        }
        for (URIRegisterDTO uri : registeredUris()) {
            emit(uri, target.name().toLowerCase(Locale.ROOT),
                    target == ReadinessTarget.READY ? () -> repository.ready(uri) : () -> repository.notReady(uri));
        }
    }

    private void stopHeartbeats() {
        renewalStopper.run();
    }

    private List<URIRegisterDTO> registeredUris() {
        return registeredUrisProvider.get();
    }

    private void emit(final URIRegisterDTO uri, final String action, final Runnable update) {
        try {
            update.run();
        } catch (RuntimeException ex) {
            // a failed serving-state update must never block shutdown or become an offline;
            // the Admin keeps the previous serving state for this identity
            LOG.warn("Failed to emit {} for {}:{} in namespace {}, keeping the previous serving state",
                    action, uri.getProtocol(), uri.getHost() + ":" + uri.getPort(), uri.getNamespaceId(), ex);
        }
    }
}
