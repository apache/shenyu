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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.common.metrics.AgentAdmissionMetrics;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import java.util.Objects;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentResourceAdmissionTest {

    private AgentResourceAdmission controller;

    @AfterEach
    void closeController() {
        if (Objects.nonNull(controller)) {
            controller.close();
        }
    }

    @Test
    void rejectQueueOverflowWithExplicitOverload() throws Exception {
        controller = new AgentResourceAdmission(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        Sinks.Empty<Void> first = Sinks.empty();
        ExecutorService executor = Executors.newFixedThreadPool(3);
        try {
            final Future<?> active = executor.submit(() -> StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(30), first.asMono())).expectComplete().verify());
            await(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
            final Future<?> queued = executor.submit(() -> StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(30), Mono.empty()))
                    .expectComplete().verify());
            awaitQueued(controller, 1);
            StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(30), Mono.empty()))
                    .expectErrorMatches(error -> error instanceof AgentResourceAdmission.AgentAdmissionException
                            || error instanceof java.util.concurrent.TimeoutException).verify();
            first.tryEmitEmpty();
            active.get(5, TimeUnit.SECONDS);
            queued.get(5, TimeUnit.SECONDS);
        } finally {
            executor.shutdownNow();
        }
        assertEquals(1, controller.snapshot().get(0).rejected());
    }

    @Test
    void rejectQueuedWaiterAfterDeadline() throws Exception {
        controller = new AgentResourceAdmission(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        Sinks.Empty<Void> active = Sinks.empty();
        StepVerifier.create(controller.withLocal(Instant.now().plusMillis(100), active.asMono())).expectComplete().verifyLater();
        await(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
        AtomicInteger calls = new AtomicInteger();
        StepVerifier.create(controller.withLocal(Instant.now().plusMillis(100), Mono.fromCallable(calls::incrementAndGet)))
                .expectErrorMatches(error -> error instanceof AgentResourceAdmission.AgentAdmissionException
                        && ((AgentResourceAdmission.AgentAdmissionException) error).isTimeout()).verify(Duration.ofSeconds(2));
        assertEquals(0, calls.get());
        active.tryEmitEmpty();
    }

    @Test
    void cancellationRemovesQueuedWaiterWithoutReleasingActivePermit() throws Exception {
        controller = new AgentResourceAdmission(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        Sinks.Empty<Void> active = Sinks.empty();
        StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(5), active.asMono())).expectComplete().verifyLater();
        await(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
        reactor.core.Disposable queued = controller.withLocal(Instant.now().plusSeconds(5), Mono.just("unused"))
                .subscribe();
        awaitQueued(controller, 1);
        queued.dispose();
        awaitQueued(controller, 0);
        assertEquals(1, controller.snapshot().get(0).inFlight());
        active.tryEmitEmpty();
        await(controller, AgentAdmissionMetrics.Scope.GLOBAL, 0);
    }

    @Test
    void closeRejectsQueuedWaiterWithoutExecutingAction() throws Exception {
        controller = new AgentResourceAdmission(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        Sinks.Empty<Void> active = Sinks.empty();
        StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(5), active.asMono())).expectComplete().verifyLater();
        await(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
        AtomicInteger calls = new AtomicInteger();
        java.util.concurrent.atomic.AtomicReference<Throwable> failure = new java.util.concurrent.atomic.AtomicReference<>();
        controller.withLocal(Instant.now().plusSeconds(5), Mono.fromCallable(calls::incrementAndGet))
                .subscribe(value -> { }, failure::set);
        awaitQueued(controller, 1);
        controller.close();
        org.awaitility.Awaitility.await().atMost(Duration.ofSeconds(2)).until(() -> Objects.nonNull(failure.get()));
        assertTrue(failure.get() instanceof AgentResourceAdmission.AgentAdmissionException);
        assertEquals(0, calls.get());
        assertEquals(0, controller.snapshot().get(0).queued());
        active.tryEmitEmpty();
    }

    @Test
    void isolateSlowRemoteServerFromIndependentTarget() {
        controller = new AgentResourceAdmission(new AgentGatewayAdmissionConfig(4, 4, 4, 4, 1, 1));
        Sinks.Empty<Void> slow = Sinks.empty();
        StepVerifier.create(controller.withRemote("slow", Instant.now().plusSeconds(2), slow.asMono())).expectComplete().verifyLater();
        StepVerifier.create(controller.withRemote("fast", Instant.now().plusSeconds(2), Mono.just("ok")))
                .expectNext("ok").verifyComplete();
        slow.tryEmitEmpty();
    }

    @Test
    void releasePermitsExactlyOnceOnTerminalPaths() throws Exception {
        controller = new AgentResourceAdmission(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        Sinks.Empty<Void> release = Sinks.empty();
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Future<?> first = executor.submit(() -> StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), release.asMono())).expectComplete().verify());
            await(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
            release.tryEmitEmpty();
            first.get(5, TimeUnit.SECONDS);
            AtomicInteger calls = new AtomicInteger();
            StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), Mono.fromCallable(calls::incrementAndGet))).expectNext(1).verifyComplete();
            assertEquals(1, calls.get());
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    void replacingControllerKeepsSharedAccountingAndAppliesNewLimits() {
        AgentMcpResourceAdmission.ResourceAdmissionController first =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        AgentMcpResourceAdmission.ResourceAdmissionController second =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(2, 1, 1, 1, 1, 1));
        try {
            first.replace(second);
            List<AgentAdmissionMetrics.Snapshot> snapshots = AgentAdmissionMetrics.snapshots();
            assertEquals(2, snapshots.size());
            assertEquals(2, snapshots.stream().filter(state -> state.scope() == AgentAdmissionMetrics.Scope.GLOBAL)
                    .mapToInt(AgentAdmissionMetrics.Snapshot::inFlightLimit).sum(), "replacement metrics must stay retired");
        } finally {
            first.close();
        }
    }

    @Test
    void everyReplacementUpdatesActiveControllerAndFinalCloseRemovesMetrics() throws Exception {
        AgentMcpResourceAdmission.ResourceAdmissionController controller =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 1, 1, 1, 1, 1));
        try {
            controller.replace(AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(2, 1, 1, 1, 1, 1)));
            controller.replace(AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(3, 1, 1, 1, 1, 1)));
            Sinks.Empty<Void> first = Sinks.empty();
            Sinks.Empty<Void> second = Sinks.empty();
            ExecutorService executor = Executors.newFixedThreadPool(2);
            try {
                final Future<?> one = executor.submit(() -> StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), first.asMono())).expectComplete().verify());
                final Future<?> two = executor.submit(() -> StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), second.asMono())).expectComplete().verify());
                awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 2);
                first.tryEmitEmpty();
                second.tryEmitEmpty();
                one.get(5, TimeUnit.SECONDS);
                two.get(5, TimeUnit.SECONDS);
            } finally {
                executor.shutdownNow();
            }
            StepVerifier.create(controller.withLocal(Instant.now(), Mono.just("third"))).expectNext("third").verifyComplete();
        } finally {
            controller.close();
        }
        assertTrue(AgentAdmissionMetrics.snapshots().stream()
                .filter(snapshot -> snapshot.scope() == AgentAdmissionMetrics.Scope.GLOBAL)
                .noneMatch(snapshot -> snapshot.inFlightLimit() == 3), "active admission metrics must be removed after close");
    }

    @Test
    void unchangedReplacementDoesNotResetActiveRequestAccounting() throws Exception {
        AgentMcpResourceAdmission.ResourceAdmissionController controller =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 0, 1, 0, 1, 0));
        Sinks.Empty<Void> active = Sinks.empty();
        try {
            StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), active.asMono()))
                    .expectComplete().verifyLater();
            awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
            controller.replace(AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 0, 1, 0, 1, 0)));
            AtomicInteger calls = new AtomicInteger();
            StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), Mono.fromCallable(calls::incrementAndGet)))
                    .expectError(AgentResourceAdmission.AgentAdmissionException.class).verify(Duration.ofSeconds(2));
            assertEquals(0, calls.get());
            assertEquals(1, controller.snapshot().stream()
                    .filter(snapshot -> snapshot.scope() == AgentAdmissionMetrics.Scope.GLOBAL)
                    .findFirst().orElseThrow().inFlight());
        } finally {
            active.tryEmitEmpty();
            controller.close();
        }
    }

    @Test
    void localLimitReloadInstallsBeforePromotingQueuedRequests() throws Exception {
        AgentMcpResourceAdmission.ResourceAdmissionController controller =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 4, 1, 0, 1, 4));
        List<Sinks.Empty<Void>> actions = new ArrayList<>();
        try {
            for (int i = 0; i < 4; i++) {
                Sinks.Empty<Void> action = Sinks.empty();
                actions.add(action);
                StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), action.asMono()))
                        .expectComplete().verifyLater();
                if (i == 0) {
                    awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
                }
            }
            awaitQueued(controller, AgentAdmissionMetrics.Scope.GLOBAL, 3);

            controller.replace(AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(4, 4, 4, 4, 1, 4)));

            awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 4);
            awaitActive(controller, AgentAdmissionMetrics.Scope.LOCAL, 4);
            assertEquals(0, snapshot(controller, AgentAdmissionMetrics.Scope.GLOBAL).queued());
            assertEquals(0, snapshot(controller, AgentAdmissionMetrics.Scope.LOCAL).queued());
        } finally {
            actions.forEach(Sinks.Empty::tryEmitEmpty);
            controller.close();
        }
    }

    @Test
    void remoteLimitReloadInstallsBeforePromotingQueuedRequests() throws Exception {
        AgentMcpResourceAdmission.ResourceAdmissionController controller =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 4, 4, 4, 1, 0));
        List<Sinks.Empty<Void>> actions = new ArrayList<>();
        try {
            for (int i = 0; i < 4; i++) {
                Sinks.Empty<Void> action = Sinks.empty();
                actions.add(action);
                StepVerifier.create(controller.withRemote("orders", Instant.now().plusSeconds(2), action.asMono()))
                        .expectComplete().verifyLater();
                if (i == 0) {
                    awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
                }
            }
            awaitQueued(controller, AgentAdmissionMetrics.Scope.GLOBAL, 3);

            controller.replace(AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(4, 4, 4, 4, 4, 4)));

            awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 4);
            awaitActive(controller, AgentAdmissionMetrics.Scope.REMOTE, 4);
            assertEquals(0, snapshot(controller, AgentAdmissionMetrics.Scope.GLOBAL).queued());
            assertEquals(0, snapshot(controller, AgentAdmissionMetrics.Scope.REMOTE).queued());
        } finally {
            actions.forEach(Sinks.Empty::tryEmitEmpty);
            controller.close();
        }
    }

    @Test
    void globalLimitReloadPromotesAllNewlyAvailableWaiters() throws Exception {
        AgentMcpResourceAdmission.ResourceAdmissionController controller =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(1, 4, 4, 4, 4, 4));
        List<Sinks.Empty<Void>> actions = new ArrayList<>();
        try {
            for (int i = 0; i < 4; i++) {
                Sinks.Empty<Void> action = Sinks.empty();
                actions.add(action);
                StepVerifier.create(controller.withLocal(Instant.now().plusSeconds(2), action.asMono()))
                        .expectComplete().verifyLater();
                if (i == 0) {
                    awaitActive(controller, AgentAdmissionMetrics.Scope.GLOBAL, 1);
                }
            }
            awaitQueued(controller, AgentAdmissionMetrics.Scope.GLOBAL, 3);

            controller.replace(AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(4, 4, 4, 4, 4, 4)));

            AgentAdmissionMetrics.Snapshot global = snapshot(controller, AgentAdmissionMetrics.Scope.GLOBAL);
            assertEquals(4, global.inFlight());
            assertEquals(0, global.queued());
            assertEquals(4, snapshot(controller, AgentAdmissionMetrics.Scope.LOCAL).inFlight());
        } finally {
            actions.forEach(Sinks.Empty::tryEmitEmpty);
            controller.close();
        }
    }

    @Test
    void redundantReplacementIsClosedWithoutReplacingActiveLimits() {
        AgentMcpResourceAdmission.ResourceAdmissionController active =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(2, 1, 1, 1, 1, 1));
        AgentMcpResourceAdmission.ResourceAdmissionController stale =
                AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(3, 1, 1, 1, 1, 1));
        try {
            stale.close();
            active.replace(stale);
            assertEquals(2, active.snapshot().stream()
                    .filter(snapshot -> snapshot.scope() == AgentAdmissionMetrics.Scope.GLOBAL)
                    .findFirst().orElseThrow().inFlightLimit());
        } finally {
            active.close();
        }
        assertTrue(AgentAdmissionMetrics.snapshots().stream()
                        .filter(snapshot -> snapshot.scope() == AgentAdmissionMetrics.Scope.GLOBAL)
                        .noneMatch(snapshot -> snapshot.inFlightLimit() == 3),
                "a closed redundant replacement must stay unregistered");
    }

    @Test
    void rejectInvalidConfigurationBeforeControllerCreation() {
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayAdmissionConfig(0, 0, 1, 0, 1, 0));
        assertThrows(IllegalArgumentException.class, () -> new AgentGatewayAdmissionConfig(1, 0, 1, 0, 33, 0));
    }

    private static void await(final AgentResourceAdmission controller, final AgentAdmissionMetrics.Scope scope, final int expected) throws InterruptedException {
        for (int i = 0; i < 200; i++) {
            if (controller.snapshot().stream().anyMatch(state -> state.scope() == scope && state.inFlight() >= expected)) {
                return;
            }
            TimeUnit.MILLISECONDS.sleep(10);
        }
        assertTrue(false, "admission permit was not acquired");
    }

    private static void awaitQueued(final AgentResourceAdmission controller, final int expected) throws InterruptedException {
        for (int i = 0; i < 200; i++) {
            if (controller.snapshot().stream().mapToInt(AgentAdmissionMetrics.Snapshot::queued).max().orElse(0) == expected) {
                return;
            }
            TimeUnit.MILLISECONDS.sleep(10);
        }
        assertTrue(false, "admission waiter was not queued");
    }

    private static void awaitQueued(final AgentMcpResourceAdmission.ResourceAdmissionController controller,
                                    final AgentAdmissionMetrics.Scope scope,
                                    final int expected) throws InterruptedException {
        for (int i = 0; i < 200; i++) {
            if (snapshot(controller, scope).queued() == expected) {
                return;
            }
            TimeUnit.MILLISECONDS.sleep(10);
        }
        assertTrue(false, "admission waiter was not queued");
    }

    private static AgentAdmissionMetrics.Snapshot snapshot(final AgentMcpResourceAdmission.ResourceAdmissionController controller,
                                                           final AgentAdmissionMetrics.Scope scope) {
        return controller.snapshot().stream()
                .filter(state -> state.scope() == scope)
                .findFirst()
                .orElseThrow();
    }

    private static void awaitActive(final AgentMcpResourceAdmission.ResourceAdmissionController controller,
                                    final AgentAdmissionMetrics.Scope scope, final int expected) throws InterruptedException {
        for (int i = 0; i < 200; i++) {
            if (controller.snapshot().stream().anyMatch(state -> state.scope() == scope && state.inFlight() >= expected)) {
                return;
            }
            TimeUnit.MILLISECONDS.sleep(10);
        }
        assertTrue(false, "admission permit was not acquired");
    }
}
