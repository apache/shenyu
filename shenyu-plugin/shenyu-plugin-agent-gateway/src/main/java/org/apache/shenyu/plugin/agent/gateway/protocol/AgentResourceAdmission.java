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
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;
import java.util.ArrayDeque;
import java.util.Deque;

/** Deterministic gateway resource admission with bounded deadline-aware queues. */
public final class AgentResourceAdmission implements AutoCloseable {

    private final AgentGatewayAdmissionConfig config;

    private final Scope global;

    private final Scope local;

    private final Deque<Scope> remotes = new ArrayDeque<>();

    private final AgentAdmissionMetrics registration;

    private volatile boolean closed;

    public AgentResourceAdmission(final AgentGatewayAdmissionConfig config) {
        this.config = Objects.requireNonNull(config, "config");
        global = new Scope("global", config.globalInFlight(), config.globalQueued());
        local = new Scope("local", config.localInFlight(), config.localQueued());
        registration = AgentAdmissionMetrics.register(this::snapshot);
    }

    /**
     * Acquire global and LOCAL admission around the single local provider execution.
     * @param deadline absolute request deadline, including queue wait
     * @param action execution after all permits are acquired
 *    @param <T> response type
     * @return one execution result with terminal permit release
     */
    public <T> Mono<T> withLocal(final Instant deadline, final Mono<T> action) {
        return with(global, deadline, with(local, deadline, action));
    }

    /**
     * Acquire global and one stable remote-server scope around a single tool call.
     * @param server configured remote target name
     * @param deadline absolute request deadline, including queue wait
     * @param action execution after all permits are acquired
*    @param <T> response type
     * @return one execution result with terminal permit release
     */
    public <T> Mono<T> withRemote(final String server, final Instant deadline, final Mono<T> action) {
        return with(global, deadline, with(remote(server), deadline, action));
    }

    private <T> Mono<T> with(final Scope scope, final Instant deadline, final Mono<T> action) {
        return Mono.defer(() -> {
            if (closed) {
                scope.rejected.incrementAndGet();
                return Mono.error(AgentAdmissionException.closed());
            }
            AcquireResult acquired = scope.acquire(deadline);
            if (Objects.nonNull(acquired.error)) {
                return Mono.error(acquired.error);
            }
            return acquired.wait.admission()
                    .timeout(Duration.between(Instant.now(), deadline))
                    .onErrorMap(java.util.concurrent.TimeoutException.class,
                            error -> AgentAdmissionException.deadline())
                    .then(action)
                    .doFinally(signal -> scope.release(acquired.wait));
        });
    }

    private Scope remote(final String name) {
        if (Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
            throw new IllegalArgumentException("Invalid remote admission scope");
        }
        for (Scope scope : remotes) {
            if (scope.name.equals(name)) {
                return scope;
            }
        }
        synchronized (remotes) {
            for (Scope scope : remotes) {
                if (scope.name.equals(name)) {
                    return scope;
                }
            }
            Scope scope = new Scope(name, config.remoteInFlight(), config.remoteQueued());
            remotes.add(scope);
            return scope;
        }
    }

    /**
     * Secret-free live states for bounded runtime metrics.
     * @return immutable controller snapshot
     */
    public List<AgentAdmissionMetrics.Snapshot> snapshot() {
        List<AgentAdmissionMetrics.Snapshot> result = new ArrayList<>();
        result.add(global.snapshot(AgentAdmissionMetrics.Scope.GLOBAL));
        result.add(local.snapshot(AgentAdmissionMetrics.Scope.LOCAL));
        remotes.forEach(scope -> result.add(scope.snapshot(AgentAdmissionMetrics.Scope.REMOTE)));
        return result;
    }

    /** Stop new work and cancel queued waiters; terminal executions still release once. */
    public void close() {
        closed = true;
        registration.close();
        global.close();
        local.close();
        remotes.forEach(Scope::close);
    }

    /** Explicit resource-limit rejection, distinguishable from provider failures. */
    public static final class AgentAdmissionException extends RuntimeException {

        private static final long serialVersionUID = 1L;

        private final boolean timeout;

        private final boolean closed;

        private AgentAdmissionException(final boolean timeout, final boolean closed) {
            super(timeout ? "Admission deadline exceeded" : closed ? "Gateway admission closed" : "Gateway resource limit exceeded");
            this.timeout = timeout;
            this.closed = closed;
        }

        static AgentAdmissionException overloaded() {
            return new AgentAdmissionException(false, false);
        }

        static AgentAdmissionException deadline() {
            return new AgentAdmissionException(true, false);
        }

        private static AgentAdmissionException closed() {
            return new AgentAdmissionException(false, true);
        }

        /** Whether the request already exhausted its absolute deadline.
         * @return deadline category
         */
        public boolean isTimeout() {
            return timeout;
        }
    }

    private static final class Wait {

        private final Scope scope;

        private final Waiter waiter;

        private final boolean queued;

        private Wait(final Scope scope, final Waiter waiter) {
            this.scope = scope;
            this.waiter = waiter;
            this.queued = Objects.nonNull(waiter);
        }

        private Mono<Void> admission() {
            return Objects.isNull(waiter) ? Mono.empty() : waiter.ready.asMono();
        }
    }

    private static final class Scope {

        private final String name;

        private final int inFlightLimit;

        private final int queueLimit;

        private final AtomicLong rejected = new AtomicLong();

        private final Object lock = new Object();

        private final Deque<Waiter> waiting = new ArrayDeque<>();

        private int inFlight;

        private int queued;

        private boolean closed;

        private Scope(final String name, final int inFlightLimit, final int queueLimit) {
            this.name = name;
            this.inFlightLimit = inFlightLimit;
            this.queueLimit = queueLimit;
        }

        AcquireResult acquire(final Instant deadline) {
            synchronized (lock) {
                if (closed) {
                    rejected.incrementAndGet();
                    return AcquireResult.reject(AgentAdmissionException.closed());
                }
                if (inFlight < inFlightLimit) {
                    inFlight++;
                    return AcquireResult.admitted(new Wait(this, null));
                }
                if (queued >= queueLimit) {
                    rejected.incrementAndGet();
                    return AcquireResult.reject(AgentAdmissionException.overloaded());
                }
                if (!Instant.now().isBefore(deadline)) {
                    rejected.incrementAndGet();
                    return AcquireResult.reject(AgentAdmissionException.deadline());
                }
                Waiter waiter = new Waiter(deadline);
                waiting.addLast(waiter);
                queued++;
                return AcquireResult.admitted(new Wait(this, waiter));
            }
        }

        void release(final Wait wait) {
            if (Objects.isNull(wait.waiter)) {
                completeActive(wait);
                return;
            }
            boolean promoted;
            synchronized (lock) {
                promoted = wait.waiter.state == State.PROMOTED;
                if (wait.waiter.state == State.WAITING && waiting.remove(wait.waiter)) {
                    queued--;
                    rejected.incrementAndGet();
                }
                wait.waiter.completeExceptionally(AgentAdmissionException.deadline());
                if (promoted) {
                    completeActive(wait);
                }
            }
        }

        private void completeActive(final Wait wait) {
            synchronized (lock) {
                inFlight--;
                promote();
            }
        }

        private void promote() {
            while (inFlight < inFlightLimit) {
                Waiter next = waiting.pollFirst();
                if (Objects.isNull(next)) {
                    return;
                }
                queued--;
                if (!Instant.now().isBefore(next.deadline) || closed) {
                    rejected.incrementAndGet();
                    next.completeExceptionally(AgentAdmissionException.deadline());
                    continue;
                }
                inFlight++;
                next.complete();
                return;
            }
        }


        void close() {
            synchronized (lock) {
                closed = true;
                Waiter wait;
                while (Objects.nonNull(wait = waiting.pollFirst())) {
                    queued--;
                    rejected.incrementAndGet();
                    wait.completeExceptionally(AgentAdmissionException.closed());
                }
            }
        }

        AgentAdmissionMetrics.Snapshot snapshot(final AgentAdmissionMetrics.Scope scope) {
            synchronized (lock) {
                return new AgentAdmissionMetrics.Snapshot(scope, inFlight, queued, inFlightLimit, queueLimit, rejected.get());
            }
        }
    }

    private static final class Waiter {

        private final reactor.core.publisher.Sinks.One<Void> ready = reactor.core.publisher.Sinks.one();

        private final Instant deadline;

        private State state = State.WAITING;

        private Waiter(final Instant deadline) {
            this.deadline = deadline;
        }

        void complete() {
            state = State.PROMOTED;
            ready.tryEmitValue(null);
        }

        void completeExceptionally(final AgentAdmissionException error) {
            state = State.TERMINATED;
            ready.tryEmitError(error);
        }
    }

    private enum State {
        WAITING,
        PROMOTED,
        TERMINATED
    }

    private static final class AcquireResult {

        private final Wait wait;

        private final Throwable error;

        private AcquireResult(final Wait wait, final Throwable error) {
            this.wait = wait;
            this.error = error;
        }

        static AcquireResult admitted(final Wait wait) {
            return new AcquireResult(wait, null);
        }

        static AcquireResult reject(final Throwable error) {
            return new AcquireResult(null, error);
        }
    }
}
