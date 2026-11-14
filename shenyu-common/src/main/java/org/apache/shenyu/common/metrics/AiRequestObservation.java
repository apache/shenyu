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

package org.apache.shenyu.common.metrics;

import org.apache.shenyu.common.dto.AgentRequestContext;
import reactor.core.publisher.SignalType;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Subscription-local owner shared by the gateway, limiter boundary and AI proxy.
 * Only the component that creates the state finishes the logical request. State
 * retains no throwable, response body, credentials or unbounded attempt list.
 */
public final class AiRequestObservation {

    public static final String REACTOR_KEY = AiRequestObservation.class.getName();

    private final AiCallObserver observer;

    private final AgentRuntimeMetrics metrics;

    private final long started = System.nanoTime();

    private AgentRequestContext context;

    private Attempt active;

    private int attempts;

    private AiUsage usage = AiUsage.UNKNOWN;

    private AiStreamCompletion completion = AiStreamCompletion.NOT_INSPECTED;

    private AiCallOutcome failure;

    private boolean completed;

    /**
     * Inferred peer-disconnect evidence written by the response write path. Guarded by the
     * observation monitor like every other mutable field, and cleared when a new attempt starts so
     * a disconnect observed during one attempt never classifies a later attempt.
     */
    private boolean downstreamDisconnected;

    /**
     * The response write path itself terminated with an error. Guarded by the observation monitor.
     */
    private boolean downstreamWriteFailed;

    public AiRequestObservation(final AiCallObserver observer, final AgentRequestContext context) {
        this(observer, context, AgentRuntimeMetrics.NOOP);
    }

    public AiRequestObservation(final AiCallObserver observer, final AgentRequestContext context, final AgentRuntimeMetrics metrics) {
        this.observer = observer;
        this.context = Objects.requireNonNull(context, "context");
        this.metrics = Objects.requireNonNull(metrics, "metrics");
        AgentRuntimeMetrics.safely(() -> metrics.started(AgentRuntimeMetrics.Traffic.LLM));
    }

    public synchronized AgentRequestContext getContext() {
        return context;
    }

    /**
     * Replace immutable facts only within the same logical request.
     * @param value trusted snapshot, never client headers
     */
    public synchronized void updateContext(final AgentRequestContext value) {
        if (!completed && context.getRequestId().equals(value.getRequestId())) {
            context = value;
        }
    }

    /**
     * Classify without retaining the exception or its potentially sensitive message.
     * @param error terminal execution or response-write failure
     */
    public synchronized void error(final Throwable error) {
        if (!completed) {
            Throwable cause = error;
            failure = AiCallOutcome.ERROR;
            for (int depth = 0; depth < 16 && Objects.nonNull(cause); depth++) {
                if (cause instanceof TimeoutException) {
                    failure = AiCallOutcome.TIMEOUT;
                    break;
                }
                cause = cause.getCause();
            }
        }
    }

    /**
     * Supply a layer-specific bounded classification without implementation coupling.
     * @param outcome classification of the logical terminal failure
     */
    public synchronized void failure(final AiCallOutcome outcome) {
        if (!completed) {
            failure = outcome;
        }
    }

    /**
     * Record a real downstream peer disconnect observed while the response body was written.
     *
     * <p>Inferred evidence, deliberately kept apart from the assigned HTTP status and from any
     * classified failure: it only upgrades a request to {@code CANCELLED} when the response had
     * already been committed as successful and nothing failed, so an upstream error or a forwarded
     * 4xx/5xx is never masked by a client that goes away. Ignored once the request is settled, and
     * cleared for every new attempt.</p>
     */
    public synchronized void downstreamDisconnected() {
        if (!completed) {
            downstreamDisconnected = true;
        }
    }

    /**
     * Record that the response write path terminated with an error.
     *
     * <p>Only the classification is retained, never the throwable or its text. A write failure
     * outranks a normal completion, so a partially delivered body is never reported as success.</p>
     */
    public synchronized void downstreamWriteFailed() {
        if (!completed) {
            downstreamWriteFailed = true;
        }
    }

    /**
     * Allocate state exactly when an upstream attempt starts, not when preparing it.
     * @param target trusted context updated with the actual selected target
     * @param kind reason for the attempt
     * @param provider selected provider
     * @param protocol selected client protocol
     * @param streaming actual mode
     * @return isolated attempt state
     */
    public synchronized Attempt startAttempt(final AgentRequestContext target, final AiAttemptEvent.Kind kind,
                                             final String provider, final String protocol, final boolean streaming) {
        if (completed) {
            Attempt ignored = new Attempt(this, context, attempts, kind, provider, protocol, streaming);
            ignored.completed = true;
            return ignored;
        }
        updateContext(target);
        // a new attempt owns a new response write path, so write evidence never carries over
        downstreamDisconnected = false;
        downstreamWriteFailed = false;
        active = new Attempt(this, context, ++attempts, kind, provider, protocol, streaming);
        usage = AiUsage.UNKNOWN;
        completion = AiStreamCompletion.NOT_INSPECTED;
        return active;
    }

    /**
     * Freeze once after the entire owned chain terminates, including recovery writes.
     * @param signal logical terminal signal
     * @param httpStatus assigned downstream status, nullable before commitment
     */
    public void finish(final SignalType signal, final Integer httpStatus) {
        final AiAttemptEvent attemptEvent;
        final AiTerminalEvent terminalEvent;
        synchronized (this) {
            if (completed) {
                return;
            }
            completed = true;
            final AiCallOutcome error = terminalFailure(signal);
            attemptEvent = Objects.isNull(active) ? null : active.freeze(signal, terminalOutcome(active.status, error));
            final AiCallOutcome outcome = interrupted(signal, Objects.nonNull(error), httpStatus)
                    ? AiCallOutcome.CANCELLED : terminalOutcome(httpStatus, error);
            terminalEvent = new AiTerminalEvent(context, outcome, httpStatus, elapsed(started), attempts, usage, completion);
        }
        recordAttempt(attemptEvent);
        AgentRuntimeMetrics.safely(() -> metrics.finished(AgentRuntimeMetrics.Traffic.LLM,
                AgentRuntimeMetrics.Outcome.valueOf(terminalEvent.outcome().name()), reason(terminalEvent.httpStatus()), terminalEvent.elapsedMillis()));
        if (Objects.nonNull(observer)) {
            try {
                observer.record(terminalEvent);
            } catch (RuntimeException ignored) {
                // Observation must never change the request; do not log callback text.
            }
        }
    }

    private void recordAttempt(final AiAttemptEvent event) {
        if (Objects.nonNull(event)) {
            AgentRuntimeMetrics.safely(() -> metrics.attempt(event.kind(), AgentRuntimeMetrics.Outcome.valueOf(event.outcome().name()), event.elapsedMillis()));
        }
        if (Objects.nonNull(observer) && Objects.nonNull(event)) {
            try {
                observer.recordAttempt(event);
            } catch (RuntimeException ignored) {
                // Observation must not change retry, cancellation or response delivery.
            }
        }
    }

    private static AgentRuntimeMetrics.Reason reason(final Integer status) {
        if (Objects.equals(status, 401)) {
            return AgentRuntimeMetrics.Reason.AUTHENTICATION;
        }
        if (Objects.equals(status, 403)) {
            return AgentRuntimeMetrics.Reason.AUTHORIZATION;
        }
        if (Objects.equals(status, 413) || Objects.equals(status, 429)) {
            return AgentRuntimeMetrics.Reason.RESOURCE_LIMIT;
        }
        return AgentRuntimeMetrics.Reason.NONE;
    }

    /**
     * Classify explicit HTTP status without treating a cancelled write as delivered.
     * @param status assigned status
     * @param error bounded failure, or null
     * @return bounded outcome
     */
    public static AiCallOutcome terminalOutcome(final Integer status, final AiCallOutcome error) {
        if (Objects.nonNull(error)) {
            return error;
        }
        if (Objects.equals(status, 504)) {
            return AiCallOutcome.TIMEOUT;
        }
        if (Objects.equals(status, 429)) {
            return AiCallOutcome.RATE_LIMITED;
        }
        if (Objects.nonNull(status) && status >= 400 && status < 500) {
            return AiCallOutcome.REJECTED;
        }
        return Objects.nonNull(status) && status >= 500 ? AiCallOutcome.ERROR : AiCallOutcome.SUCCESS;
    }

    /**
     * Resolve the recorded failure, including evidence only the response write path can see.
     *
     * @param signal logical terminal signal
     * @return classified failure, or null when nothing failed
     */
    private AiCallOutcome terminalFailure(final SignalType signal) {
        if (Objects.nonNull(failure)) {
            return failure;
        }
        if (downstreamWriteFailed || signal == SignalType.ON_ERROR) {
            return AiCallOutcome.ERROR;
        }
        return null;
    }

    /**
     * An explicit cancellation signal always wins, as it does today. A peer disconnect is only
     * inferred evidence, so it may explain nothing but a response that was already committed as
     * successful and that has no classified failure: a real upstream error or a forwarded
     * 4xx/5xx keeps its own outcome and its status.
     *
     * @param signal logical terminal signal
     * @param failure whether any terminal failure was classified
     * @param httpStatus assigned downstream status, nullable before commitment
     * @return whether the request ended because the downstream peer went away
     */
    private boolean interrupted(final SignalType signal, final boolean failure, final Integer httpStatus) {
        if (signal == SignalType.CANCEL) {
            return true;
        }
        return downstreamDisconnected && !failure && committedAsSuccess(httpStatus);
    }

    private static boolean committedAsSuccess(final Integer httpStatus) {
        return Objects.isNull(httpStatus) || (httpStatus >= 200 && httpStatus < 300);
    }

    private static long elapsed(final long start) {
        return TimeUnit.NANOSECONDS.toMillis(Math.max(0, System.nanoTime() - start));
    }

    /**
     * Mutable bounded state for one actual attempt, guarded by its logical owner.
     */
    public static final class Attempt {

        public static final String REACTOR_KEY = Attempt.class.getName();

        private final AiRequestObservation owner;

        private final AgentRequestContext context;

        private final int number;

        private final AiAttemptEvent.Kind kind;

        private final String provider;

        private final String protocol;

        private final boolean streaming;

        private final long started = System.nanoTime();

        private Integer status;

        private AiUsage usage = AiUsage.UNKNOWN;

        private AiStreamCompletion completion = AiStreamCompletion.NOT_INSPECTED;

        private boolean completed;

        private Attempt(final AiRequestObservation owner, final AgentRequestContext context, final int number, final AiAttemptEvent.Kind kind,
                        final String provider, final String protocol, final boolean streaming) {
            this.owner = owner;
            this.context = context;
            this.number = number;
            this.kind = kind;
            this.provider = provider;
            this.protocol = protocol;
            this.streaming = streaming;
        }

        public void status(final int value) {
            synchronized (owner) {
                if (!completed) {
                    status = value;
                }
            }
        }

        public void usage(final AiUsage value) {
            synchronized (owner) {
                if (!completed) {
                    usage = Objects.requireNonNull(value);
                }
            }
        }

        /**
         * Record raw upstream evidence separately from any converter or encoder output.
         * @param evidence bounded evidence
         */
        public void streamEvidence(final AiStreamCompletion.Evidence evidence) {
            synchronized (owner) {
                if (!completed && completion.evidence() != AiStreamCompletion.Evidence.UPSTREAM_MARKER) {
                    completion = new AiStreamCompletion(evidence, completion.framing());
                }
            }
        }

        /**
         * Record the encoder's framing without upgrading raw evidence.
         * @param framing bounded output fact
         */
        public void streamFraming(final AiStreamCompletion.Framing framing) {
            synchronized (owner) {
                if (!completed) {
                    completion = new AiStreamCompletion(completion.evidence(), framing);
                }
            }
        }

        public AiStreamCompletion streamCompletion() {
            synchronized (owner) {
                return completion;
            }
        }

        /**
         * Complete before retry/fallback recovery starts; repeated calls are harmless.
         * @param signal attempt terminal signal
         * @param error classified error or null on normal completion
         */
        public void finish(final SignalType signal, final AiCallOutcome error) {
            final AiAttemptEvent event;
            synchronized (owner) {
                event = freeze(signal, error);
            }
            owner.recordAttempt(event);
        }

        private AiAttemptEvent freeze(final SignalType signal, final AiCallOutcome error) {
            if (completed) {
                return null;
            }
            completed = true;
            owner.usage = usage;
            final AiStreamCompletion.Evidence evidence = completion.evidence();
            // a truncated stream, a malformed partial event or a transport failure is a stronger
            // fact than "the source was cancelled": keep it and keep the matching classification
            final boolean failedStream = evidence == AiStreamCompletion.Evidence.INCOMPLETE
                    || evidence == AiStreamCompletion.Evidence.TRANSPORT_FAILURE;
            final boolean failed = Objects.nonNull(error) && error != AiCallOutcome.SUCCESS;
            final boolean interrupted = !failedStream && owner.interrupted(signal, failed, status);
            if (interrupted && (evidence == AiStreamCompletion.Evidence.PENDING
                    || evidence == AiStreamCompletion.Evidence.UNKNOWN)) {
                completion = new AiStreamCompletion(AiStreamCompletion.Evidence.SOURCE_CANCELLED, completion.framing());
            }
            owner.completion = completion;
            if (owner.active == this) {
                owner.active = null;
            }
            final AiCallOutcome outcome;
            if (failedStream) {
                outcome = failed ? error : AiCallOutcome.ERROR;
            } else if (interrupted) {
                outcome = AiCallOutcome.CANCELLED;
            } else {
                outcome = terminalOutcome(status, error);
            }
            return new AiAttemptEvent(context, number, kind, provider, protocol, streaming, outcome, status, elapsed(started), usage, completion);
        }
    }
}
