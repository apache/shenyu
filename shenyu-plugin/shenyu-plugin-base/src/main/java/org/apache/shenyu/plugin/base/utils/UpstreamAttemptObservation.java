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

package org.apache.shenyu.plugin.base.utils;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Read-only facts of one actual upstream transport attempt, including its response
 * body: attempt identity, the target that was addressed, the method, headers-ready
 * timing/status and the terminal outcome. Observers receive the same live instance;
 * mutable fields are written once by the transport path and are safe to read from
 * any thread afterwards.
 *
 * <p>A subscription of the transport publisher is one attempt — proof that a transport
 * call was admitted, not that bytes reached the backend. The terminal outcome is
 * recorded exactly once per attempt: the response body's completion, error or
 * cancellation finishes the observation of the serving attempt, while a failed attempt
 * finishes its own as soon as it terminates (#8209).
 */
public final class UpstreamAttemptObservation {

    /**
     * Terminal outcomes of an attempt.
     */
    public enum Outcome {

        /**
         * The response body was fully consumed and written downstream.
         */
        COMPLETED,

        /**
         * The attempt terminated with an error before or while the body streamed.
         */
        ERROR,

        /**
         * The attempt was cancelled before its body completed.
         */
        CANCELLED
    }

    /**
     * Exchange attribute naming the observation of the most recent transport attempt
     * of the logical request; the serving attempt's terminal outcome is recorded by
     * the forwarding path when the downstream chain terminates (#8209).
     */
    public static final String CURRENT_ATTRIBUTE = UpstreamAttemptObservation.class.getName();

    private static final AtomicLong ATTEMPT_SEQUENCE = new AtomicLong();

    private final long attemptId = ATTEMPT_SEQUENCE.incrementAndGet();

    private final String selectorId;

    private final String protocol;

    private final String url;

    private final String method;

    private final long startNanos = System.nanoTime();

    private final AtomicBoolean finished = new AtomicBoolean();

    private volatile Integer headersStatus;

    private volatile long headersReadyNanos;

    private volatile Outcome outcome;

    private UpstreamAttemptObservation(final String selectorId, final String protocol,
                                       final String url, final String method) {
        this.selectorId = selectorId;
        this.protocol = protocol;
        this.url = url;
        this.method = method;
    }

    /**
     * Create an observation for an admitted transport attempt.
     *
     * @param selectorId the selector the target belongs to
     * @param protocol the target's protocol
     * @param url the target's address (no query, no credentials)
     * @param method the outbound HTTP method
     * @return the observation
     */
    public static UpstreamAttemptObservation started(final String selectorId, final String protocol,
                                                     final String url, final String method) {
        return new UpstreamAttemptObservation(selectorId, protocol, url, method);
    }

    /**
     * Record the response headers becoming ready. A later error means the body did not
     * complete and must not be counted as a fully successful attempt (#8209).
     *
     * @param status the response status code
     */
    public void markHeadersReady(final Integer status) {
        this.headersStatus = status;
        this.headersReadyNanos = System.nanoTime();
    }

    /**
     * Finish the observation exactly once; later calls are ignored.
     *
     * @param outcome the terminal outcome
     * @return true when this call recorded the terminal outcome
     */
    public boolean finish(final Outcome outcome) {
        if (finished.compareAndSet(false, true)) {
            this.outcome = Objects.requireNonNull(outcome, "outcome");
            return true;
        }
        return false;
    }

    /**
     * Get the attempt identity, unique within this gateway process.
     *
     * @return the attempt id
     */
    public long getAttemptId() {
        return attemptId;
    }

    /**
     * Get the selector the target belongs to.
     *
     * @return the selector id
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Get the target's protocol.
     *
     * @return the protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * Get the target's address. Never carries a query or credentials.
     *
     * @return the address
     */
    public String getUrl() {
        return url;
    }

    /**
     * Get the outbound HTTP method.
     *
     * @return the method
     */
    public String getMethod() {
        return method;
    }

    /**
     * Get the response status observed when headers became ready, or null when the
     * attempt failed before headers arrived.
     *
     * @return the status, or null
     */
    public Integer getHeadersStatus() {
        return headersStatus;
    }

    /**
     * Headers-ready timestamp in nanoseconds.
     *
     * @return the timestamp
     */
    public long getHeadersReadyNanos() {
        return headersReadyNanos;
    }

    /**
     * Attempt start timestamp in nanoseconds.
     *
     * @return the timestamp
     */
    public long getStartNanos() {
        return startNanos;
    }

    /**
     * Get the terminal outcome, or null while the attempt is still in flight.
     *
     * @return the outcome, or null
     */
    public Outcome getOutcome() {
        return outcome;
    }

    /**
     * Whether the observation has reached its terminal outcome.
     *
     * @return true when finished
     */
    public boolean isFinished() {
        return finished.get();
    }
}
