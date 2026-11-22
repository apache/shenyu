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

package org.apache.shenyu.register.client.api;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.LongAdder;

/**
 * Bounded, application-facing view of the Java client's registration progress.
 *
 * <p>Status is written only at the completion boundaries the registration flow already
 * passes through — persist success/failure in {@link FailbackRegistryRepository}, retry
 * attempts, retry exhaustion and resource close — so diagnostics add no registration
 * calls, no second retry chain and no per-request work. A known zero of nothing is never
 * fabricated: an identity that has not reached a boundary stays out of the snapshot, and
 * pending/unknown is always distinguishable from {@link RegistrationStatus#REGISTERED}.</p>
 *
 * <p>Retained state is bounded: at most {@value #MAX_ENTRIES} identities are kept (one
 * entry per identity, no history), overflows only bump a counter, and aggregate counters
 * are unbounded primitives. Entries never contain credentials, tokens or raw payloads —
 * the identity is the registration key the failback layer already builds, and failure
 * reasons are truncated to {@value #MAX_REASON_LENGTH} characters.</p>
 */
public final class ClientRegistrationDiagnostics {

    private static final int MAX_ENTRIES = 512;

    private static final int MAX_REASON_LENGTH = 256;

    private static final ClientRegistrationDiagnostics INSTANCE = new ClientRegistrationDiagnostics();

    private final Map<String, RegistrationEntry> entries = new ConcurrentHashMap<>();

    private final LongAdder queuedEvents = new LongAdder();

    private final LongAdder registeredCount = new LongAdder();

    private final LongAdder retryingCount = new LongAdder();

    private final LongAdder exhaustedCount = new LongAdder();

    private final LongAdder offlineCount = new LongAdder();

    private final LongAdder overflowedCount = new LongAdder();

    private ClientRegistrationDiagnostics() {
    }

    /**
     * Gets the shared diagnostics instance.
     *
     * @return the shared instance
     */
    public static ClientRegistrationDiagnostics getInstance() {
        return INSTANCE;
    }

    /**
     * Record that a registration event was handed to the async pipeline (aggregate only:
     * per-event identity is not reliably available before a completion boundary).
     */
    public void recordQueuedEvent() {
        queuedEvents.increment();
    }

    /**
     * Record an acknowledged registration for the identity.
     *
     * @param identity the registration identity
     */
    public void recordRegistered(final String identity) {
        update(identity, RegistrationStatus.REGISTERED, 0, FailureCategory.NONE, null);
        registeredCount.increment();
    }

    /**
     * Record that a persistence attempt failed and the failback layer owns a retry.
     *
     * @param identity the registration identity
     * @param category the bounded failure category
     * @param reason the raw failure message; truncated and kept credential free
     */
    public void recordRetrying(final String identity, final FailureCategory category, final String reason) {
        update(identity, RegistrationStatus.RETRYING, 0, category, reason);
        retryingCount.increment();
    }

    /**
     * Record one retry attempt observed for the identity.
     *
     * @param identity the registration identity
     */
    public void recordAttempt(final String identity) {
        update(identity, RegistrationStatus.RETRYING, 1, FailureCategory.NONE, null);
    }

    /**
     * Record that the failback retry budget was exhausted for the identity.
     *
     * @param identity the registration identity
     */
    public void recordExhausted(final String identity) {
        update(identity, RegistrationStatus.EXHAUSTED, 0, FailureCategory.NONE, "retry budget exhausted");
        exhaustedCount.increment();
    }

    /**
     * Record that a pending registration was removed without an exhaustion signal.
     *
     * @param identity the registration identity
     */
    public void recordRemoved(final String identity) {
        update(identity, RegistrationStatus.UNKNOWN, 0, FailureCategory.NONE, "pending registration removed");
    }

    /**
     * Retire every retained entry as offline (resource close/shutdown). The replacement
     * is conditional so a concurrent fresh failure is never overwritten by this pass.
     */
    public void retireAll() {
        for (Map.Entry<String, RegistrationEntry> entry : entries.entrySet()) {
            RegistrationEntry current = entry.getValue();
            RegistrationEntry retired = new RegistrationEntry(current.identity(), RegistrationStatus.OFFLINE,
                    current.attempts(), FailureCategory.NONE, null, System.currentTimeMillis());
            entries.replace(entry.getKey(), current, retired);
        }
        offlineCount.add(entries.size());
    }

    /**
     * The bounded point-in-time snapshot: retained entries (most recently updated first)
     * plus aggregate counters.
     *
     * @return the snapshot
     */
    public RegistrationSnapshot snapshot() {
        List<RegistrationEntry> list = new ArrayList<>(entries.values());
        list.sort(Comparator.comparingLong(RegistrationEntry::lastUpdatedMillis).reversed());
        return new RegistrationSnapshot(list, queuedEvents.sum(), registeredCount.sum(), retryingCount.sum(),
                exhaustedCount.sum(), offlineCount.sum(), overflowedCount.sum(), System.currentTimeMillis());
    }

    /**
     * Test-only reset of the shared state; production code retires state instead.
     */
    void resetForTest() {
        entries.clear();
        queuedEvents.reset();
        registeredCount.reset();
        retryingCount.reset();
        exhaustedCount.reset();
        offlineCount.reset();
        overflowedCount.reset();
    }

    private void update(final String identity, final RegistrationStatus status, final long attemptDelta,
                        final FailureCategory category, final String rawReason) {
        if (Objects.isNull(identity) || identity.isEmpty()) {
            return;
        }
        boolean known = entries.containsKey(identity);
        if (!known && entries.size() >= MAX_ENTRIES) {
            // the cap is on retained identities, not on observations: counters keep moving
            overflowedCount.increment();
            return;
        }
        long now = System.currentTimeMillis();
        entries.compute(identity, (key, current) -> {
            long attempts = (Objects.isNull(current) ? 0 : current.attempts()) + attemptDelta;
            if (Objects.nonNull(current) && current.status() == RegistrationStatus.OFFLINE
                    && status == RegistrationStatus.RETRYING) {
                // a fresh failure after retirement restarts the identity's lifecycle
                attempts = Math.max(attempts, 1);
            }
            return new RegistrationEntry(key, status, attempts, category, truncate(rawReason), now);
        });
    }

    private static String truncate(final String reason) {
        if (Objects.isNull(reason)) {
            return null;
        }
        return reason.length() <= MAX_REASON_LENGTH ? reason : reason.substring(0, MAX_REASON_LENGTH);
    }

    /**
     * Registration status of one registration identity.
     */
    public enum RegistrationStatus {
        /**
         * The registration work was handed to the async pipeline; no completion boundary
         * has been reached for it yet (aggregate counter only).
         */
        QUEUED,
        /**
         * The registration request completed without error: the Admin acknowledged the
         * payload. This is not gateway-observed readiness — no convergence is inferred
         * from a successful persistence.
         */
        REGISTERED,
        /**
         * A persistence attempt failed and the failback layer owns a retry task.
         */
        RETRYING,
        /**
         * The failback retry budget was exhausted; the identity is no longer pending.
         */
        EXHAUSTED,
        /**
         * The identity was retired (orderly close/shutdown or explicit removal).
         */
        OFFLINE,
        /**
         * The identity no longer has a pending registration for an unclassified reason.
         */
        UNKNOWN
    }

    /**
     * Bounded failure category; never derived from payload content.
     */
    public enum FailureCategory {
        /**
         * No failure recorded.
         */
        NONE,
        /**
         * The registration transport failed (connection, HTTP error, unexpected exception).
         */
        TRANSPORT,
        /**
         * The registration attempt exceeded a timeout.
         */
        TIMEOUT
    }

    /**
     * One registration identity's current observation.
     *
     * @param identity the registration identity (config/namespace/protocol scoped, credential free)
     * @param status the current registration status
     * @param attempts the number of retry attempts observed for this identity
     * @param failureCategory the bounded category of the last failure
     * @param failureReason the truncated, credential-free failure message
     * @param lastUpdatedMillis epoch millis of the last boundary observed for this identity
     */
    public record RegistrationEntry(String identity, RegistrationStatus status, long attempts,
                                    FailureCategory failureCategory, String failureReason, long lastUpdatedMillis) {
    }

    /**
     * Bounded point-in-time view: current entries plus aggregate counters.
     *
     * @param entries the retained per-identity observations, most recently updated first
     * @param queuedEvents the number of registration events handed to the async pipeline
     * @param registeredCount the number of observed registrations that completed with acknowledgement
     * @param retryingCount the number of identities currently observed as retrying
     * @param exhaustedCount the number of identities whose retry budget was exhausted
     * @param offlineCount the number of identities retired by close/shutdown
     * @param overflowedCount the number of identities rejected because the entry cap was reached
     * @param generatedAtMillis epoch millis when the snapshot was taken
     */
    public record RegistrationSnapshot(List<RegistrationEntry> entries, long queuedEvents, long registeredCount,
                                       long retryingCount, long exhaustedCount, long offlineCount,
                                       long overflowedCount, long generatedAtMillis) {
    }
}
