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
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;
import java.util.regex.Pattern;

/**
 * Bounded, application-facing view of the Java client's registration progress.
 *
 * <p>Status is written only at the completion boundaries the registration flow already
 * passes through — persist success/failure and retry attempts in
 * {@link FailbackRegistryRepository}, retry exhaustion, explicit removal and resource
 * close — so diagnostics add no registration calls, no second retry chain and no
 * per-request work. A known zero of nothing is never fabricated: an identity that has not
 * reached a boundary stays out of the snapshot, and pending/unknown is always
 * distinguishable from {@link RegistrationStatus#REGISTERED}.</p>
 *
 * <p>Retained state is bounded: at most {@value #MAX_ENTRIES} identities are kept (one
 * entry per identity, no history). Every write is serialized, so the bound holds under
 * concurrency; when the retained set is full a retired ({@link RegistrationStatus#OFFLINE})
 * entry is evicted first, and only if no retired entry exists is the new identity refused
 * and counted in {@code overflowedEvents}. Active state is never discarded. Aggregate
 * counters are cumulative event counters since process start, not gauges. Entries never
 * contain credentials, tokens or raw payloads — the identity is the registration key the
 * failback layer already builds, and failure reasons are redacted and truncated to
 * {@value #MAX_REASON_LENGTH} characters.</p>
 *
 * <p>The singleton is process-wide, but retirement is scoped: an entry remembers the
 * repository that last observed it, and closing one repository retires only its own
 * entries.</p>
 */
public final class ClientRegistrationDiagnostics {

    private static final int MAX_ENTRIES = 512;

    private static final int MAX_REASON_LENGTH = 256;

    private static final String REDACTED = "***";

    private static final String REASON_EXHAUSTED = "retry budget exhausted";

    private static final String REASON_REMOVED = "pending registration removed";

    private static final Pattern URL_USER_INFO = Pattern.compile("(?<=://)[^/@\\s]+@");

    private static final Pattern QUERY_SECRET = Pattern.compile(
            "(?i)([?&](?:password|passwd|pwd|secret|token|access[_-]?key|credential)[^=&\\s]*=)[^&\\s]*");

    private static final Pattern BEARER_TOKEN = Pattern.compile("(?i)(\\bbearer\\s+)[A-Za-z0-9._~+/=-]+");

    private static final ClientRegistrationDiagnostics INSTANCE = new ClientRegistrationDiagnostics();

    private final Map<String, StoredEntry> entries = new ConcurrentHashMap<>();

    private final Object writeLock = new Object();

    private final AtomicLong updateSequence = new AtomicLong();

    private final LongAdder queuedEvents = new LongAdder();

    private final LongAdder registeredEvents = new LongAdder();

    private final LongAdder retryingEvents = new LongAdder();

    private final LongAdder exhaustedEvents = new LongAdder();

    private final LongAdder offlineEvents = new LongAdder();

    private final LongAdder overflowedEvents = new LongAdder();

    private final LongAdder evictedEvents = new LongAdder();

    private final LongAdder skippedEvents = new LongAdder();

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

    void recordRegistered(final String identity, final Object owner) {
        update(identity, new Observation(RegistrationStatus.REGISTERED, 0, FailureCategory.NONE, null, false), owner);
        registeredEvents.increment();
    }

    void recordRetrying(final String identity, final FailureCategory category, final String reason, final Object owner) {
        update(identity, new Observation(RegistrationStatus.RETRYING, 0, category, reason, false), owner);
        retryingEvents.increment();
    }

    void recordAttempt(final String identity, final Object owner) {
        update(identity, new Observation(RegistrationStatus.ATTEMPTING, 1, null, null, true), owner);
    }

    void recordExhausted(final String identity, final Object owner) {
        update(identity, new Observation(RegistrationStatus.EXHAUSTED, 0, FailureCategory.NONE, REASON_EXHAUSTED, false), owner);
        exhaustedEvents.increment();
    }

    void recordRemoved(final String identity, final Object owner) {
        update(identity, new Observation(RegistrationStatus.UNKNOWN, 0, FailureCategory.NONE, REASON_REMOVED, false), owner);
    }

    /**
     * Record that a persistence call deliberately sent nothing (for example a fenced
     * registration owned by the operator, or a port owned by another process). No entry is
     * written: a skip is not an acknowledgement.
     */
    void recordSkipped() {
        skippedEvents.increment();
    }

    /**
     * Retire the retained entries observed by {@code owner} as offline (resource
     * close/shutdown). The replacement is conditional and already-offline entries are left
     * alone, so only real transitions are counted and a concurrent fresh failure is never
     * overwritten by the retirement pass.
     *
     * @param owner the repository whose entries are retired
     */
    void retireAll(final Object owner) {
        long now = System.currentTimeMillis();
        synchronized (writeLock) {
            int retired = 0;
            for (Map.Entry<String, StoredEntry> entry : entries.entrySet()) {
                StoredEntry stored = entry.getValue();
                RegistrationEntry previous = stored.entry();
                if (stored.owner() != owner || previous.status() == RegistrationStatus.OFFLINE) {
                    continue;
                }
                RegistrationEntry offline = new RegistrationEntry(previous.identity(), RegistrationStatus.OFFLINE,
                        previous.attempts(), FailureCategory.NONE, null, now);
                StoredEntry replacement = new StoredEntry(offline, updateSequence.incrementAndGet(), owner);
                if (entries.replace(entry.getKey(), stored, replacement)) {
                    retired++;
                }
            }
            offlineEvents.add(retired);
        }
    }

    /**
     * The bounded point-in-time snapshot: retained entries (most recently updated first)
     * plus cumulative event counters.
     *
     * @return the snapshot
     */
    public RegistrationSnapshot snapshot() {
        List<StoredEntry> stored = new ArrayList<>(entries.values());
        stored.sort(Comparator.comparingLong(StoredEntry::sequence).reversed());
        List<RegistrationEntry> list = new ArrayList<>(stored.size());
        for (StoredEntry entry : stored) {
            list.add(entry.entry());
        }
        return new RegistrationSnapshot(list, queuedEvents.sum(), registeredEvents.sum(), retryingEvents.sum(),
                exhaustedEvents.sum(), offlineEvents.sum(), overflowedEvents.sum(), evictedEvents.sum(),
                skippedEvents.sum(), System.currentTimeMillis());
    }

    /**
     * Test-only reset of the shared state; production code retires state instead.
     */
    void resetForTest() {
        synchronized (writeLock) {
            entries.clear();
        }
        updateSequence.set(0L);
        queuedEvents.reset();
        registeredEvents.reset();
        retryingEvents.reset();
        exhaustedEvents.reset();
        offlineEvents.reset();
        overflowedEvents.reset();
        evictedEvents.reset();
        skippedEvents.reset();
    }

    private void update(final String identity, final Observation observation, final Object owner) {
        if (Objects.isNull(identity) || identity.isEmpty()) {
            return;
        }
        long now = System.currentTimeMillis();
        synchronized (writeLock) {
            StoredEntry current = entries.get(identity);
            if (Objects.isNull(current) && entries.size() >= MAX_ENTRIES) {
                evictRetired();
                if (entries.size() >= MAX_ENTRIES) {
                    overflowedEvents.increment();
                    return;
                }
            }
            RegistrationEntry previous = Objects.isNull(current) ? null : current.entry();
            RegistrationEntry updated = observation.apply(identity, previous, now);
            entries.put(identity, new StoredEntry(updated, updateSequence.incrementAndGet(), owner));
        }
    }

    private void evictRetired() {
        String victimKey = null;
        StoredEntry victim = null;
        for (Map.Entry<String, StoredEntry> entry : entries.entrySet()) {
            StoredEntry stored = entry.getValue();
            if (stored.entry().status() != RegistrationStatus.OFFLINE) {
                continue;
            }
            if (Objects.isNull(victim) || stored.sequence() < victim.sequence()) {
                victim = stored;
                victimKey = entry.getKey();
            }
        }
        if (Objects.nonNull(victim) && entries.remove(victimKey, victim)) {
            evictedEvents.increment();
        }
    }

    private static String sanitize(final String reason) {
        if (Objects.isNull(reason)) {
            return null;
        }
        String sanitized = URL_USER_INFO.matcher(reason).replaceAll(REDACTED + "@");
        sanitized = QUERY_SECRET.matcher(sanitized).replaceAll("$1" + REDACTED);
        return BEARER_TOKEN.matcher(sanitized).replaceAll("$1" + REDACTED);
    }

    private static String exportReason(final String reason) {
        if (Objects.isNull(reason)) {
            return null;
        }
        String sanitized = sanitize(reason);
        return sanitized.length() <= MAX_REASON_LENGTH ? sanitized : sanitized.substring(0, MAX_REASON_LENGTH);
    }

    /**
     * Registration status of one registration identity.
     */
    public enum RegistrationStatus {
        /**
         * A retry attempt is in flight; the last bounded failure category and reason are
         * preserved until the attempt completes.
         */
        ATTEMPTING,
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
     * @param failureReason the redacted, truncated failure message
     * @param lastUpdatedMillis epoch millis of the last boundary observed for this identity
     */
    public record RegistrationEntry(String identity, RegistrationStatus status, long attempts,
                                    FailureCategory failureCategory, String failureReason, long lastUpdatedMillis) {
    }

    /**
     * Bounded point-in-time view: current entries plus cumulative event counters.
     *
     * @param entries the retained per-identity observations, most recently updated first
     * @param queuedEvents the number of registration events handed to the async pipeline
     * @param registeredEvents the number of acknowledged persist completions observed
     * @param retryingEvents the number of failed persist/retry observations
     * @param exhaustedEvents the number of retry-budget exhaustions observed
     * @param offlineEvents the number of identities this process retired on close/removal
     * @param overflowedEvents the number of new identities refused because the retained set was full
     * @param evictedEvents the number of retired entries evicted to admit a new identity
     * @param skippedEvents the number of persist calls that deliberately sent nothing
     * @param generatedAtMillis epoch millis when the snapshot was taken
     */
    public record RegistrationSnapshot(List<RegistrationEntry> entries, long queuedEvents, long registeredEvents,
                                       long retryingEvents, long exhaustedEvents, long offlineEvents,
                                       long overflowedEvents, long evictedEvents, long skippedEvents,
                                       long generatedAtMillis) {
    }

    private record StoredEntry(RegistrationEntry entry, long sequence, Object owner) {
    }

    private record Observation(RegistrationStatus status, long attemptDelta, FailureCategory category,
                               String reason, boolean keepFailure) {

        private RegistrationEntry apply(final String identity, final RegistrationEntry previous, final long now) {
            long attempts = (Objects.isNull(previous) ? 0 : previous.attempts()) + attemptDelta;
            if (Objects.nonNull(previous) && previous.status() == RegistrationStatus.OFFLINE
                    && status == RegistrationStatus.RETRYING) {
                // a fresh failure after retirement restarts the identity's lifecycle
                attempts = Math.max(attempts, 1);
            }
            if (keepFailure && Objects.nonNull(previous)) {
                return new RegistrationEntry(identity, status, attempts, previous.failureCategory(),
                        previous.failureReason(), now);
            }
            return new RegistrationEntry(identity, status, attempts, category, exportReason(reason), now);
        }
    }
}