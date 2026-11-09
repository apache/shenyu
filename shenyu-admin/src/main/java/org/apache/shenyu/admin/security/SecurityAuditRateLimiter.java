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

package org.apache.shenyu.admin.security;

/**
 * Bounded fixed-window rate limiter for security audit emission. One slot per
 * {@link SecurityAuditEventType} constant is held in fixed-length arrays, so memory stays
 * constant, no map keyed by attacker-controlled values exists, and no expiry cleanup is
 * required.
 *
 * <p>Thread safety: every read and write of the per-slot window state
 * ({@code windowStartMillis}, {@code emittedCount}, {@code suppressedCount}) happens while
 * holding that slot's monitor, so the state triple is always updated as one atomic unit. A
 * window flip (capture the old suppressed total, zero it, advance the window start, account
 * the flipping event) can therefore never interleave with same-window accounting by a
 * racing thread. This guarantees:
 * </p>
 * <ul>
 * <li>every event is accounted to exactly one window — the one whose start is current when
 * the event enters the critical section;</li>
 * <li>the suppressed total reported by a flip contains only events of the window that just
 * closed, never events of the new window;</li>
 * <li>each window admits at most {@code maxPerWindow} events, even under concurrent flips;</li>
 * <li>each window's suppressed total is reported at most once, to the single caller whose
 * call performs the flip.</li>
 * </ul>
 *
 * <p>The critical section is a few array accesses, short enough for a log-throttling path,
 * and slots are independent, so contending on one event type never blocks another.
 * Instances must be safely published; the singleton in {@link SecurityAuditLog} is
 * published through a static field initializer.
 * </p>
 */
public class SecurityAuditRateLimiter {

    /**
     * default window length in milliseconds.
     */
    public static final long DEFAULT_WINDOW_MILLIS = 60_000L;

    /**
     * default maximum number of events of one type emitted per window.
     */
    public static final int DEFAULT_MAX_PER_WINDOW = 120;

    private static final int SLOT_COUNT = SecurityAuditEventType.values().length;

    private final long windowMillis;

    private final int maxPerWindow;

    private final long[] windowStartMillis = new long[SLOT_COUNT];

    private final long[] emittedCount = new long[SLOT_COUNT];

    private final long[] suppressedCount = new long[SLOT_COUNT];

    private final Object[] slotMonitors = new Object[SLOT_COUNT];

    /**
     * create a limiter with the default window length and quota.
     */
    public SecurityAuditRateLimiter() {
        this(DEFAULT_WINDOW_MILLIS, DEFAULT_MAX_PER_WINDOW);
    }

    /**
     * create a limiter.
     *
     * @param windowMillis window length in milliseconds
     * @param maxPerWindow maximum number of events of one type emitted per window
     */
    public SecurityAuditRateLimiter(final long windowMillis, final int maxPerWindow) {
        this.windowMillis = windowMillis;
        this.maxPerWindow = maxPerWindow;
        long now = System.currentTimeMillis();
        for (int i = 0; i < SLOT_COUNT; i++) {
            windowStartMillis[i] = now;
            slotMonitors[i] = new Object();
        }
    }

    /**
     * try to acquire emission quota for an event type at the current time.
     *
     * @param eventType event type
     * @return acquisition result
     */
    public Acquisition tryAcquire(final SecurityAuditEventType eventType) {
        return tryAcquire(eventType, System.currentTimeMillis());
    }

    /**
     * try to acquire emission quota for an event type at the given time. When this call flips the
     * window, the number of events suppressed during the previous window is returned exactly once
     * (to the flipping thread) so the caller can emit a single bounded summary.
     *
     * @param eventType event type
     * @param nowMillis current time in milliseconds
     * @return acquisition result
     */
    public Acquisition tryAcquire(final SecurityAuditEventType eventType, final long nowMillis) {
        final int slot = eventType.ordinal();
        synchronized (slotMonitors[slot]) {
            long flushed = 0L;
            if (nowMillis - windowStartMillis[slot] >= windowMillis) {
                // close the old window and open the new one atomically: the old suppressed
                // total is captured and the counters are reset in the same critical section,
                // so no racing event can be counted against the wrong window and the
                // flipping event is always accounted as the first of the new window.
                flushed = suppressedCount[slot];
                suppressedCount[slot] = 0L;
                emittedCount[slot] = 0L;
                windowStartMillis[slot] = nowMillis;
            }
            if (emittedCount[slot] < maxPerWindow) {
                emittedCount[slot]++;
                return new Acquisition(true, flushed);
            }
            suppressedCount[slot]++;
            return new Acquisition(false, flushed);
        }
    }

    /**
     * result of one quota acquisition.
     */
    public static final class Acquisition {

        private final boolean allowed;

        private final long flushedSuppressed;

        private Acquisition(final boolean allowed, final long flushedSuppressed) {
            this.allowed = allowed;
            this.flushedSuppressed = flushedSuppressed;
        }

        /**
         * whether the event may be emitted.
         *
         * @return true when quota was available
         */
        public boolean isAllowed() {
            return allowed;
        }

        /**
         * number of events of the same type suppressed during the window this call closed,
         * greater than zero only for the single call that flipped the window.
         *
         * @return flushed suppressed count
         */
        public long getFlushedSuppressed() {
            return flushedSuppressed;
        }
    }
}
