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

package org.apache.shenyu.loadbalancer.spi;

import org.apache.shenyu.loadbalancer.entity.UpstreamIdentity;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.OptionalLong;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Bounded per-upstream feedback state for the adaptive load balancer.
 *
 * <p>Each upstream identity holds one immutable statistics record that is replaced
 * atomically on every feedback. The record carries a smoothed latency in the same
 * EWMA family as the p2c trigger. The smoothing keeps double precision internally
 * (in milliseconds): truncating every update to whole milliseconds would discard
 * sub-millisecond increments at short feedback intervals and could pin the estimate
 * at its old value forever. Rounding happens only where a long leaves the record,
 * namely {@link Stats#ewmaLatencyMillis()} and the ceiling of {@link #score}. Feedback
 * classification follows
 * {@link AdaptiveOutcome}: a 4xx is neutral (the observation says nothing about the
 * upstream), while a 5xx or a transport failure enters the smoothing as a synthetic
 * latency sample of at least {@link #FAILURE_PENALTY_MILLIS} — this is what keeps a
 * fast failing node unattractive: finishing quickly can never make its score smaller
 * than the failure floor, and a clean node recovers because subsequent successful
 * samples dominate the smoothing. Neutrality also covers timing: a 4xx refreshes the
 * activity timestamp used for retention, but never the effective-sample timestamp
 * that drives the decay of the next latency-bearing observation.</p>
 *
 * <p>The map is bounded by amortized maintenance: once it holds more than
 * {@link #PRUNE_THRESHOLD} entries, selections drive a prune that drops identities
 * without feedback for {@link #IDLE_MILLIS}. Maintenance runs at most once per
 * {@link #MAINTENANCE_INTERVAL_MILLIS} and inspects at most
 * {@link #MAINTENANCE_SCAN_BUDGET} entries per run, so a hot set above the threshold
 * never pays a whole-map sweep per selection. Once triggered, maintenance keeps
 * running across windows until a full scan cycle completes without pruning anything,
 * so expired entries are drained even after the map shrinks below the threshold.
 * Above the hard capacity
 * {@link #MAX_TRACKED_IDENTITIES} a full idle sweep runs instead, and any remaining
 * excess is evicted oldest-activity-first: statistics are only a scoring cache, so
 * eviction merely returns a node to the cold-start comparison, while in-flight
 * reservations live on the upstream itself and are never touched. All timing is
 * passed in by the caller, which keeps the scoring deterministic under test.</p>
 */
public class AdaptiveUpstreamStats {

    /**
     * Latency observations required before the adaptive score of an upstream is
     * trusted; below this the caller falls back to the p2c load comparison.
     */
    public static final int MIN_SAMPLES = 5;

    /**
     * EWMA time-constant in millis, the same family as the p2c response trigger.
     */
    public static final long EWMA_WINDOW_MILLIS = 600L;

    /**
     * Minimum decay interval in millis applied to same-timestamp observations: a
     * zero gap would give the new sample zero weight and discard it permanently,
     * so concurrent completions are blended with at least this much evidence.
     */
    public static final long MIN_DECAY_INTERVAL_MILLIS = 1L;

    /**
     * Synthetic latency floor for a failed attempt: a 5xx or transport failure costs
     * the caller at least a retry, so it is smoothed in with this latency even when
     * the upstream answered (or died) within a millisecond.
     */
    public static final long FAILURE_PENALTY_MILLIS = 1000L;

    /**
     * Feedback age after which an untouched identity becomes a prune candidate.
     */
    public static final long IDLE_MILLIS = 10 * 60 * 1000L;

    /**
     * Map size that triggers an amortized prune on the next selection.
     */
    public static final int PRUNE_THRESHOLD = 256;

    /**
     * Minimum gap between two maintenance runs; selections in between pay only the
     * size check, so a hot set above the threshold is not swept on every selection.
     */
    public static final long MAINTENANCE_INTERVAL_MILLIS = 10_000L;

    /**
     * Maximum number of entries inspected by one incremental maintenance run; the
     * scan cursor resumes where the previous run stopped.
     */
    public static final int MAINTENANCE_SCAN_BUDGET = 64;

    /**
     * Hard capacity of the registry. Beyond it a full idle sweep runs regardless of
     * the maintenance interval, and any remaining excess is evicted
     * oldest-activity-first so retained state stays bounded under discovery churn.
     */
    public static final int MAX_TRACKED_IDENTITIES = 4096;

    private final Map<UpstreamIdentity, Stats> stats = new ConcurrentHashMap<>();

    private volatile boolean maintenancePending;

    private long lastMaintenanceMillis = Long.MIN_VALUE;

    private Iterator<Map.Entry<UpstreamIdentity, Stats>> scanCursor;

    private int cyclePruned;

    private int lastScanCount;

    /**
     * Record one classified attempt. The elapsed time is the caller's own
     * per-attempt measurement, so overlapping attempts never share a timestamp.
     *
     * @param identity the upstream identity that served the attempt
     * @param elapsedMillis the measured attempt latency
     * @param outcome the classified outcome
     * @param nowMillis the feedback receipt time
     */
    public void record(final UpstreamIdentity identity, final long elapsedMillis, final AdaptiveOutcome outcome,
                       final long nowMillis) {
        stats.compute(identity, (key, existing) -> {
            if (Objects.isNull(existing)) {
                return Stats.first(elapsedMillis, outcome, nowMillis);
            }
            return existing.next(elapsedMillis, outcome, nowMillis);
        });
    }

    /**
     * The adaptive score of one upstream, or {@code empty} when its statistics are
     * still cold: the caller must then compare the candidates with the p2c load
     * formula instead of mixing the two scales.
     *
     * @param identity the upstream identity
     * @param inflight the current in-flight attempt count
     * @param effectiveWeight the warm-up adjusted configured weight
     * @return the score, smaller is better, or empty when cold
     */
    public OptionalLong score(final UpstreamIdentity identity, final long inflight,
                              final int effectiveWeight) {
        Stats current = stats.get(identity);
        if (Objects.isNull(current) || current.samples() < MIN_SAMPLES) {
            return OptionalLong.empty();
        }
        double score = (current.ewmaLatency() + 1) * (inflight + 1)
                / (double) Math.max(effectiveWeight, 1);
        return OptionalLong.of((long) Math.ceil(score));
    }

    /**
     * Whether both candidates carry enough samples for an adaptive comparison.
     *
     * @param first the first identity
     * @param second the second identity
     * @return true when neither is cold
     */
    public boolean bothAdaptive(final UpstreamIdentity first, final UpstreamIdentity second) {
        Stats firstStats = stats.get(first);
        Stats secondStats = stats.get(second);
        return Objects.nonNull(firstStats) && firstStats.samples() >= MIN_SAMPLES
                && Objects.nonNull(secondStats) && secondStats.samples() >= MIN_SAMPLES;
    }

    /**
     * Amortized prune, called from the selection path (both the singleton fast path
     * and the multi-candidate comparison): only runs once the map outgrew its
     * threshold, then drops identities without feedback for {@link #IDLE_MILLIS}
     * under the documented work and frequency bounds.
     *
     * @param nowMillis the selection time
     * @return the number of pruned identities
     */
    public int pruneIfNeeded(final long nowMillis) {
        if (stats.size() <= PRUNE_THRESHOLD && !maintenancePending) {
            return 0;
        }
        return maintain(nowMillis);
    }

    private synchronized int maintain(final long nowMillis) {
        int before = stats.size();
        if (before > PRUNE_THRESHOLD) {
            maintenancePending = true;
        }
        if (!maintenancePending) {
            return 0;
        }
        lastScanCount = 0;
        if (before > MAX_TRACKED_IDENTITIES) {
            sweepIdle(nowMillis);
            evictOldestBeyondCapacity();
            maintenancePending = stats.size() > PRUNE_THRESHOLD;
            lastMaintenanceMillis = nowMillis;
        } else if (maintenanceDue(nowMillis)) {
            boolean cycleCompleted = scanBudgeted(nowMillis);
            if (cycleCompleted) {
                // one full inspection cycle is done; stay armed while the map still
                // outgrows the threshold or this cycle pruned something (entries may
                // have expired after their inspection), otherwise the drain is done
                maintenancePending = stats.size() > PRUNE_THRESHOLD || cyclePruned > 0;
            }
            // only an actual run advances the gate; a skipped call must not push the
            // next eligible maintenance further into the future
            lastMaintenanceMillis = nowMillis;
        }
        return before - stats.size();
    }

    private boolean maintenanceDue(final long nowMillis) {
        // the sentinel forces the first run; a backward clock also re-arms the gate
        // instead of overflowing the subtraction into a negative gap
        return lastMaintenanceMillis == Long.MIN_VALUE || nowMillis < lastMaintenanceMillis
                || nowMillis - lastMaintenanceMillis >= MAINTENANCE_INTERVAL_MILLIS;
    }

    private boolean scanBudgeted(final long nowMillis) {
        if (Objects.isNull(scanCursor) || !scanCursor.hasNext()) {
            scanCursor = stats.entrySet().iterator();
            cyclePruned = 0;
        }
        int inspected = 0;
        while (inspected < MAINTENANCE_SCAN_BUDGET && scanCursor.hasNext()) {
            Map.Entry<UpstreamIdentity, Stats> entry = scanCursor.next();
            inspected++;
            Stats observed = entry.getValue();
            if (nowMillis - observed.lastFeedbackMillis() > IDLE_MILLIS) {
                // conditional remove: a feedback recorded since the inspection keeps
                // its fresh record instead of being dropped by this maintenance
                if (stats.remove(entry.getKey(), observed)) {
                    cyclePruned++;
                }
            }
        }
        lastScanCount = inspected;
        return !scanCursor.hasNext();
    }

    private void sweepIdle(final long nowMillis) {
        stats.entrySet().removeIf(entry -> nowMillis - entry.getValue().lastFeedbackMillis() > IDLE_MILLIS);
        lastScanCount = stats.size();
        scanCursor = null;
    }

    private void evictOldestBeyondCapacity() {
        int excess = stats.size() - MAX_TRACKED_IDENTITIES;
        if (excess <= 0) {
            return;
        }
        List<Map.Entry<UpstreamIdentity, Stats>> entries = new ArrayList<>(stats.entrySet());
        entries.sort(Comparator.comparingLong(entry -> entry.getValue().lastFeedbackMillis()));
        for (int i = 0; i < excess && i < entries.size(); i++) {
            Map.Entry<UpstreamIdentity, Stats> entry = entries.get(i);
            stats.remove(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Number of tracked identities; visible for tests.
     *
     * @return the map size
     */
    int size() {
        return stats.size();
    }

    /**
     * Stats of one identity; visible for tests.
     *
     * @param identity the upstream identity
     * @return the current record, or null when cold
     */
    Stats statsOf(final UpstreamIdentity identity) {
        return stats.get(identity);
    }

    /**
     * Entries inspected by the last maintenance run; visible for tests asserting the
     * per-run work bound.
     *
     * @return the inspected entry count of the last run
     */
    int lastScanCount() {
        return lastScanCount;
    }

    /**
     * One immutable statistics snapshot.
     *
     * @param ewmaLatency smoothed latency of latency-bearing attempts, kept as a
     *                    double in millis so sub-millisecond increments survive
     *                    short feedback intervals; always finite and non-negative
     * @param samples number of latency-bearing observations
     * @param lastFeedbackMillis receipt time of the last feedback of any class;
     *                           activity bookkeeping for retention and pruning only
     * @param lastSampleMillis receipt time of the last latency-bearing observation;
     *                         the decay of the next sample is measured from here, so
     *                         a neutral 4xx cannot reshape the next latency update
     */
    record Stats(double ewmaLatency, long samples, long lastFeedbackMillis, long lastSampleMillis) {

        static Stats first(final long elapsedMillis, final AdaptiveOutcome outcome, final long nowMillis) {
            return new Stats(latencyOf(elapsedMillis, outcome), samplesOf(outcome), nowMillis, nowMillis);
        }

        /**
         * The smoothed latency rounded to whole millis; the internal double keeps
         * the unrounded estimate for the next update and for scoring.
         *
         * @return the smoothed latency in millis, rounded to the nearest long
         */
        long ewmaLatencyMillis() {
            return Math.round(ewmaLatency);
        }

        Stats next(final long elapsedMillis, final AdaptiveOutcome outcome, final long nowMillis) {
            if (outcome == AdaptiveOutcome.CLIENT_ERROR) {
                // neutral: the observation says nothing about the upstream; only the
                // activity timestamp moves, the effective-sample timing stays intact
                return new Stats(ewmaLatency, samples, nowMillis, lastSampleMillis);
            }
            long latency = latencyOf(elapsedMillis, outcome);
            double updated = samples <= 0L ? latency : smooth(latency, nowMillis - lastSampleMillis);
            return new Stats(updated, samples + 1L, nowMillis, nowMillis);
        }

        private double smooth(final long latency, final long sinceLastMillis) {
            long interval = Math.max(sinceLastMillis, MIN_DECAY_INTERVAL_MILLIS);
            double w = Math.exp((double) -interval / (double) EWMA_WINDOW_MILLIS);
            return ewmaLatency * w + (double) latency * (1.0 - w);
        }

        private static long latencyOf(final long elapsedMillis, final AdaptiveOutcome outcome) {
            long measured = Math.max(elapsedMillis, 0L);
            if (outcome == AdaptiveOutcome.SERVER_ERROR || outcome == AdaptiveOutcome.TRANSPORT_ERROR) {
                return Math.max(measured, FAILURE_PENALTY_MILLIS);
            }
            return measured;
        }

        private static long samplesOf(final AdaptiveOutcome outcome) {
            return outcome == AdaptiveOutcome.CLIENT_ERROR ? 0L : 1L;
        }
    }
}
