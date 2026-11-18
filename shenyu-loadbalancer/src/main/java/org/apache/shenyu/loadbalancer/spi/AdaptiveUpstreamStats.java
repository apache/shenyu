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

import java.util.Map;
import java.util.Objects;
import java.util.OptionalLong;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Bounded per-upstream feedback state for the adaptive load balancer.
 *
 * <p>Each upstream identity holds one immutable statistics record that is replaced
 * atomically on every feedback. The record carries a smoothed latency in the same
 * EWMA family as the p2c trigger. Feedback classification follows
 * {@link AdaptiveOutcome}: a 4xx is neutral (the observation says nothing about the
 * upstream), while a 5xx or a transport failure enters the smoothing as a synthetic
 * latency sample of at least {@link #FAILURE_PENALTY_MILLIS} — this is what keeps a
 * fast failing node unattractive: finishing quickly can never make its score smaller
 * than the failure floor, and a clean node recovers because subsequent successful
 * samples dominate the smoothing.</p>
 *
 * <p>The map is bounded: once it holds more than {@link #PRUNE_THRESHOLD} entries a
 * selection prunes the identities whose last feedback is older than
 * {@link #IDLE_MILLIS}, so churned upstreams cannot accumulate indefinitely. All
 * timing is passed in by the caller, which keeps the scoring deterministic under
 * test.</p>
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

    private final Map<UpstreamIdentity, Stats> stats = new ConcurrentHashMap<>();

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
        double score = (current.ewmaLatencyMillis() + 1) * (inflight + 1)
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
     * Amortized prune, called from the selection path: only runs a scan once the map
     * outgrew its threshold, and then drops identities without feedback for
     * {@link #IDLE_MILLIS}.
     *
     * @param nowMillis the selection time
     * @return the number of pruned identities
     */
    public int pruneIfNeeded(final long nowMillis) {
        if (stats.size() <= PRUNE_THRESHOLD) {
            return 0;
        }
        int before = stats.size();
        stats.entrySet().removeIf(entry -> nowMillis - entry.getValue().lastFeedbackMillis() > IDLE_MILLIS);
        return before - stats.size();
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
     * One immutable statistics snapshot.
     *
     * @param ewmaLatencyMillis smoothed latency of latency-bearing attempts
     * @param samples number of latency-bearing observations
     * @param lastFeedbackMillis receipt time of the last feedback of any class
     */
    record Stats(long ewmaLatencyMillis, long samples, long lastFeedbackMillis) {

        static Stats first(final long elapsedMillis, final AdaptiveOutcome outcome, final long nowMillis) {
            return new Stats(latencyOf(elapsedMillis, outcome), samplesOf(outcome), nowMillis);
        }

        Stats next(final long elapsedMillis, final AdaptiveOutcome outcome, final long nowMillis) {
            if (outcome == AdaptiveOutcome.CLIENT_ERROR) {
                // neutral: the observation says nothing about the upstream
                return new Stats(ewmaLatencyMillis, samples, nowMillis);
            }
            long latency = latencyOf(elapsedMillis, outcome);
            long updated = samples <= 0L ? latency : smooth(latency, nowMillis - lastFeedbackMillis);
            return new Stats(updated, samples + 1L, nowMillis);
        }

        private long smooth(final long latency, final long sinceLastMillis) {
            double w = Math.exp((double) -Math.max(sinceLastMillis, 0L) / (double) EWMA_WINDOW_MILLIS);
            return (long) ((double) ewmaLatencyMillis * w + (double) latency * (1.0 - w));
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
