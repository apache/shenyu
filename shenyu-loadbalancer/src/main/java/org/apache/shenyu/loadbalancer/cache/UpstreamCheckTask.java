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

package org.apache.shenyu.loadbalancer.cache;

import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * Health check manager for upstream servers.
 */
public final class UpstreamCheckTask implements Runnable {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckTask.class);

    private final Map<String, List<Upstream>> healthyUpstream = Maps.newConcurrentMap();

    private final Map<String, List<Upstream>> unhealthyUpstream = Maps.newConcurrentMap();

    private final Object lock = new Object();

    private final AtomicBoolean checkStarted = new AtomicBoolean(false);

    /**
     * Probes of the round in flight. Only the round thread writes here; the list is copy-on-write
     * so a concurrent reader can observe submitted probes without taking the configuration lock.
     */
    private final List<CompletableFuture<ProbeResult>> futures = new CopyOnWriteArrayList<>();

    private final int checkInterval;

    private ExecutorService executor;

    private int poolSize;

    private int checkTimeout = 3000;

    private int healthyThreshold = 1;

    private int unhealthyThreshold = 1;

    private volatile long lastRoundDurationMillis;

    private volatile int lastRoundSubmittedChecks;

    private volatile int lastRoundPendingChecks;

    private volatile long lastRoundMaxEndpointLatencyMillis;

    private volatile long lastRoundOldestHealthStateMillis;

    private volatile long lastRoundCompletedAtMillis;

    private final AtomicLong totalRounds = new AtomicLong();

    private final AtomicLong totalSubmittedChecks = new AtomicLong();

    private final AtomicLong totalEndpointLatencyMillis = new AtomicLong();
    
    /**
     * Instantiates a new Upstream check task.
     *
     * @param checkInterval the check interval
     */
    public UpstreamCheckTask(final int checkInterval) {
        this.checkInterval = checkInterval;
    }
    
    /**
     * get checkStarted.
     *
     * @return checkStarted check started
     */
    public AtomicBoolean getCheckStarted() {
        return checkStarted;
    }
    
    /**
     * Schedule health check task.
     */
    public void schedule() {
        // executor for health check
        ThreadFactory healthCheckFactory = ShenyuThreadFactory.create("upstream-health-check", true);
        new ScheduledThreadPoolExecutor(1, healthCheckFactory)
                .scheduleWithFixedDelay(this, 3000, checkInterval, TimeUnit.MILLISECONDS);

        // executor for async request, avoid request block health check thread
        ThreadFactory requestFactory = ShenyuThreadFactory.create("upstream-health-check-request", true);
        executor = new ScheduledThreadPoolExecutor(poolSize, requestFactory);
    }
    
    /**
     * Set check timeout.
     *
     * @param checkTimeout milliseconds
     */
    public void setCheckTimeout(final int checkTimeout) {
        this.checkTimeout = checkTimeout;
    }

    /**
     * get checkThreadPoolSize.
     *
     * @return checkThreadPoolSize
     */
    public int getPoolSize() {
        return poolSize;
    }

    /**
     * set checkThreadPoolSize.
     *
     * @param poolSize checkThreadPoolSize
     */
    public void setPoolSize(final int poolSize) {
        this.poolSize = poolSize;
    }

    /**
     * Set healthy threshold.
     *
     * @param healthyThreshold healthy threshold
     */
    public void setHealthyThreshold(final int healthyThreshold) {
        this.healthyThreshold = healthyThreshold;
    }
    
    /**
     * Set unhealthy threshold.
     *
     * @param unhealthyThreshold unhealthy threshold
     */
    public void setUnhealthyThreshold(final int unhealthyThreshold) {
        this.unhealthyThreshold = unhealthyThreshold;
    }

    @Override
    public void run() {
        healthCheck();
    }

    private void healthCheck() {
        final long startNanos = System.nanoTime();
        // Per-round scratch counters. maxEndpointLatencyMillis is mutated by the async probe
        // tasks running on the executor, so it stays an AtomicLong captured by the lambdas.
        final AtomicLong maxEndpointLatencyMillis = new AtomicLong();
        int submitted = 0;
        int pendingAtWait = 0;
        try {
            if (tryStartHealthCheck()) {
                try {
                    submitted = doHealthCheck(maxEndpointLatencyMillis);
                    pendingAtWait = countPendingFutures();
                    /*
                     * The probe wait deliberately runs without the lock. Holding it across network
                     * probes made every configuration update, for any selector, wait for a whole
                     * round to finish (#8139). Stale results are fenced instead: each probe carries
                     * the selector version it was submitted under, and applyResults() drops the
                     * results of a selector whose configuration changed in the meantime, so a
                     * removal, draining or replacement stays authoritative.
                     */
                    applyResults(waitFinish());
                } finally {
                    // A round aborted between submit and wait (for example an executor rejection)
                    // must not leak its futures into the next round, where they would be awaited
                    // and applied a second time.
                    futures.clear();
                    finishHealthCheck();
                }
            }
        } catch (Exception e) {
            LOG.error("[Health Check] Meet problem: ", e);
        } finally {
            recordRoundMetrics(startNanos, submitted, pendingAtWait, maxEndpointLatencyMillis.get());
        }
    }

    private int countPendingFutures() {
        int pending = 0;
        for (CompletableFuture<ProbeResult> future : futures) {
            if (!future.isDone()) {
                pending++;
            }
        }
        return pending;
    }

    private void recordRoundMetrics(final long startNanos, final int submitted, final int pendingAtWait,
                                    final long maxEndpointLatencyMillis) {
        final long durationMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
        final long oldestHealthStateMillis = computeOldestHealthStateMillis();
        final long completedAtMillis = System.currentTimeMillis();
        this.lastRoundDurationMillis = durationMillis;
        this.lastRoundSubmittedChecks = submitted;
        this.lastRoundPendingChecks = pendingAtWait;
        this.lastRoundMaxEndpointLatencyMillis = maxEndpointLatencyMillis;
        this.lastRoundOldestHealthStateMillis = oldestHealthStateMillis;
        this.lastRoundCompletedAtMillis = completedAtMillis;
        totalRounds.incrementAndGet();
        totalSubmittedChecks.addAndGet(submitted);
        if (LOG.isDebugEnabled()) {
            LOG.debug("[Health Check] round summary: durationMs={}, submitted={}, pendingAtWait={}, "
                            + "maxEndpointLatencyMs={}, oldestHealthStateMs={}, healthySelectors={}, unhealthySelectors={}",
                    durationMillis, submitted, pendingAtWait, maxEndpointLatencyMillis, oldestHealthStateMillis,
                    healthyUpstream.size(), unhealthyUpstream.size());
        }
    }

    private long computeOldestHealthStateMillis() {
        final long now = System.currentTimeMillis();
        long oldest = 0L;
        for (List<Upstream> bucket : healthyUpstream.values()) {
            for (Upstream upstream : bucket) {
                // Skip never-measured upstreams: lastHealthTimestamp defaults to 0 for entries that
                // were registered but not probed yet, or whose health check is disabled while already
                // healthy. Without this guard now - 0 would report an epoch-sized (since 1970) age and
                // permanently dominate the metric.
                final long timestamp = upstream.getLastHealthTimestamp();
                if (timestamp > 0) {
                    final long age = now - timestamp;
                    if (age > oldest) {
                        oldest = age;
                    }
                }
            }
        }
        for (List<Upstream> bucket : unhealthyUpstream.values()) {
            for (Upstream upstream : bucket) {
                final long timestamp = upstream.getLastUnhealthyTimestamp();
                if (timestamp > 0) {
                    final long age = now - timestamp;
                    if (age > oldest) {
                        oldest = age;
                    }
                }
            }
        }
        return oldest;
    }

    private int doHealthCheck(final AtomicLong maxEndpointLatencyMillis) {
        int submitted = 0;
        for (SelectorSnapshot snapshot : captureSnapshot()) {
            for (Upstream upstream : snapshot.upstreams) {
                futures.add(submitCheck(snapshot.selectorId, snapshot.fingerprint, upstream, maxEndpointLatencyMillis));
                submitted++;
            }
        }
        return submitted;
    }

    /**
     * Capture the selectors and endpoints to probe, together with the configuration fingerprint of
     * each selector. The pools and the fingerprints are read under the lock, so a round never mixes
     * a probe list with a fingerprint that does not describe it.
     *
     * @return one entry per selector and pool, holding a copy-on-write view of its endpoints
     */
    private List<SelectorSnapshot> captureSnapshot() {
        synchronized (lock) {
            final List<SelectorSnapshot> snapshot = new ArrayList<>(healthyUpstream.size() + unhealthyUpstream.size());
            appendSnapshot(snapshot, healthyUpstream);
            appendSnapshot(snapshot, unhealthyUpstream);
            return snapshot;
        }
    }

    private void appendSnapshot(final List<SelectorSnapshot> snapshot, final Map<String, List<Upstream>> map) {
        for (Map.Entry<String, List<Upstream>> entry : map.entrySet()) {
            snapshot.add(new SelectorSnapshot(entry.getKey(), fingerprintOf(entry.getKey()), entry.getValue()));
        }
    }

    /**
     * Fingerprint of everything a probe result depends on for one selector: the endpoints in both
     * pools, their configuration, and which instance each pool holds.
     *
     * <p>It is derived from the state rather than maintained by the writers of that state, so no
     * configuration path can bypass it. A reconciliation that mutates a cached upstream in place
     * (weight, {@code healthCheckEnabled}, metadata) changes the fingerprint exactly like a pool
     * membership change does, while re-applying an unchanged snapshot does not, so periodic
     * discovery syncs never starve the health results of a selector.
     *
     * <p>Health state is deliberately excluded: applying a probe result must not change the
     * fingerprint, or the results of one round would invalidate each other. The round reads it
     * under the lock so the value is coherent with a concurrent configuration update.
     *
     * @param selectorId the selector id
     * @return the configuration fingerprint
     */
    long fingerprintOf(final String selectorId) {
        long fingerprint = 17L;
        fingerprint = 31 * fingerprint + poolFingerprint(healthyUpstream.get(selectorId));
        fingerprint = 31 * fingerprint + poolFingerprint(unhealthyUpstream.get(selectorId));
        return fingerprint;
    }

    private long poolFingerprint(final List<Upstream> pool) {
        long fingerprint = 1L;
        if (Objects.isNull(pool)) {
            return fingerprint;
        }
        for (Upstream upstream : pool) {
            // the instance is part of the fingerprint: a replacement must fence a probe of the
            // instance it replaced, even when both carry the same configuration
            fingerprint = 31 * fingerprint + System.identityHashCode(upstream);
            // Upstream.hashCode() is the hash of its logical identity
            fingerprint = 31 * fingerprint + upstream.hashCode();
            fingerprint = 31 * fingerprint + (upstream.isHealthCheckEnabled() ? 1 : 0);
            fingerprint = 31 * fingerprint + upstream.getWeight();
            fingerprint = 31 * fingerprint + (upstream.isStatus() ? 1 : 0);
            fingerprint = 31 * fingerprint + (upstream.isGray() ? 1 : 0);
            fingerprint = 31 * fingerprint + (upstream.isDraining() ? 1 : 0);
            fingerprint = 31 * fingerprint + Objects.hashCode(upstream.getMetadata());
            fingerprint = 31 * fingerprint + Objects.hashCode(upstream.getLabels());
        }
        return fingerprint;
    }

    /**
     * Probe one endpoint. A probe only measures reachability: it never writes health state, because
     * the state transition has to happen under the lock and only while the selector's configuration
     * is still the one the probe was submitted for (#8139).
     *
     * @param selectorId selectorId
     * @param fingerprint configuration fingerprint the probe was submitted under
     * @param upstream upstream to probe
     * @param maxEndpointLatencyMillis per-round latency accumulator
     * @return the probe result
     */
    private ProbeResult probe(final String selectorId, final long fingerprint, final Upstream upstream,
                              final AtomicLong maxEndpointLatencyMillis) {
        final long probeStart = System.nanoTime();
        boolean pass;
        try {
            pass = UpstreamCheckUtils.checkUrl(upstream.getUrl(), checkTimeout);
        } catch (RuntimeException e) {
            // An unparsable upstream URL throws from the probe instead of returning false. Treat it
            // as a failed probe so this endpoint can still leave the healthy pool; otherwise it would
            // keep being selected while never producing a result.
            LOG.error("[Health Check] Selector [{}] upstream {} could not be probed.",
                    selectorId, upstream.getUrl(), e);
            pass = false;
        } finally {
            final long latencyMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - probeStart);
            maxEndpointLatencyMillis.accumulateAndGet(latencyMillis, Math::max);
            totalEndpointLatencyMillis.addAndGet(latencyMillis);
        }
        return new ProbeResult(selectorId, fingerprint, upstream, pass, true);
    }

    /**
     * Submit one probe so that its failure stays contained in its own future.
     *
     * <p>An exceptional probe completes with a {@code null} result instead of an exception, so the
     * round can still apply every other probe result and can always clear its bookkeeping.
     * Ordinary unsuccessful probes are unaffected: they return {@code false} from the URL check
     * rather than throwing.
     *
     * <p>An endpoint with health checking disabled needs no probe at all, so it is carried as an
     * already-completed result and applied under the lock like any other.
     *
     * @param selectorId selectorId
     * @param fingerprint configuration fingerprint the probe is submitted under
     * @param upstream upstream to probe
     * @param maxEndpointLatencyMillis per-round latency accumulator
     * @return the probe future, never completing exceptionally
     */
    private CompletableFuture<ProbeResult> submitCheck(final String selectorId, final long fingerprint,
                                                       final Upstream upstream,
                                                       final AtomicLong maxEndpointLatencyMillis) {
        if (!upstream.isHealthCheckEnabled()) {
            return CompletableFuture.completedFuture(ProbeResult.notProbed(selectorId, fingerprint, upstream));
        }
        return CompletableFuture
                .supplyAsync(() -> probe(selectorId, fingerprint, upstream, maxEndpointLatencyMillis), executor)
                .exceptionally(ex -> {
                    LOG.error("[Health Check] Selector [{}] upstream {} health check failed to complete.",
                            selectorId, upstream.getUrl(), ex);
                    return null;
                });
    }

    private boolean tryStartHealthCheck() {
        return checkStarted.compareAndSet(false, true);
    }

    /**
     * Await the results of the current round without holding the lock.
     *
     * <p>Each future is awaited on its own, so one failed or cancelled probe does not hide the
     * results of the others, and the caller always clears the round bookkeeping afterwards.
     *
     * @return the completed probe results, in submission order
     */
    private List<ProbeResult> waitFinish() {
        final List<ProbeResult> results = new ArrayList<>(futures.size());
        for (CompletableFuture<ProbeResult> future : futures) {
            final ProbeResult result = await(future);
            if (Objects.nonNull(result)) {
                results.add(result);
            } else if (Thread.currentThread().isInterrupted()) {
                LOG.warn("[Health Check] Interrupted while awaiting probe results, abandoning this round.");
                break;
            }
        }
        return results;
    }

    private ProbeResult await(final CompletableFuture<ProbeResult> future) {
        try {
            return future.get();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return null;
        } catch (CancellationException | ExecutionException e) {
            // Probes complete with null instead of exceptionally, so this only covers a future
            // that was cancelled or rejected before running. Skip it and keep applying the rest.
            LOG.error("[Health Check] A probe did not complete, skipping its result for this round.", e);
            return null;
        }
    }

    /**
     * Apply the results of a round under the lock, dropping the ones whose selector configuration
     * changed while they were in flight.
     *
     * <p>A configuration update reconciles the pools itself, based on the state it found, so
     * applying a result captured before that update could reintroduce a removed or drained target,
     * undo a replacement, or override a {@code healthCheckEnabled} change. Such results are
     * discarded and the next round probes the new configuration.
     *
     * <p>The current fingerprint of a selector is read once, before its first result is applied:
     * applying a result moves endpoints between pools, which changes the fingerprint but not the
     * configuration the round is being compared against. Holding the lock here is bounded by the
     * number of results — no network work happens inside it.
     *
     * @param results the completed probe results
     */
    private void applyResults(final List<ProbeResult> results) {
        if (results.isEmpty()) {
            return;
        }
        synchronized (lock) {
            final Map<String, Long> fingerprints = new HashMap<>();
            for (ProbeResult result : results) {
                final long current = fingerprints.computeIfAbsent(result.selectorId, this::fingerprintOf);
                if (current != result.fingerprint) {
                    LOG.debug("[Health Check] Selector [{}] changed while its probe was in flight, "
                            + "discarding the result for {}.", result.selectorId, result.upstream.getUrl());
                    continue;
                }
                applyResult(result);
            }
        }
    }

    private void applyResult(final ProbeResult result) {
        final Upstream upstream = result.upstream;
        if (!result.probed) {
            if (!upstream.isHealthy()) {
                upstream.setHealthy(true);
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
            }
        } else if (result.pass) {
            if (upstream.isHealthy()) {
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastUnhealthyTimestamp();
                if (interval >= (long) checkInterval * healthyThreshold) {
                    upstream.setHealthy(true);
                    upstream.setLastHealthTimestamp(now);
                    LOG.info("[Health Check] Selector [{}] upstream {} health check passed, server is back online.",
                            result.selectorId, upstream.getUrl());
                }
            }
        } else {
            if (!upstream.isHealthy()) {
                upstream.setLastUnhealthyTimestamp(System.currentTimeMillis());
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastHealthTimestamp();
                if (interval >= (long) checkInterval * unhealthyThreshold) {
                    upstream.setHealthy(false);
                    upstream.setLastUnhealthyTimestamp(now);
                    LOG.info("[Health Check] Selector [{}] upstream {} health check failed, server is offline.",
                            result.selectorId, upstream.getUrl());
                }
            }
        }
        putEntityToMap(result.selectorId, upstream);
    }

    private void putEntityToMap(final String selectorId, final Upstream upstream) {
        if (upstream.isHealthy()) {
            putToMap(healthyUpstream, selectorId, upstream);
            removeFromMap(unhealthyUpstream, selectorId, upstream);
        } else {
            putToMap(unhealthyUpstream, selectorId, upstream);
            removeFromMap(healthyUpstream, selectorId, upstream);
        }
    }

    private void finishHealthCheck() {
        checkStarted.set(false);
    }
    
    /**
     * Add one upstream via selectorData.
     *
     * @param selectorId selectorId
     * @param upstream upstream
     */
    public void triggerAddOne(final String selectorId, final Upstream upstream) {
        putToMap(healthyUpstream, selectorId, upstream);
    }

    /**
     * Remove a specific upstream via selectorId.
     *
     * @param selectorId selectorId
     * @param upstream upstream
     */
    public void triggerRemoveOne(final String selectorId, final Upstream upstream) {
        removeFromMap(healthyUpstream, selectorId, upstream);
        removeFromMap(unhealthyUpstream, selectorId, upstream);
    }

    /**
     * Put upstream to specified map (for preserving health status).
     *
     * @param map the map to put upstream
     * @param selectorId the selector id
     * @param upstream the upstream
     */
    public void putToMap(final Map<String, List<Upstream>> map, final String selectorId, final Upstream upstream) {
        synchronized (lock) {
            List<Upstream> list = MapUtils.computeIfAbsent(map, selectorId, k -> new CopyOnWriteArrayList<>());
            if (!list.contains(upstream)) {
                list.add(upstream);
            }
        }
    }

    /**
     * Remove upstream from specified map.
     *
     * @param map the map to remove upstream from
     * @param selectorId the selector id
     * @param upstream the upstream
     */
    public void removeFromMap(final Map<String, List<Upstream>> map, final String selectorId, final Upstream upstream) {
        synchronized (lock) {
            List<Upstream> list = map.get(selectorId);
            if (CollectionUtils.isNotEmpty(list)) {
                list.remove(upstream);
            }
        }
    }

    /**
     * Run a configuration update atomically with respect to health checks.
     *
     * <p>The lock is only held for the update itself: a health round no longer holds it while
     * waiting for probes, so this call does not wait for network I/O (#8139).
     *
     * @param action configuration update
     */
    void withLock(final Runnable action) {
        synchronized (lock) {
            action.run();
        }
    }

    /**
     * Remove all upstream via selectorId.
     *
     * @param selectorId selectorId
     */
    public void triggerRemoveAll(final String selectorId) {
        synchronized (lock) {
            healthyUpstream.remove(selectorId);
            unhealthyUpstream.remove(selectorId);
        }
    }
    
    /**
     * Print healthy and unhealthy check log.
     */
    public void print() {
        printHealthyUpstream();
        printUnhealthyUpstream();
    }
    
    private void printHealthyUpstream() {
        healthyUpstream.forEach((k, v) -> {
            if (Objects.nonNull(v)) {
                List<String> list = v.stream().map(Upstream::getUrl).collect(Collectors.toList());
                LOG.info("[Health Check] currently healthy upstream: {}", GsonUtils.getInstance().toJson(list));
            }
        });
    }
    
    private void printUnhealthyUpstream() {
        unhealthyUpstream.forEach((k, v) -> {
            if (Objects.nonNull(v)) {
                List<String> list = v.stream().map(Upstream::getUrl).collect(Collectors.toList());
                LOG.info("[Health Check] currently unhealthy upstream: {}", GsonUtils.getInstance().toJson(list));
            }
        });
    }
    
    /**
     * Get healthy upstream map.
     *
     * @return healthy map.
     */
    public Map<String, List<Upstream>> getHealthyUpstream() {
        return healthyUpstream;
    }

    /**
     * Get healthy upstream list.
     * @param selectorId selectorId
     * @return healthy upstream list.
     */
    public List<Upstream> getHealthyUpstreamListBySelectorId(final String selectorId) {
        return getHealthyUpstream().get(selectorId);
    }
    
    /**
     * Get unhealthy upstream map.
     *
     * @return unhealthy map.
     */
    public Map<String, List<Upstream>> getUnhealthyUpstream() {
        return unhealthyUpstream;
    }

    /**
     * Duration of the most recent health-check round in milliseconds, measured with
     * {@link System#nanoTime()} around the whole round (snapshot, submit, wait and apply).
     * Zero until the first round completes.
     *
     * <p>Note: a round that could not start (because another was still in flight) is still recorded,
     * so a {@code 0} duration together with {@code 0} from {@link #getLastRoundSubmittedChecks()}
     * means "this round was skipped", not "the round was instant".
     *
     * @return last round duration in millis
     */
    public long getLastRoundDurationMillis() {
        return lastRoundDurationMillis;
    }

    /**
     * Number of endpoint probes submitted to the async executor during the most recent round.
     * A {@code 0} value alongside a {@code 0} {@link #getLastRoundDurationMillis()} indicates the
     * round was skipped because another round was already in flight, rather than an empty population.
     *
     * @return submitted probe count
     */
    public int getLastRoundSubmittedChecks() {
        return lastRoundSubmittedChecks;
    }

    /**
     * Number of submitted probes that were still pending when the round entered {@code waitFinish()}.
     * A persistently high value indicates the executor is the bottleneck, not the endpoints.
     *
     * @return pending probe count at wait time
     */
    public int getLastRoundPendingChecks() {
        return lastRoundPendingChecks;
    }

    /**
     * Maximum single-endpoint probe latency observed during the most recent round, in milliseconds.
     *
     * @return max endpoint latency in millis
     */
    public long getLastRoundMaxEndpointLatencyMillis() {
        return lastRoundMaxEndpointLatencyMillis;
    }

    /**
     * Age of the oldest health-state timestamp across healthy and unhealthy buckets, in milliseconds.
     * Grows when rounds fall behind the configured interval. Upstreams that have never been measured
     * (unset {@code 0} timestamp, e.g. newly registered or health-check-disabled while already healthy)
     * are skipped, so an all-unmeasured population reports {@code 0} rather than an epoch-sized age.
     *
     * @return oldest health-state age in millis
     */
    public long getLastRoundOldestHealthStateMillis() {
        return lastRoundOldestHealthStateMillis;
    }

    /**
     * Wall-clock timestamp at which the most recent round finished, in milliseconds since epoch.
     *
     * @return last round completion timestamp
     */
    public long getLastRoundCompletedAtMillis() {
        return lastRoundCompletedAtMillis;
    }

    /**
     * Total number of health-check rounds completed since this task was created.
     *
     * @return total rounds
     */
    public long getTotalRounds() {
        return totalRounds.get();
    }

    /**
     * Total number of endpoint probes submitted since this task was created.
     *
     * @return total submitted probes
     */
    public long getTotalSubmittedChecks() {
        return totalSubmittedChecks.get();
    }

    /**
     * Cumulative probe latency in milliseconds since this task was created. Divide by
     * {@link #getTotalSubmittedChecks()} for an average; only probes with health-check
     * enabled contribute.
     *
     * @return cumulative endpoint latency in millis
     */
    public long getTotalEndpointLatencyMillis() {
        return totalEndpointLatencyMillis.get();
    }

    /**
     * The endpoints of one selector, captured for a round together with the configuration
     * fingerprint they are probed under.
     */
    private static final class SelectorSnapshot {

        private final String selectorId;

        private final long fingerprint;

        private final List<Upstream> upstreams;

        private SelectorSnapshot(final String selectorId, final long fingerprint, final List<Upstream> upstreams) {
            this.selectorId = selectorId;
            this.fingerprint = fingerprint;
            this.upstreams = upstreams;
        }
    }

    /**
     * One probe result awaiting application: the probed endpoint, whether the probe passed, and the
     * selector fingerprint captured when the probe was submitted. A result whose selector no longer
     * has that fingerprint is discarded instead of applied.
     */
    static final class ProbeResult {

        private final String selectorId;

        private final long fingerprint;

        private final Upstream upstream;

        private final boolean pass;

        private final boolean probed;

        ProbeResult(final String selectorId, final long fingerprint, final Upstream upstream,
                    final boolean pass, final boolean probed) {
            this.selectorId = selectorId;
            this.fingerprint = fingerprint;
            this.upstream = upstream;
            this.pass = pass;
            this.probed = probed;
        }

        static ProbeResult notProbed(final String selectorId, final long fingerprint, final Upstream upstream) {
            return new ProbeResult(selectorId, fingerprint, upstream, true, false);
        }
    }
}
