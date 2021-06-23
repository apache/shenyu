package org.apache.shenyu.plugin.divide.health;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * Health check manager for upstream servers.
 */
@Slf4j
public class HealthCheckManager {

    private static final HealthCheckManager INSTANCE = new HealthCheckManager();

    private static final Map<String, List<DivideUpstream>> HEALTHY_UPSTREAM = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> UNHEALTHY_UPSTREAM = Maps.newConcurrentMap();

    private static final Map<String, SelectorData> SELECTOR_CACHE = Maps.newConcurrentMap();

    private final AtomicBoolean checkStarted = new AtomicBoolean(false);

    private Boolean checkEnable;
    private Integer checkTimeout;
    private Integer checkInterval;
    private Integer healthyThreshold;
    private Integer unhealthyThreshold;

    // healthy upstream print param
    private Boolean printEnable;
    private Integer printInterval;

    private ExecutorService executor;
    private final List<CompletableFuture<UpstreamWithSelectorId>> futures = Lists.newArrayList();
    private static final Object LOCK = new Object();

    public static HealthCheckManager getInstance() {
        return INSTANCE;
    }

    private HealthCheckManager() {
        initHealthCheck();
    }

    private void initHealthCheck() {
        checkEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.enable", "true"));
        checkTimeout = Integer.parseInt(System.getProperty("shenyu.upstream.check.timeout", "3000"));
        checkInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.interval", "5000"));
        healthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.healthy-threshold", "1"));
        unhealthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.unhealthy-threshold", "1"));

        printEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.print.enable", "true"));
        printInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.print.interval", "60000"));

        scheduleHealthCheck();
    }

    private void scheduleHealthCheck() {
        if (checkEnable) {
            // executor for health check
            ThreadFactory healthCheckFactory = ShenyuThreadFactory.create("upstream-health-check", true);
            new ScheduledThreadPoolExecutor(1, healthCheckFactory)
                    .scheduleWithFixedDelay(this::healthCheck, 3000, checkInterval, TimeUnit.MILLISECONDS);

            // executor for async request, avoid request block health check thread
            ThreadFactory requestFactory = ShenyuThreadFactory.create("upstream-health-check-request", true);
            executor = new ScheduledThreadPoolExecutor(10, requestFactory);

            // executor for log print
            if (printEnable) {
                ThreadFactory printFactory = ShenyuThreadFactory.create("upstream-health-print", true);
                new ScheduledThreadPoolExecutor(1, printFactory)
                        .scheduleWithFixedDelay(this::printHealthyUpstream, printInterval, printInterval, TimeUnit.MILLISECONDS);
            }
        }
    }

    private void healthCheck() {
        try {
            // If there is no synchronized. when check is done and all upstream check result is in the futures list.
            // In the same time, triggerRemoveAll() called before waitFinish(), there will be dirty data stay in map.
            synchronized (LOCK) {
                if (tryStartHealthCheck()) {
                    doHealthCheck();
                    waitFinish();
                }
            }
        } catch (Exception e) {
            log.error("[Health Check] Meet problem: ", e);
        } finally {
            finishHealthCheck();
        }
    }

    private void doHealthCheck() {
        check(HEALTHY_UPSTREAM);
        check(UNHEALTHY_UPSTREAM);
    }

    private void check(Map<String, List<DivideUpstream>> map) {
        for (Map.Entry<String, List<DivideUpstream>> entry : map.entrySet()) {
            String key = entry.getKey();
            List<DivideUpstream> value = entry.getValue();
            for (DivideUpstream upstream : value) {
                CompletableFuture<UpstreamWithSelectorId> future = CompletableFuture.supplyAsync(() -> check(key, upstream), executor);

                futures.add(future);
            }
        }
    }

    private UpstreamWithSelectorId check(String selectorId, DivideUpstream upstream) {
        SelectorData selectorData = SELECTOR_CACHE.get(selectorId);
        boolean pass = UpstreamCheckUtils.checkUrl(upstream.getUpstreamUrl(), checkTimeout);
        if (pass) {
            if (upstream.isHealthy()) {
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastUnhealthyTimestamp();
                if (interval >= (long) checkInterval * healthyThreshold) {
                    upstream.setHealthy(true);
                    upstream.setLastHealthTimestamp(now);
                    log.info("[Health Check] Selector [{}] upstream {} health check passed, server is back online.",
                            selectorData.getName(), upstream.getUpstreamUrl());
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
                    log.info("[Health Check] Selector [{}] upstream {} health check failed, server is offline.",
                            selectorData.getName(), upstream.getUpstreamUrl());
                }
            }
        }

        return new UpstreamWithSelectorId(selectorId, upstream);
    }

    private boolean tryStartHealthCheck() {
        return checkStarted.compareAndSet(false, true);
    }

    private void waitFinish() throws ExecutionException, InterruptedException {
        for (CompletableFuture<UpstreamWithSelectorId> future : futures) {
            UpstreamWithSelectorId entity = future.get();
            putEntityToMap(entity);
        }

        futures.clear();
    }

    private void putEntityToMap(UpstreamWithSelectorId entity) {
        DivideUpstream upstream = entity.getDivideUpstream();
        if (upstream.isHealthy()) {
            putToMap(HEALTHY_UPSTREAM, entity.getSelectorId(), upstream);
            removeFromMap(UNHEALTHY_UPSTREAM, entity.getSelectorId(), upstream);
        } else {
            putToMap(UNHEALTHY_UPSTREAM, entity.getSelectorId(), upstream);
            removeFromMap(HEALTHY_UPSTREAM, entity.getSelectorId(), upstream);
        }
    }

    private void finishHealthCheck() {
        checkStarted.set(false);
    }

    public void triggerAddOne(SelectorData selectorData, DivideUpstream upstream) {
        SELECTOR_CACHE.putIfAbsent(selectorData.getId(), selectorData);

        // check immediately
        log.info("[Health Check] Selector [{}] new incoming upstream: {}, check immediately.",
                selectorData.getName(), upstream.getUpstreamUrl());

        UpstreamWithSelectorId entity = check(selectorData.getId(), upstream);
        if (!entity.getDivideUpstream().isHealthy()) {
            log.info("[Health Check] Selector [{}] upstream {} health check failed, server is offline.",
                    selectorData.getName(), upstream.getUpstreamUrl());
        }

        putEntityToMap(entity);
    }

    public void triggerRemoveOne(SelectorData selectorData, DivideUpstream upstream) {
        triggerRemoveOne(selectorData.getId(), upstream);
    }

    public void triggerRemoveOne(String selectorId, DivideUpstream upstream) {
        removeFromMap(HEALTHY_UPSTREAM, selectorId, upstream);
        removeFromMap(UNHEALTHY_UPSTREAM, selectorId, upstream);
    }

    private void putToMap(Map<String, List<DivideUpstream>> map, String selectorId, DivideUpstream upstream) {
        synchronized (LOCK) {
            List<DivideUpstream> list = map.computeIfAbsent(selectorId, k -> Lists.newArrayList());
            if (!list.contains(upstream)) {
                list.add(upstream);
            }
        }
    }

    private void removeFromMap(Map<String, List<DivideUpstream>> map, String selectorId, DivideUpstream upstream) {
        synchronized (LOCK) {
            List<DivideUpstream> list = map.get(selectorId);
            if (CollectionUtils.isNotEmpty(list)) {
                list.remove(upstream);
            }
        }
    }

    public void triggerRemoveAll(SelectorData selectorData) {
        triggerRemoveAll(selectorData.getId());
    }

    public void triggerRemoveAll(String selectorId) {
        synchronized (LOCK) {
            HEALTHY_UPSTREAM.remove(selectorId);
            UNHEALTHY_UPSTREAM.remove(selectorId);
            SELECTOR_CACHE.remove(selectorId);
        }
    }

    private void printHealthyUpstream() {
        HEALTHY_UPSTREAM.forEach((k, v) -> {
            if (v != null) {
                SelectorData selectorData = SELECTOR_CACHE.get(k);
                List<String> list = v.stream().map(DivideUpstream::getUpstreamUrl).collect(Collectors.toList());
                log.info("[Health Check] Selector [{}] currently healthy upstream: {}",
                        selectorData.getName(), GsonUtils.getInstance().toJson(list));
            }
        });
    }

    public Map<String, List<DivideUpstream>> getHealthyUpstream() {
        return HEALTHY_UPSTREAM;
    }

    public Map<String, List<DivideUpstream>> getUnhealthyUpstream() {
        return UNHEALTHY_UPSTREAM;
    }
}
