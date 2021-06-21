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

import java.util.ArrayList;
import java.util.Iterator;
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
 *
 * TODO not thread-safe.
 * TODO WAITING_FOR_CHECK seems useless.
 */
@Slf4j
public class HealthCheckManager {

    private static final HealthCheckManager INSTANCE = new HealthCheckManager();

    private static final Map<String, List<DivideUpstream>> HEALTHY_UPSTREAM = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> UNHEALTHY_UPSTREAM = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> WAITING_FOR_CHECK = Maps.newConcurrentMap();

    private static final Map<String, SelectorData> SELECTOR_CACHE = Maps.newConcurrentMap();

    private final AtomicBoolean checkStarted = new AtomicBoolean(false);

    private Boolean checkEnable;
    private Integer checkTimeout;
    private Integer checkInterval;
    private Integer healthyThreshold;
    private Integer unhealthyThreshold;

    private ExecutorService executor;
    private final List<CompletableFuture<?>> futures = Lists.newArrayList();
    private static final Object LOCK = new Object();


    private HealthCheckManager() {
        initHealthCheck();
    }

    private void initHealthCheck() {
        checkEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.enable", "true"));
        checkTimeout = Integer.parseInt(System.getProperty("shenyu.upstream.check.timeout", "3000"));
        checkInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.interval", "5000"));
        healthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.healthy-threshold", "1"));
        unhealthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.unhealthy-threshold", "1"));

        scheduleHealthCheck();
    }

    private void scheduleHealthCheck() {
        if (checkEnable) {
            ThreadFactory healthCheckFactory = ShenyuThreadFactory.create("upstream-health-check", true);
            new ScheduledThreadPoolExecutor(1, healthCheckFactory)
                    .scheduleWithFixedDelay(this::healthCheck, 3000, checkInterval, TimeUnit.MILLISECONDS);

            // executor for async request, avoid request block health check thread
            ThreadFactory requestFactory = ShenyuThreadFactory.create("upstream-health-check-request", true);
            executor = new ScheduledThreadPoolExecutor(10, requestFactory);
        }
    }

    private void healthCheck() {
        try {
            if (tryStartHealthCheck()) {
                doHealthCheck();
                waitFinish();
                finishHealthCheck();
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
        check(WAITING_FOR_CHECK);

        log.info("[Health Check] Check finished.");
        HEALTHY_UPSTREAM.keySet().forEach(k -> printHealthyUpstream(SELECTOR_CACHE.get(k)));
    }

    private void check(Map<String, List<DivideUpstream>> map) {
        for (Map.Entry<String, List<DivideUpstream>> entry : map.entrySet()) {
            String key = entry.getKey();
            List<DivideUpstream> value = entry.getValue();
            Iterator<DivideUpstream> iterator = value.iterator();
            while (iterator.hasNext()) {
                // TODO bug here
                DivideUpstream upstream = iterator.next();
                iterator.remove();

                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> check(key, upstream), executor);
                futures.add(future);
            }
        }
    }

    private void check(String selectorId, DivideUpstream upstream) {
        boolean pass = UpstreamCheckUtils.checkUrl(upstream.getUpstreamUrl(), checkTimeout);
        if (pass) {
            if (upstream.isHealthy()) {
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
                putMap(HEALTHY_UPSTREAM, selectorId, upstream);
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastUnhealthyTimestamp();
                if (interval >= (long) checkInterval * healthyThreshold) {
                    upstream.setHealthy(true);
                    upstream.setLastHealthTimestamp(now);
                    putMap(HEALTHY_UPSTREAM, selectorId, upstream);
                    log.info("upstream {} health check passed, server is back online.", upstream.getUpstreamUrl());
                } else {
                    putMap(UNHEALTHY_UPSTREAM, selectorId, upstream);
                }
            }
        } else {
            if (!upstream.isHealthy()) {
                upstream.setLastUnhealthyTimestamp(System.currentTimeMillis());
                putMap(UNHEALTHY_UPSTREAM, selectorId, upstream);
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastHealthTimestamp();
                if (interval >= (long) checkInterval * unhealthyThreshold) {
                    upstream.setHealthy(false);
                    upstream.setLastUnhealthyTimestamp(now);
                    putMap(UNHEALTHY_UPSTREAM, selectorId, upstream);
                    log.info("upstream {} health check failed, server is offline.", upstream.getUpstreamUrl());
                } else {
                    putMap(HEALTHY_UPSTREAM, selectorId, upstream);
                }
            }
        }
    }

    private void putMap(Map<String, List<DivideUpstream>> map, String selectorId, DivideUpstream upstream) {
        List<DivideUpstream> list = map.computeIfAbsent(selectorId, k -> Lists.newArrayList());
        if (!list.contains(upstream)) {
            list.add(upstream);
        }
    }

    private boolean tryStartHealthCheck() {
        return checkStarted.compareAndSet(false, true);
    }

    private void waitFinish() throws ExecutionException, InterruptedException {
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).get();
        futures.clear();
    }

    private void finishHealthCheck() {
        checkStarted.set(false);
    }

    public static void triggerAddOne(SelectorData selectorData, DivideUpstream upstream) {
        List<DivideUpstream> list = WAITING_FOR_CHECK.computeIfAbsent(selectorData.getId(), k -> Lists.newArrayList());
        list.add(upstream);

        SELECTOR_CACHE.putIfAbsent(selectorData.getId(), selectorData);
    }

    public static void triggerRemoveOne(SelectorData selectorData, DivideUpstream upstream) {
        removeFromMap(HEALTHY_UPSTREAM, selectorData.getId(), upstream);
        removeFromMap(UNHEALTHY_UPSTREAM, selectorData.getId(), upstream);
        removeFromMap(WAITING_FOR_CHECK, selectorData.getId(), upstream);
        printHealthyUpstream(selectorData);
    }

    private static void removeFromMap(Map<String, List<DivideUpstream>> map, String selectorId, DivideUpstream upstream) {
        List<DivideUpstream> list = map.get(selectorId);
        if (CollectionUtils.isNotEmpty(list)) {
            list.remove(upstream);
        }
    }

    public static void triggerRemoveAll(SelectorData selectorData) {
        HEALTHY_UPSTREAM.remove(selectorData.getId());
        UNHEALTHY_UPSTREAM.remove(selectorData.getId());
        WAITING_FOR_CHECK.remove(selectorData.getId());
        printHealthyUpstream(selectorData);
    }

    private static void printHealthyUpstream(SelectorData selectorData) {
        List<DivideUpstream> upstreamList = HEALTHY_UPSTREAM.get(selectorData.getId());
        if (upstreamList != null) {
            List<String> list = HEALTHY_UPSTREAM.get(selectorData.getId()).stream().map(DivideUpstream::getUpstreamUrl).collect(Collectors.toList());
            log.info("[Health Check] Selector {} currently healthy upstream: {}", selectorData.getName(), GsonUtils.getInstance().toJson(list));
        }
    }

    public static Map<String, List<DivideUpstream>> getHealthyUpstream() {
        return HEALTHY_UPSTREAM;
    }
}
