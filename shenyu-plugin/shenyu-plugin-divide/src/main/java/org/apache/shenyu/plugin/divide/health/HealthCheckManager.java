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
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Health check manager for upstream servers.
 */
@Slf4j
public class HealthCheckManager {

    private static final HealthCheckManager INSTANCE = new HealthCheckManager();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

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

    private HealthCheckManager() {
        initHealthCheck();
    }

    private void initHealthCheck() {
        checkEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.enable", "false"));
        checkTimeout = Integer.parseInt(System.getProperty("shenyu.upstream.check.timeout", "3000"));
        checkInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.interval", "5000"));
        healthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.healthy-threshold", "1"));
        unhealthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.unhealthy-threshold", "1"));

        scheduleHealthCheck();
    }

    private void scheduleHealthCheck() {
        if (checkEnable) {
            ThreadFactory healthCheckFactory = ShenyuThreadFactory.create("upstream-health-check", false);
            new ScheduledThreadPoolExecutor(1, healthCheckFactory)
                    .scheduleWithFixedDelay(this::healthCheck, 3000, checkInterval, TimeUnit.MILLISECONDS);
        }
    }

    private void healthCheck() {
        if (tryStartHealthCheck()) {
            doHealthCheck();
            finishHealthCheck();
        }
    }

    private void doHealthCheck() {
//        if (UPSTREAM_MAP.size() > 0) {
//            UPSTREAM_MAP.forEach((k, v) -> {
//                List<DivideUpstream> result = check(v);
//                if (result.size() > 0) {
//                    HEALTHY_UPSTREAM.put(k, result);
//                } else {
//                    HEALTHY_UPSTREAM.remove(k);
//                }
//            });
//        }
        check(HEALTHY_UPSTREAM);
        check(UNHEALTHY_UPSTREAM);
        check(WAITING_FOR_CHECK);

        log.info("[Health Check] Check finished.");
        HEALTHY_UPSTREAM.forEach((k, v) -> printHealthyUpstream(SELECTOR_CACHE.get(k)));
    }

    private void check(Map<String, List<DivideUpstream>> map) {
        map.forEach((k, v) -> {
            // TODO check health
        });
    }

    private List<DivideUpstream> check(final List<DivideUpstream> upstreamList) {
        List<DivideUpstream> resultList = Lists.newArrayListWithCapacity(upstreamList.size());
        for (DivideUpstream divideUpstream : upstreamList) {
            final boolean pass = UpstreamCheckUtils.checkUrl(divideUpstream.getUpstreamUrl(), checkTimeout);
            if (pass) {
                if (divideUpstream.isHealthy()) {
                    divideUpstream.setLastHealthTimestamp(System.currentTimeMillis());
                    resultList.add(divideUpstream);
                } else {
                    long now = System.currentTimeMillis();
                    long interval = now - divideUpstream.getLastUnhealthyTimestamp();
                    if (interval >= (long) checkInterval * healthyThreshold) {
                        divideUpstream.setHealthy(true);
                        divideUpstream.setLastHealthTimestamp(now);
                        resultList.add(divideUpstream);
                        log.info("upstream {} health check passed, server is back online.", divideUpstream.getUpstreamUrl());
                    }
                }
            } else {
                if (!divideUpstream.isHealthy()) {
                    divideUpstream.setLastUnhealthyTimestamp(System.currentTimeMillis());
                } else {
                    long now = System.currentTimeMillis();
                    long interval = now - divideUpstream.getLastHealthTimestamp();
                    if (interval >= (long) checkInterval * unhealthyThreshold) {
                        divideUpstream.setHealthy(false);
                        divideUpstream.setLastUnhealthyTimestamp(now);
                        log.info("upstream {} health check failed, server is offline.", divideUpstream.getUpstreamUrl());
                    } else {
                        // we assume it's still healthy.
                        resultList.add(divideUpstream);
                    }
                }
            }
        }

        return resultList;
    }

    private boolean tryStartHealthCheck() {
        return checkStarted.compareAndSet(false, true);
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
        List<DivideUpstream> list = HEALTHY_UPSTREAM.get(selectorData.getId());
        log.info("[Health Check] Selector {} currently healthy upstream: {}", selectorData.getName(), GsonUtils.getInstance().toJson(list));
    }

    public static Map<String, List<DivideUpstream>> getHealthyUpstream() {
        return HEALTHY_UPSTREAM;
    }
}
