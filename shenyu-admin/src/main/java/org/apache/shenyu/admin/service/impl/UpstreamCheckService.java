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

package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.common.util.concurrent.Striped;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.discovery.DiscoveryStreamUpdatedEvent;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingException;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.ZombieUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionTemplate;

import jakarta.annotation.PreDestroy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * This is the upstream check service.
 */
@Component
public class UpstreamCheckService {

    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckService.class);

    private static final Map<String, List<CommonUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private static final Striped<Lock> UPSTREAM_UPDATE_LOCKS = Striped.lazyWeakLock(1024);

    private static final Set<Integer> PENDING_SYNC = Sets.newConcurrentHashSet();

    private static final Set<ZombieUpstream> ZOMBIE_SET = Sets.newConcurrentHashSet();

    private static final String REGISTER_TYPE_HTTP = "http";

    private static int zombieRemovalTimes;

    private final int zombieCheckTimes;

    private final int scheduledTime;

    private final String registerType;

    private final boolean checked;

    private final boolean localMode;

    private final Integer scheduledThreads;

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final PluginMapper pluginMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final SelectorHandleConverterFactor converterFactor;

    private final DiscoveryUpstreamService discoveryUpstreamService;

    /**
     * The selector row lock must cover the final discovery snapshot and selector
     * handle write. The field is optional for the lightweight unit tests that
     * construct this service without a Spring transaction manager.
     */
    @jakarta.annotation.Resource
    private PlatformTransactionManager transactionManager;

    private ScheduledThreadPoolExecutor executor;

    private ScheduledFuture<?> scheduledFuture;

    private ScheduledThreadPoolExecutor invokeExecutor;

    private final List<CompletableFuture<Void>> futures = Lists.newArrayList();

    /**
     * Instantiates a new Upstream check service.
     *
     * @param selectorMapper             the selector mapper
     * @param eventPublisher             the event publisher
     * @param pluginMapper               the plugin mapper
     * @param selectorConditionMapper    the selectorCondition mapper
     * @param shenyuRegisterCenterConfig the shenyu register center config
     * @param converterFactor            the converter factor
     */
    public UpstreamCheckService(final SelectorMapper selectorMapper,
                                final ApplicationEventPublisher eventPublisher,
                                final PluginMapper pluginMapper,
                                final SelectorConditionMapper selectorConditionMapper,
                                final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig,
                                final SelectorHandleConverterFactor converterFactor,
                                final DiscoveryUpstreamService discoveryUpstreamService) {
        this.selectorMapper = selectorMapper;
        this.eventPublisher = eventPublisher;
        this.pluginMapper = pluginMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.converterFactor = converterFactor;
        this.discoveryUpstreamService = discoveryUpstreamService;
        Properties props = shenyuRegisterCenterConfig.getProps();
        this.checked = Boolean.parseBoolean(props.getProperty(Constants.IS_CHECKED, Constants.DEFAULT_CHECK_VALUE));
        this.localMode = parseProbeMode(props.getProperty(Constants.PROBE_MODE, Constants.DEFAULT_PROBE_MODE_VALUE));
        this.scheduledThreads = Integer.parseInt(props.getProperty(Constants.ZOMBIE_CHECK_THREADS, Constants.ZOMBIE_CHECK_THREADS_VALUE));
        this.zombieCheckTimes = Integer.parseInt(props.getProperty(Constants.ZOMBIE_CHECK_TIMES, Constants.ZOMBIE_CHECK_TIMES_VALUE));
        this.scheduledTime = Integer.parseInt(props.getProperty(Constants.SCHEDULED_TIME, Constants.SCHEDULED_TIME_VALUE));
        this.registerType = shenyuRegisterCenterConfig.getRegisterType();
        zombieRemovalTimes = Integer.parseInt(props.getProperty(Constants.ZOMBIE_REMOVAL_TIMES, Constants.ZOMBIE_REMOVAL_TIMES_VALUE));
    }

    private static boolean parseProbeMode(final String probeMode) {
        if (Constants.LOCAL_PROBE_MODE_VALUE.equalsIgnoreCase(probeMode)) {
            return true;
        }
        if (!Constants.DEFAULT_PROBE_MODE_VALUE.equalsIgnoreCase(probeMode)) {
            LOG.warn("unknown probe mode [{}], fallback to [{}]", probeMode, Constants.DEFAULT_PROBE_MODE_VALUE);
        }
        return false;
    }

    /**
     * Whether the local probe mode is enabled. In local mode the admin never gates
     * registration nor removes upstreams based on its own reachability probes;
     * bootstrap-local health checking decides routing eligibility.
     *
     * @return true if the local probe mode is enabled
     */
    public boolean isLocalMode() {
        return localMode;
    }

    /**
     * Set up.
     */
    public void setup() {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked) {
            return;
        }
        if (localMode) {
            LOG.info("skip upstream check task in local probe mode, upstream reachability is observed by each bootstrap locally");
            return;
        }
        LOG.info("setup upstream check task");
        this.fetchUpstreamData();
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-upstream-task", false));
        scheduledFuture = executor.scheduleWithFixedDelay(this::scheduled, 10, scheduledTime, TimeUnit.SECONDS);

        ThreadFactory requestFactory = ShenyuThreadFactory.create("upstream-health-check-request", true);
        invokeExecutor = new ScheduledThreadPoolExecutor(this.scheduledThreads, requestFactory);
    }

    /**
     * Close relative resource on container destroy.
     */
    @PreDestroy
    public void close() {
        if (checked) {
            if (Objects.nonNull(scheduledFuture)) {
                scheduledFuture.cancel(false);
            }
            if (Objects.nonNull(executor)) {
                executor.shutdown();
            }
            if (Objects.nonNull(invokeExecutor)) {
                invokeExecutor.shutdown();
            }
        }
    }

    /**
     * Remove by key.
     *
     * @param selectorId the selector id
     */
    public static void removeByKey(final String selectorId) {
        Lock lock = UPSTREAM_UPDATE_LOCKS.get(selectorId);
        lock.lock();
        try {
            UPSTREAM_MAP.remove(selectorId);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Submit client health check.
     *
     * @param selectorId     the selector id
     * @param commonUpstream the common upstream
     */
    public void submit(final String selectorId, final CommonUpstream commonUpstream) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked || localMode) {
            return;
        }

        Optional.ofNullable(submitJust(selectorId, commonUpstream))
                .ifPresent(upstreams -> executor.execute(() -> updateHandler(selectorId, upstreams, upstreams, upstreams)));
    }

    private List<CommonUpstream> submitJust(final String selectorId, final CommonUpstream commonUpstream) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked || localMode) {
            return null;
        }

        return UPSTREAM_MAP.compute(selectorId, (key, current) -> {
            List<CommonUpstream> upstreams = Objects.isNull(current) ? new CopyOnWriteArrayList<>() : new CopyOnWriteArrayList<>(current);
            if (commonUpstream.isStatus()) {
                Optional<CommonUpstream> exists = upstreams.stream().filter(item -> StringUtils.isNotBlank(item.getUpstreamUrl())
                        && item.getUpstreamUrl().equals(commonUpstream.getUpstreamUrl())).findFirst();
                if (!exists.isPresent()) {
                    upstreams.add(commonUpstream);
                } else {
                    exists.get().setHealthCheckEnabled(commonUpstream.isHealthCheckEnabled());
                }
                PENDING_SYNC.add(commonUpstream.hashCode());
            } else {
                upstreams.removeIf(item -> item.equals(commonUpstream));
                PENDING_SYNC.add(NumberUtils.INTEGER_ZERO);
            }
            return upstreams;
        });
    }

    /**
     * If the health check passes, the service will be added to
     * the normal service list; if the health check fails, the service
     * will not be discarded directly and add to the zombie nodes.
     *
     * <p>Note: This is to be compatible with older versions of clients
     * that do not register with the gateway by listening to
     * {@link org.springframework.context.event.ContextRefreshedEvent},
     * which will cause some problems,
     * check <a href="https://github.com/apache/shenyu/issues/3484">...</a> for more details.
     *
     * @param selectorId     the selector id
     * @param commonUpstream the common upstream
     * @return whether this module handles
     */
    public boolean checkAndSubmit(final String selectorId, final CommonUpstream commonUpstream) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked) {
            return false;
        }
        if (localMode) {
            // Local probe mode: the admin never probes, never zombifies and never blocks
            // registration on its own reachability view, so the caller proceeds normally.
            return false;
        }
        if (!commonUpstream.isHealthCheckEnabled()) {
            commonUpstream.setStatus(true);
            commonUpstream.setTimestamp(System.currentTimeMillis());
            this.submit(selectorId, commonUpstream);
            return false;
        }
        final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
        if (pass) {
            this.submit(selectorId, commonUpstream);
            return false;
        }
        ZOMBIE_SET.add(ZombieUpstream.transform(commonUpstream, zombieCheckTimes, selectorId));
        LOG.error("add zombie node, url={}", commonUpstream.getUpstreamUrl());
        return true;
    }

    /**
     * Replace.
     *
     * @param selectorId      the selector name
     * @param commonUpstreams the common upstream list
     */
    public void replace(final String selectorId, final List<CommonUpstream> commonUpstreams) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType)) {
            return;
        }
        Lock lock = UPSTREAM_UPDATE_LOCKS.get(selectorId);
        lock.lock();
        try {
            UPSTREAM_MAP.put(selectorId, toThreadSafeList(commonUpstreams));
        } finally {
            lock.unlock();
        }
    }

    private void scheduled() {
        try {
            doCheck();
            waitFinish();
        } catch (Exception e) {
            LOG.error("upstream scheduled check error", e);
        }
    }

    private void doCheck() {
        if (localMode) {
            // Defensive guard: the scheduled task is never created in local mode,
            // and the executors it relies on may not exist.
            return;
        }
        // check zombie
        if (!ZOMBIE_SET.isEmpty()) {
            ZOMBIE_SET.forEach(this::checkZombie);
        }
        // check up
        if (!UPSTREAM_MAP.isEmpty()) {
            UPSTREAM_MAP.forEach(this::check);
        }
    }

    private void waitFinish() {
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        } finally {
            // A failed check must not remain in the next cycle.
            futures.clear();
        }
    }

    private void checkZombie(final ZombieUpstream zombieUpstream) {
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> checkZombie0(zombieUpstream), invokeExecutor);
        futures.add(future);
    }

    private void checkZombie0(final ZombieUpstream zombieUpstream) {
        ZOMBIE_SET.remove(zombieUpstream);
        String selectorId = zombieUpstream.getSelectorId();
        CommonUpstream commonUpstream = zombieUpstream.getCommonUpstream();
        boolean originalStatus = commonUpstream.isStatus();
        if (!commonUpstream.isHealthCheckEnabled()) {
            commonUpstream.setTimestamp(System.currentTimeMillis());
            commonUpstream.setStatus(true);
            List<CommonUpstream> old = ListUtils.unmodifiableList(UPSTREAM_MAP.getOrDefault(selectorId, Collections.emptyList()));
            this.submitJust(selectorId, commonUpstream);
            reconcileZombie(selectorId, zombieUpstream, commonUpstream, old, originalStatus);
            return;
        }
        final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
        if (pass) {
            commonUpstream.setTimestamp(System.currentTimeMillis());
            commonUpstream.setStatus(true);
            LOG.info("UpstreamCacheManager check zombie upstream success the url: {}, host: {} ", commonUpstream.getUpstreamUrl(), commonUpstream.getUpstreamHost());
            List<CommonUpstream> old = ListUtils.unmodifiableList(UPSTREAM_MAP.getOrDefault(selectorId, Collections.emptyList()));
            // fix https://github.com/apache/shenyu/issues/5311
            this.submitJust(selectorId, commonUpstream);
            reconcileZombie(selectorId, zombieUpstream, commonUpstream, old, originalStatus);
        } else {
            LOG.error("check zombie upstream the url={} is fail", commonUpstream.getUpstreamUrl());
            if (zombieUpstream.getZombieCheckTimes() > NumberUtils.INTEGER_ZERO) {
                zombieUpstream.setZombieCheckTimes(zombieUpstream.getZombieCheckTimes() - NumberUtils.INTEGER_ONE);
                ZOMBIE_SET.add(zombieUpstream);
            }
        }
    }

    private void reconcileZombie(final String selectorId, final ZombieUpstream zombieUpstream,
                                 final CommonUpstream commonUpstream, final List<CommonUpstream> old,
                                 final boolean originalStatus) {
        List<CommonUpstream> updated = UPSTREAM_MAP.get(selectorId);
        boolean rejected = updateHandler(selectorId, old, updated, updated, Sets.newHashSet(),
                Collections.singletonMap(commonUpstream.getUpstreamUrl(), originalStatus));
        if (rejected) {
            ZOMBIE_SET.add(zombieUpstream);
        }
    }

    private void check(final String selectorId, final List<CommonUpstream> upstreamList) {
        final List<CommonUpstream> checkedUpstreams = new ArrayList<>(upstreamList);
        final Map<String, Boolean> originalStatuses = checkedUpstreams.stream()
                .collect(Collectors.toMap(CommonUpstream::getUpstreamUrl, CommonUpstream::isStatus, (left, right) -> left));
        final List<CompletableFuture<CommonUpstream>> checkFutures = new ArrayList<>(checkedUpstreams.size());
        final Set<String> fencedRejectedUrls = Sets.newConcurrentHashSet();
        for (CommonUpstream commonUpstream : checkedUpstreams) {
            checkFutures.add(CompletableFuture.supplyAsync(() -> {
                if (!commonUpstream.isHealthCheckEnabled()) {
                    if (!commonUpstream.isStatus()) {
                        commonUpstream.setTimestamp(System.currentTimeMillis());
                        commonUpstream.setStatus(true);
                        PENDING_SYNC.add(commonUpstream.hashCode());
                    }
                    return commonUpstream;
                }
                final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
                if (pass) {
                    if (!commonUpstream.isStatus()) {
                        commonUpstream.setTimestamp(System.currentTimeMillis());
                        commonUpstream.setStatus(true);
                        PENDING_SYNC.add(commonUpstream.hashCode());
                        LOG.info("UpstreamCacheManager check success the url: {}, host: {} ", commonUpstream.getUpstreamUrl(), commonUpstream.getUpstreamHost());
                    }
                    return commonUpstream;
                } else {
                    LOG.info("change unlive selectorId={}|url={}", selectorId, commonUpstream.getUpstreamUrl());
                    if (!discoveryUpstreamService.tryChangeStatusBySelectorIdAndUrl(selectorId, commonUpstream.getUpstreamUrl(), Boolean.FALSE)) {
                        fencedRejectedUrls.add(commonUpstream.getUpstreamUrl());
                        return commonUpstream;
                    }
                    commonUpstream.setStatus(false);
                    ZOMBIE_SET.add(ZombieUpstream.transform(commonUpstream, zombieCheckTimes, selectorId));
                    LOG.error("check the url={} is fail ", commonUpstream.getUpstreamUrl());
                }
                return null;
            }, invokeExecutor).exceptionally(ex -> {
                if (unwrapFencingException(ex)) {
                    // A rejected probe is not a successful probe. Preserve the
                    // previous in-memory state and let reconciliation retain
                    // the persisted route without issuing another write.
                    fencedRejectedUrls.add(commonUpstream.getUpstreamUrl());
                    return commonUpstream;
                }
                LOG.error("An exception occurred during the check of url {}: ", commonUpstream.getUpstreamUrl(), ex);
                return null;
            }));
        }

        this.futures.add(CompletableFuture.runAsync(() -> {
            List<CommonUpstream> successList = checkFutures.stream()
                    .map(CompletableFuture::join)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            updateHandler(selectorId, checkedUpstreams, successList, upstreamList, fencedRejectedUrls, originalStatuses);
        }));
    }

    private void updateHandler(final String selectorId, final List<CommonUpstream> upstreamList, final List<CommonUpstream> successList) {
        updateHandler(selectorId, upstreamList, successList, null);
    }

    private void updateHandler(final String selectorId, final List<CommonUpstream> upstreamList,
                               final List<CommonUpstream> successList, final List<CommonUpstream> expectedUpstreams) {
        updateHandler(selectorId, upstreamList, successList, expectedUpstreams, Sets.newHashSet());
    }

    private void updateHandler(final String selectorId, final List<CommonUpstream> upstreamList,
                               final List<CommonUpstream> successList, final List<CommonUpstream> expectedUpstreams,
                               final Set<String> fencedRejectedUrls) {
        updateHandler(selectorId, upstreamList, successList, expectedUpstreams, fencedRejectedUrls, Collections.emptyMap());
    }

    private boolean updateHandler(final String selectorId, final List<CommonUpstream> upstreamList,
                                  final List<CommonUpstream> successList, final List<CommonUpstream> expectedUpstreams,
                                  final Set<String> fencedRejectedUrls, final Map<String, Boolean> originalStatuses) {
        //No node changes, including zombie node resurrection and live node death
        if (successList.size() == upstreamList.size() && PENDING_SYNC.isEmpty()) {
            return false;
        }
        // Serialize discovery replacement with publication, without holding a ConcurrentHashMap bin lock during I/O.
        Lock lock = UPSTREAM_UPDATE_LOCKS.get(selectorId);
        lock.lock();
        try {
            return executeUpdateTransaction(() -> updateHandlerInTransaction(selectorId, upstreamList, successList,
                    expectedUpstreams, fencedRejectedUrls, originalStatuses));
        } finally {
            lock.unlock();
        }
    }

    private boolean updateHandlerInTransaction(final String selectorId, final List<CommonUpstream> upstreamList,
                                               final List<CommonUpstream> successList,
                                               final List<CommonUpstream> expectedUpstreams,
                                               final Set<String> fencedRejectedUrls,
                                               final Map<String, Boolean> originalStatuses) {
        List<CommonUpstream> current = UPSTREAM_MAP.get(selectorId);
        // A registry replacement or removal supersedes an in-flight health check.
        if (Objects.nonNull(expectedUpstreams) && current != expectedUpstreams) {
            return false;
        }
        removePendingSync(successList);
        List<CommonUpstream> alive = buildAliveList(upstreamList, successList, expectedUpstreams, current);
        Set<String> checkedFailedUrls = upstreamList.stream().map(CommonUpstream::getUpstreamUrl).collect(Collectors.toSet());
        successList.forEach(upstream -> checkedFailedUrls.remove(upstream.getUpstreamUrl()));
        fencedRejectedUrls.forEach(checkedFailedUrls::remove);
        List<CommonUpstream> reconciled = updateSelectorHandler(selectorId, alive, checkedFailedUrls, fencedRejectedUrls);
        restoreRejectedStatuses(upstreamList, successList, fencedRejectedUrls, originalStatuses);
        // A registration can update the cache during publication; retain it for its queued update.
        UPSTREAM_MAP.compute(selectorId, (key, latest) -> latest == current
                ? (reconciled.isEmpty() ? null : toThreadSafeList(reconciled)) : latest);
        return !fencedRejectedUrls.isEmpty();
    }

    private List<CommonUpstream> buildAliveList(final List<CommonUpstream> upstreamList,
                                                final List<CommonUpstream> successList,
                                                final List<CommonUpstream> expectedUpstreams,
                                                final List<CommonUpstream> current) {
        List<CommonUpstream> alive = new ArrayList<>(successList);
        if (Objects.nonNull(expectedUpstreams)) {
            current.stream().filter(upstream -> upstreamList.stream().noneMatch(checkedUpstream ->
                    checkedUpstream.getUpstreamUrl().equals(upstream.getUpstreamUrl())))
                    .filter(CommonUpstream::isStatus).forEach(alive::add);
        }
        return alive;
    }

    private void restoreRejectedStatuses(final List<CommonUpstream> upstreamList,
                                         final List<CommonUpstream> successList,
                                         final Set<String> fencedRejectedUrls,
                                         final Map<String, Boolean> originalStatuses) {
        upstreamList.stream().filter(upstream -> fencedRejectedUrls.contains(upstream.getUpstreamUrl()))
                .forEach(upstream -> restoreStatus(upstream, originalStatuses));
        successList.stream().filter(upstream -> fencedRejectedUrls.contains(upstream.getUpstreamUrl()))
                .forEach(upstream -> restoreStatus(upstream, originalStatuses));
    }

    private <T> T executeUpdateTransaction(final Supplier<T> action) {
        if (Objects.isNull(transactionManager) || TransactionSynchronizationManager.isActualTransactionActive()) {
            return action.get();
        }
        return new TransactionTemplate(transactionManager).execute(status -> action.get());
    }

    private void restoreStatus(final CommonUpstream upstream, final Map<String, Boolean> originalStatuses) {
        Boolean original = originalStatuses.get(upstream.getUpstreamUrl());
        if (Objects.nonNull(original)) {
            upstream.setStatus(original);
        }
    }

    private void removePendingSync(final List<CommonUpstream> successList) {
        PENDING_SYNC.removeIf(NumberUtils.INTEGER_ZERO::equals);
        successList.forEach(commonUpstream -> PENDING_SYNC.remove(commonUpstream.hashCode()));
    }

    private List<CommonUpstream> toThreadSafeList(final List<CommonUpstream> upstreams) {
        return upstreams instanceof CopyOnWriteArrayList ? upstreams : new CopyOnWriteArrayList<>(upstreams);
    }

    private List<CommonUpstream> updateSelectorHandler(final String selectorId, final List<CommonUpstream> aliveList, final Set<String> checkedFailedUrls) {
        return updateSelectorHandler(selectorId, aliveList, checkedFailedUrls, Sets.newHashSet());
    }

    private List<CommonUpstream> updateSelectorHandler(final String selectorId, final List<CommonUpstream> aliveList,
                                                       final Set<String> checkedFailedUrls,
                                                       final Set<String> fencedRejectedUrls) {
        if (localMode) {
            // Local probe mode must not reconcile admin probe results into persisted or routing state.
            return aliveList;
        }
        // The caller holds the selector transaction for the complete snapshot,
        // status reconciliation and selector handle write. Direct unit-test
        // calls without a transaction use the ordinary read path.
        SelectorDO selectorDO = selectSelectorForUpdate(selectorId);
        if (Objects.isNull(selectorDO)) {
            return aliveList;
        }

        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        if (Objects.isNull(pluginDO)) {
            return aliveList;
        }

        boolean hasDiscoveryHandler = discoveryUpstreamService.hasDiscoveryHandler(selectorId);
        List<DiscoveryUpstreamData> discoveryUpstreamDataList = loadDiscoverySnapshot(selectorId, aliveList, hasDiscoveryHandler);
        Set<String> changedUrls = Sets.newHashSet();
        discoveryUpstreamDataList = reconcileDiscoverySnapshot(selectorId, aliveList, checkedFailedUrls,
                fencedRejectedUrls, hasDiscoveryHandler, discoveryUpstreamDataList, changedUrls);
        List<CommonUpstream> currentAlive = discoveryUpstreamDataList.stream().map(upstream -> {
            CommonUpstream common = aliveList.stream().filter(alive -> alive.getUpstreamUrl().equals(upstream.getUrl())).findFirst()
                    .orElseGet(() -> DiscoveryTransfer.INSTANCE.mapToCommonUpstream(upstream));
            common.setStatus(true);
            return common;
        }).collect(Collectors.toList());
        if (!fencedRejectedUrls.isEmpty() && checkedFailedUrls.isEmpty() && changedUrls.isEmpty()) {
            return currentAlive;
        }
        String pluginName = pluginDO.getName();
        String handler = converterFactor.newInstance(pluginName).handler(selectorDO.getHandle(), currentAlive);
        selectorDO.setHandle(handler);
        selectorMapper.updateSelective(selectorDO);

        List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(
                selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId())));
        SelectorData selectorData = SelectorDO.transFrom(selectorDO, pluginName, conditionDataList);
        selectorData.setHandle(handler);

        // DataChangedEventDispatcher delays delivery until this transaction commits.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(selectorData)));

        // publish discovery change event.
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setUpstreamDataList(discoveryUpstreamDataList);
        discoverySyncData.setPluginName(pluginName);
        discoverySyncData.setSelectorId(selectorId);
        discoverySyncData.setSelectorName(selectorDO.getSelectorName());
        discoverySyncData.setNamespaceId(selectorDO.getNamespaceId());
        LOG.debug("UpstreamCacheManager update selectorId={}|json={}", selectorId, GsonUtils.getGson().toJson(discoverySyncData));
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData)));
        return currentAlive;
    }

    private List<DiscoveryUpstreamData> loadDiscoverySnapshot(final String selectorId,
                                                              final List<CommonUpstream> aliveList,
                                                              final boolean hasDiscoveryHandler) {
        List<DiscoveryUpstreamData> snapshot = discoveryUpstreamService.findBySelectorId(selectorId);
        if (CollectionUtils.isEmpty(snapshot) && !hasDiscoveryHandler) {
            // Legacy registrations without discovery records still use their checked upstreams.
            return aliveList.stream().map(DiscoveryTransfer.INSTANCE::mapToDiscoveryUpstreamData).collect(Collectors.toList());
        }
        return snapshot;
    }

    private List<DiscoveryUpstreamData> reconcileDiscoverySnapshot(final String selectorId,
                                                                   final List<CommonUpstream> aliveList,
                                                                   final Set<String> checkedFailedUrls,
                                                                   final Set<String> fencedRejectedUrls,
                                                                   final boolean hasDiscoveryHandler,
                                                                   final List<DiscoveryUpstreamData> snapshot,
                                                                   final Set<String> changedUrls) {
        Set<String> aliveUrls = aliveList.stream().map(CommonUpstream::getUpstreamUrl).collect(Collectors.toSet());
        for (DiscoveryUpstreamData upstream : snapshot) {
            if (aliveUrls.contains(upstream.getUrl()) && !fencedRejectedUrls.contains(upstream.getUrl())
                    && updateDiscoveryStatus(selectorId, upstream, aliveList, fencedRejectedUrls)) {
                changedUrls.add(upstream.getUrl());
            }
        }
        List<DiscoveryUpstreamData> finalSnapshot = hasDiscoveryHandler && TransactionSynchronizationManager.isActualTransactionActive()
                ? discoveryUpstreamService.findBySelectorId(selectorId)
                : snapshot;
        return finalSnapshot.stream()
                .filter(upstream -> upstream.getStatus() == 0 && !checkedFailedUrls.contains(upstream.getUrl()))
                .collect(Collectors.toList());
    }

    private boolean updateDiscoveryStatus(final String selectorId, final DiscoveryUpstreamData upstream,
                                          final List<CommonUpstream> aliveList,
                                          final Set<String> fencedRejectedUrls) {
        LOG.info("change alive selectorId={}|url={}", selectorId, upstream.getUrl());
        boolean statusChanged = upstream.getStatus() != 0;
        if (discoveryUpstreamService.tryChangeStatusBySelectorIdAndUrl(selectorId, upstream.getUrl(), Boolean.TRUE)) {
            upstream.setStatus(NumberUtils.INTEGER_ZERO);
            return statusChanged;
        } else {
            LOG.debug("Skip protected upstream status update selectorId={}, url={}", selectorId, upstream.getUrl());
            fencedRejectedUrls.add(upstream.getUrl());
            restoreRejectedStatus(upstream, aliveList);
            return false;
        }
    }

    private SelectorDO selectSelectorForUpdate(final String selectorId) {
        if (TransactionSynchronizationManager.isActualTransactionActive()) {
            return selectorMapper.selectByIdForUpdate(selectorId);
        }
        return selectorMapper.selectById(selectorId);
    }

    private void restoreRejectedStatus(final DiscoveryUpstreamData discoveryUpstream,
                                       final List<CommonUpstream> aliveList) {
        boolean healthy = discoveryUpstream.getStatus() == 0;
        aliveList.stream().filter(upstream -> Objects.equals(discoveryUpstream.getUrl(), upstream.getUpstreamUrl()))
                .forEach(upstream -> upstream.setStatus(healthy));
    }

    private boolean unwrapFencingException(final Throwable throwable) {
        Throwable current = throwable;
        while (Objects.nonNull(current)) {
            if (current instanceof RegistrationFencingException) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }

    /**
     * fetch upstream data from db.
     */
    public void fetchUpstreamData() {
        final List<PluginDO> pluginDOList = pluginMapper.selectByNames(PluginEnum.getUpstreamNames());
        if (CollectionUtils.isEmpty(pluginDOList)) {
            return;
        }
        Map<String, String> pluginMap = pluginDOList.stream().filter(Objects::nonNull)
                .collect(Collectors.toMap(PluginDO::getId, PluginDO::getName, (value1, value2) -> value1));
        final List<SelectorDO> selectorDOList = selectorMapper.findByPluginIds(new ArrayList<>(pluginMap.keySet()));
        long currentTimeMillis = System.currentTimeMillis();
        Optional.ofNullable(selectorDOList).orElseGet(ArrayList::new).stream()
                .filter(Objects::nonNull)
                .forEach(selectorDO -> {
                    String name = pluginMap.get(selectorDO.getPluginId());
                    List<CommonUpstream> commonUpstreams = new CopyOnWriteArrayList<>();
                    discoveryUpstreamService.findBySelectorId(selectorDO.getId()).stream()
                            .map(DiscoveryTransfer.INSTANCE::mapToCommonUpstream)
                            .forEach(commonUpstreams::add);
                    String handle = selectorDO.getHandle();
                    if (StringUtils.isNotEmpty(handle)) {
                        commonUpstreams.addAll(converterFactor.newInstance(name).convertUpstream(handle)
                                .stream().filter(upstream -> upstream.isStatus() || upstream.getTimestamp() > currentTimeMillis - TimeUnit.SECONDS.toMillis(zombieRemovalTimes))
                                .collect(Collectors.toList()));
                    }
                    if (CollectionUtils.isNotEmpty(commonUpstreams)) {
                        replace(selectorDO.getId(), commonUpstreams);
                        PENDING_SYNC.add(NumberUtils.INTEGER_ZERO);
                    }
                });
    }

    /**
     * listen {@link DiscoveryStreamUpdatedEvent} add data permission.
     *
     * @param event event
     */
    @EventListener(DiscoveryStreamUpdatedEvent.class)
    public void onDiscoveryUpstreamUpdated(final DiscoveryStreamUpdatedEvent event) {
        DiscoverySyncData discoverySyncData = event.getDiscoverySyncData();
        LOG.info("onDiscoveryUpstreamUpdated plugin={}|list={}", discoverySyncData.getPluginName(), discoverySyncData.getUpstreamDataList());
        if (PluginEnum.DIVIDE.getName().equals(discoverySyncData.getPluginName())) {
            List<DiscoveryUpstreamData> upstreamDataList = discoverySyncData.getUpstreamDataList();
            List<CommonUpstream> collect = upstreamDataList.stream().map(upstream -> {
                CommonUpstream common = DiscoveryTransfer.INSTANCE.mapToCommonUpstream(upstream);
                common.setStatus(upstream.getStatus() == 0);
                return common;
            }).collect(Collectors.toList());
            List<CommonUpstream> commonUpstreams = CommonUpstreamUtils.convertCommonUpstreamList(collect);
            LOG.info("UpstreamCacheManager replace selectorId={}|json={}", discoverySyncData.getSelectorId(), GsonUtils.getGson().toJson(commonUpstreams));
            replace(discoverySyncData.getSelectorId(), commonUpstreams);
        }
    }

    /**
     * get the zombie removal time value.
     *
     * @return zombie removal time value
     */
    public static int getZombieRemovalTimes() {
        return zombieRemovalTimes;
    }
}
