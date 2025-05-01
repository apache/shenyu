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
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import jakarta.annotation.PreDestroy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
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
import java.util.stream.Collectors;

/**
 * This is the upstream check service.
 */
@Component
public class UpstreamCheckService {

    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckService.class);

    private static final Map<String, List<CommonUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private static final Set<Integer> PENDING_SYNC = Sets.newConcurrentHashSet();

    private static final Set<ZombieUpstream> ZOMBIE_SET = Sets.newConcurrentHashSet();

    private static final String REGISTER_TYPE_HTTP = "http";

    private static int zombieRemovalTimes;

    private final int zombieCheckTimes;

    private final int scheduledTime;

    private final String registerType;

    private final boolean checked;

    private final Integer scheduledThreads;

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final PluginMapper pluginMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final SelectorHandleConverterFactor converterFactor;

    private final DiscoveryUpstreamService discoveryUpstreamService;

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
        this.scheduledThreads = Integer.parseInt(props.getProperty(Constants.ZOMBIE_CHECK_THREADS, Constants.ZOMBIE_CHECK_THREADS_VALUE));
        this.zombieCheckTimes = Integer.parseInt(props.getProperty(Constants.ZOMBIE_CHECK_TIMES, Constants.ZOMBIE_CHECK_TIMES_VALUE));
        this.scheduledTime = Integer.parseInt(props.getProperty(Constants.SCHEDULED_TIME, Constants.SCHEDULED_TIME_VALUE));
        this.registerType = shenyuRegisterCenterConfig.getRegisterType();
        zombieRemovalTimes = Integer.parseInt(props.getProperty(Constants.ZOMBIE_REMOVAL_TIMES, Constants.ZOMBIE_REMOVAL_TIMES_VALUE));
    }

    /**
     * Set up.
     */
    public void setup() {
        if (REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) && checked) {
            LOG.info("setup upstream check task");
            this.fetchUpstreamData();
            executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-upstream-task", false));
            scheduledFuture = executor.scheduleWithFixedDelay(this::scheduled, 10, scheduledTime, TimeUnit.SECONDS);

            ThreadFactory requestFactory = ShenyuThreadFactory.create("upstream-health-check-request", true);
            invokeExecutor = new ScheduledThreadPoolExecutor(this.scheduledThreads, requestFactory);
        }
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
        }
    }

    /**
     * Remove by key.
     *
     * @param selectorId the selector id
     */
    public static void removeByKey(final String selectorId) {
        UPSTREAM_MAP.remove(selectorId);
    }

    /**
     * Submit client health check.
     *
     * @param selectorId     the selector id
     * @param commonUpstream the common upstream
     */
    public void submit(final String selectorId, final CommonUpstream commonUpstream) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked) {
            return;
        }

        Optional.ofNullable(submitJust(selectorId, commonUpstream))
                .ifPresent(upstreams -> executor.execute(() -> updateHandler(selectorId, upstreams, upstreams)));
    }

    private List<CommonUpstream> submitJust(final String selectorId, final CommonUpstream commonUpstream) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked) {
            return null;
        }

        List<CommonUpstream> upstreams = MapUtils.computeIfAbsent(UPSTREAM_MAP, selectorId, k -> new CopyOnWriteArrayList<>());
        if (commonUpstream.isStatus()) {
            Optional<CommonUpstream> exists = upstreams.stream().filter(item -> StringUtils.isNotBlank(item.getUpstreamUrl())
                    && item.getUpstreamUrl().equals(commonUpstream.getUpstreamUrl())).findFirst();
            if (!exists.isPresent()) {
                upstreams.add(commonUpstream);
            } else {
                LOG.info("upstream host {} is exists.", commonUpstream.getUpstreamHost());
            }
            PENDING_SYNC.add(commonUpstream.hashCode());
        } else {
            upstreams.removeIf(item -> item.equals(commonUpstream));
            PENDING_SYNC.add(NumberUtils.INTEGER_ZERO);
        }
        return upstreams;
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
        UPSTREAM_MAP.put(selectorId, commonUpstreams);
    }

    private void scheduled() {
        try {
            doCheck();
            waitFinish();
        } catch (Exception e) {
            LOG.error("upstream scheduled check error -------- ", e);
        }
    }

    private void doCheck() {
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
        // wait all check success
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        // clear, for next time
        futures.clear();
    }

    private void checkZombie(final ZombieUpstream zombieUpstream) {
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> checkZombie0(zombieUpstream), invokeExecutor);
        futures.add(future);
    }

    private void checkZombie0(final ZombieUpstream zombieUpstream) {
        ZOMBIE_SET.remove(zombieUpstream);
        String selectorId = zombieUpstream.getSelectorId();
        CommonUpstream commonUpstream = zombieUpstream.getCommonUpstream();
        final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
        if (pass) {
            commonUpstream.setTimestamp(System.currentTimeMillis());
            commonUpstream.setStatus(true);
            LOG.info("UpstreamCacheManager check zombie upstream success the url: {}, host: {} ", commonUpstream.getUpstreamUrl(), commonUpstream.getUpstreamHost());
            List<CommonUpstream> old = ListUtils.unmodifiableList(UPSTREAM_MAP.getOrDefault(selectorId, Collections.emptyList()));
            // fix https://github.com/apache/shenyu/issues/5311
            this.submitJust(selectorId, commonUpstream);
            updateHandler(selectorId, old, UPSTREAM_MAP.get(selectorId));
        } else {
            LOG.error("check zombie upstream the url={} is fail", commonUpstream.getUpstreamUrl());
            if (zombieUpstream.getZombieCheckTimes() > NumberUtils.INTEGER_ZERO) {
                zombieUpstream.setZombieCheckTimes(zombieUpstream.getZombieCheckTimes() - NumberUtils.INTEGER_ONE);
                ZOMBIE_SET.add(zombieUpstream);
            }
        }
    }

    private void check(final String selectorId, final List<CommonUpstream> upstreamList) {
        final List<CompletableFuture<CommonUpstream>> checkFutures = new ArrayList<>(upstreamList.size());
        for (CommonUpstream commonUpstream : upstreamList) {
            checkFutures.add(CompletableFuture.supplyAsync(() -> {
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
                    commonUpstream.setStatus(false);
                    ZOMBIE_SET.add(ZombieUpstream.transform(commonUpstream, zombieCheckTimes, selectorId));
                    LOG.info("change unlive selectorId={}|url={}", selectorId, commonUpstream.getUpstreamUrl());
                    discoveryUpstreamService.changeStatusBySelectorIdAndUrl(selectorId, commonUpstream.getUpstreamUrl(), Boolean.FALSE);
                    LOG.error("check the url={} is fail ", commonUpstream.getUpstreamUrl());
                }
                return null;
            }, invokeExecutor).exceptionally(ex -> {
                LOG.error("An exception occurred during the check of url {}: ", commonUpstream.getUpstreamUrl(), ex);
                return null;
            }));
        }

        this.futures.add(CompletableFuture.runAsync(() -> {
            List<CommonUpstream> successList = checkFutures.stream()
                    .map(CompletableFuture::join)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            updateHandler(selectorId, upstreamList, successList);
        }));
    }

    private void updateHandler(final String selectorId, final List<CommonUpstream> upstreamList, final List<CommonUpstream> successList) {
        //No node changes, including zombie node resurrection and live node death
        if (successList.size() == upstreamList.size() && PENDING_SYNC.isEmpty()) {
            return;
        }
        removePendingSync(successList);
        if (!successList.isEmpty()) {
            UPSTREAM_MAP.put(selectorId, successList);
            updateSelectorHandler(selectorId, successList);
        } else {
            UPSTREAM_MAP.remove(selectorId);
            updateSelectorHandler(selectorId, new ArrayList<>());
        }
    }

    private void removePendingSync(final List<CommonUpstream> successList) {
        PENDING_SYNC.removeIf(NumberUtils.INTEGER_ZERO::equals);
        successList.forEach(commonUpstream -> PENDING_SYNC.remove(commonUpstream.hashCode()));
    }

    private void updateSelectorHandler(final String selectorId, final List<CommonUpstream> aliveList) {
        SelectorDO selectorDO = selectorMapper.selectById(selectorId);
        if (Objects.isNull(selectorDO)) {
            return;
        }

        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        if (Objects.isNull(pluginDO)) {
            return;
        }
        String pluginName = pluginDO.getName();
        String handler = converterFactor.newInstance(pluginName).handler(selectorDO.getHandle(), aliveList);
        selectorDO.setHandle(handler);
        selectorMapper.updateSelective(selectorDO);

        List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(
                selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId())));
        SelectorData selectorData = SelectorDO.transFrom(selectorDO, pluginName, conditionDataList);
        selectorData.setHandle(handler);

        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(selectorData)));

        // publish discovery change event.
        List<DiscoveryUpstreamData> discoveryUpstreamDataList = discoveryUpstreamService.findBySelectorId(selectorId);
        
        if (CollectionUtils.isEmpty(discoveryUpstreamDataList)) {
            discoveryUpstreamDataList = aliveList.stream().map(DiscoveryTransfer.INSTANCE::mapToDiscoveryUpstreamData).collect(Collectors.toList());
        }
        
        discoveryUpstreamDataList.removeIf(u -> {
            for (CommonUpstream alive : aliveList) {
                if (alive.getUpstreamUrl().equals(u.getUrl())) {
                    return false;
                }
            }
            return true;
        });
        // change live node status to TRUE
        discoveryUpstreamDataList.forEach(upstream -> {
            LOG.info("change alive selectorId={}|url={}", selectorId, upstream.getUrl());
            discoveryUpstreamService.changeStatusBySelectorIdAndUrl(selectorId, upstream.getUrl(), Boolean.TRUE);
        });
        
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setUpstreamDataList(discoveryUpstreamDataList);
        discoverySyncData.setPluginName(pluginName);
        discoverySyncData.setSelectorId(selectorId);
        discoverySyncData.setSelectorName(selectorDO.getName());
        discoverySyncData.setNamespaceId(selectorDO.getNamespaceId());
        LOG.debug("UpstreamCacheManager update selectorId={}|json={}", selectorId, GsonUtils.getGson().toJson(discoverySyncData));
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData)));
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
                    List<CommonUpstream> commonUpstreams = new LinkedList<>();
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
                        UPSTREAM_MAP.put(selectorDO.getId(), commonUpstreams);
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
            List<CommonUpstream> collect = upstreamDataList.stream().map(DiscoveryTransfer.INSTANCE::mapToCommonUpstream).collect(Collectors.toList());
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
