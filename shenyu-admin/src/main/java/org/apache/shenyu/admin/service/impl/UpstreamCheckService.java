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
import org.apache.shenyu.admin.model.event.selector.SelectorCreatedEvent;
import org.apache.shenyu.admin.model.event.selector.SelectorUpdatedEvent;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.transfer.ConditionTransfer;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.SelectorUtil;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.ZombieUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import javax.annotation.PreDestroy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
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

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final PluginMapper pluginMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final SelectorHandleConverterFactor converterFactor;

    private ScheduledThreadPoolExecutor executor;

    private ScheduledFuture<?> scheduledFuture;

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
                                final SelectorHandleConverterFactor converterFactor) {
        this.selectorMapper = selectorMapper;
        this.eventPublisher = eventPublisher;
        this.pluginMapper = pluginMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.converterFactor = converterFactor;
        Properties props = shenyuRegisterCenterConfig.getProps();
        this.checked = Boolean.parseBoolean(props.getProperty(Constants.IS_CHECKED, Constants.DEFAULT_CHECK_VALUE));
        this.zombieCheckTimes = Integer.parseInt(props.getProperty(Constants.ZOMBIE_CHECK_TIMES, Constants.ZOMBIE_CHECK_TIMES_VALUE));
        this.scheduledTime = Integer.parseInt(props.getProperty(Constants.SCHEDULED_TIME, Constants.SCHEDULED_TIME_VALUE));
        this.registerType = shenyuRegisterCenterConfig.getRegisterType();
        zombieRemovalTimes = Integer.parseInt(props.getProperty(Constants.ZOMBIE_REMOVAL_TIMES, Constants.ZOMBIE_REMOVAL_TIMES_VALUE));
        if (REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType)) {
            setup();
        }
    }

    /**
     * Set up.
     */
    public void setup() {
        if (checked) {
            this.fetchUpstreamData();
            executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-upstream-task", false));
            scheduledFuture = executor.scheduleWithFixedDelay(this::scheduled, 10, scheduledTime, TimeUnit.SECONDS);
        }
    }

    /**
     * Close relative resource on container destroy.
     */
    @PreDestroy
    public void close() {
        if (checked) {
            scheduledFuture.cancel(false);
            executor.shutdown();
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
     * Submit.
     *
     * @param selectorId     the selector id
     * @param commonUpstream the common upstream
     * @return whether this module handles
     */
    public boolean submit(final String selectorId, final CommonUpstream commonUpstream) {
        if (!REGISTER_TYPE_HTTP.equalsIgnoreCase(registerType) || !checked) {
            return false;
        }

        List<CommonUpstream> upstreams = UPSTREAM_MAP.computeIfAbsent(selectorId, k -> new CopyOnWriteArrayList<>());
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
        executor.execute(() -> updateHandler(selectorId, upstreams, upstreams));
        return true;
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
     * check https://github.com/apache/shenyu/issues/3484 for more details.
     *
     * @param selectorId     the selector id
     * @param commonUpstream the common upstream
     * @return whether this module handles
     */
    public boolean checkAndSubmit(final String selectorId, final CommonUpstream commonUpstream) {
        final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
        if (pass) {
            return submit(selectorId, commonUpstream);
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
            if (ZOMBIE_SET.size() > 0) {
                ZOMBIE_SET.parallelStream().forEach(this::checkZombie);
            }
            if (UPSTREAM_MAP.size() > 0) {
                UPSTREAM_MAP.forEach(this::check);
            }
        } catch (Exception e) {
            LOG.error("upstream scheduled check error -------- ", e);
        }
    }

    private void checkZombie(final ZombieUpstream zombieUpstream) {
        ZOMBIE_SET.remove(zombieUpstream);
        String selectorId = zombieUpstream.getSelectorId();
        CommonUpstream commonUpstream = zombieUpstream.getCommonUpstream();
        final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
        if (pass) {
            commonUpstream.setTimestamp(System.currentTimeMillis());
            commonUpstream.setStatus(true);
            LOG.info("UpstreamCacheManager check zombie upstream success the url: {}, host: {} ", commonUpstream.getUpstreamUrl(), commonUpstream.getUpstreamHost());
            List<CommonUpstream> old = ListUtils.unmodifiableList(UPSTREAM_MAP.getOrDefault(selectorId, Collections.emptyList()));
            this.submit(selectorId, commonUpstream);
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
        List<CommonUpstream> successList = Lists.newArrayListWithCapacity(upstreamList.size());
        for (CommonUpstream commonUpstream : upstreamList) {
            final boolean pass = UpstreamCheckUtils.checkUrl(commonUpstream.getUpstreamUrl());
            if (pass) {
                if (!commonUpstream.isStatus()) {
                    commonUpstream.setTimestamp(System.currentTimeMillis());
                    commonUpstream.setStatus(true);
                    LOG.info("UpstreamCacheManager check success the url: {}, host: {} ", commonUpstream.getUpstreamUrl(), commonUpstream.getUpstreamHost());
                }
                successList.add(commonUpstream);
            } else {
                commonUpstream.setStatus(false);
                ZOMBIE_SET.add(ZombieUpstream.transform(commonUpstream, zombieCheckTimes, selectorId));
                LOG.error("check the url={} is fail ", commonUpstream.getUpstreamUrl());
            }
        }
        updateHandler(selectorId, upstreamList, successList);
    }

    private void updateHandler(final String selectorId, final List<CommonUpstream> upstreamList, final List<CommonUpstream> successList) {
        //No node changes, including zombie node resurrection and live node death
        if (successList.size() == upstreamList.size() && PENDING_SYNC.size() == 0) {
            return;
        }
        removePendingSync(successList);
        if (successList.size() > 0) {
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
        String handler = converterFactor.newInstance(pluginDO.getName()).handler(selectorDO.getHandle(), aliveList);
        selectorDO.setHandle(handler);
        selectorMapper.updateSelective(selectorDO);

        List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(
                selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId())));
        SelectorData selectorData = SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList);
        selectorData.setHandle(handler);

        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(selectorData)));

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
                .filter(selectorDO -> Objects.nonNull(selectorDO) && StringUtils.isNotEmpty(selectorDO.getHandle()))
                .forEach(selectorDO -> {
                    String name = pluginMap.get(selectorDO.getPluginId());
                    List<CommonUpstream> commonUpstreams = converterFactor.newInstance(name).convertUpstream(selectorDO.getHandle())
                            .stream().filter(upstream -> upstream.isStatus() || upstream.getTimestamp() > currentTimeMillis - TimeUnit.SECONDS.toMillis(zombieRemovalTimes))
                            .collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(commonUpstreams)) {
                        UPSTREAM_MAP.put(selectorDO.getId(), commonUpstreams);
                        PENDING_SYNC.add(NumberUtils.INTEGER_ZERO);
                    }
                });
    }
    
    /**
     * listen {@link SelectorCreatedEvent} add data permission.
     *
     * @param event event
     */
    @EventListener(SelectorCreatedEvent.class)
    public void onSelectorCreated(final SelectorCreatedEvent event) {
        final PluginDO plugin = pluginMapper.selectById(event.getSelector().getPluginId());
        List<DivideUpstream> existDivideUpstreams = SelectorUtil.buildDivideUpstream(event.getSelector(), plugin.getName());
        if (CollectionUtils.isNotEmpty(existDivideUpstreams)) {
            replace(event.getSelector().getId(), CommonUpstreamUtils.convertCommonUpstreamList(existDivideUpstreams));
        }
    }
    
    /**
     * listen {@link SelectorCreatedEvent} add data permission.
     *
     * @param event event
     */
    @EventListener(SelectorUpdatedEvent.class)
    public void onSelectorUpdated(final SelectorUpdatedEvent event) {
        final PluginDO plugin = pluginMapper.selectById(event.getSelector().getPluginId());
        List<DivideUpstream> existDivideUpstreams = SelectorUtil.buildDivideUpstream(event.getSelector(), plugin.getName());
        if (CollectionUtils.isNotEmpty(existDivideUpstreams)) {
            replace(event.getSelector().getId(), CommonUpstreamUtils.convertCommonUpstreamList(existDivideUpstreams));
        }
    }

    /**
     * get the zombie removal time value.
     * @return zombie removal time value
     */
    public static int getZombieRemovalTimes() {
        return zombieRemovalTimes;
    }
}
