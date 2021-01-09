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

package org.dromara.soul.admin.service.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import javax.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.UpstreamCheckUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * this is divide  http url upstream.
 *
 * @author xiaoyu
 */
@Slf4j
@Component
public class UpstreamCheckService {

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    @Value("${soul.upstream.check:true}")
    private boolean check;

    @Value("${soul.upstream.scheduledTime:10}")
    private int scheduledTime;

    private final SelectorService selectorService;

    private final SelectorMapper selectorMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final PluginMapper pluginMapper;
    
    /**
     * Instantiates a new Upstream check service.
     *
     * @param selectorService the selector service
     * @param selectorMapper  the selector mapper
     * @param eventPublisher  the event publisher
     * @param pluginMapper    the plugin mapper
     */
    @Autowired(required = false)
    public UpstreamCheckService(final SelectorService selectorService, final SelectorMapper selectorMapper,
                                final ApplicationEventPublisher eventPublisher, final PluginMapper pluginMapper) {
        this.selectorService = selectorService;
        this.selectorMapper = selectorMapper;
        this.eventPublisher = eventPublisher;
        this.pluginMapper = pluginMapper;
    }
    
    /**
     * Setup selectors of divide plugin.
     */
    @PostConstruct
    public void setup() {
        PluginDO pluginDO = pluginMapper.selectByName(PluginEnum.DIVIDE.getName());
        if (pluginDO != null) {
            List<SelectorDO> selectorDOList = selectorMapper.findByPluginId(pluginDO.getId());
            for (SelectorDO selectorDO : selectorDOList) {
                List<DivideUpstream> divideUpstreams = GsonUtils.getInstance().fromList(selectorDO.getHandle(), DivideUpstream.class);
                if (CollectionUtils.isNotEmpty(divideUpstreams)) {
                    UPSTREAM_MAP.put(selectorDO.getName(), divideUpstreams);
                }
            }
        }
        if (check) {
            new ScheduledThreadPoolExecutor(Runtime.getRuntime().availableProcessors(), SoulThreadFactory.create("scheduled-upstream-task", false))
                    .scheduleWithFixedDelay(this::scheduled, 10, scheduledTime, TimeUnit.SECONDS);
        }
    }
    
    /**
     * Remove by key.
     *
     * @param selectorName the selector name
     */
    public static void removeByKey(final String selectorName) {
        UPSTREAM_MAP.remove(selectorName);
    }
    
    /**
     * Submit.
     *
     * @param selectorName   the selector name
     * @param divideUpstream the divide upstream
     */
    public void submit(final String selectorName, final DivideUpstream divideUpstream) {
        if (UPSTREAM_MAP.containsKey(selectorName)) {
            UPSTREAM_MAP.get(selectorName).add(divideUpstream);
        } else {
            UPSTREAM_MAP.put(selectorName, Lists.newArrayList(divideUpstream));
        }
    }

    private void scheduled() {
        if (UPSTREAM_MAP.size() > 0) {
            UPSTREAM_MAP.forEach(this::check);
        }
    }

    private void check(final String selectorName, final List<DivideUpstream> upstreamList) {
        List<DivideUpstream> successList = Lists.newArrayListWithCapacity(upstreamList.size());
        for (DivideUpstream divideUpstream : upstreamList) {
            final boolean pass = UpstreamCheckUtils.checkUrl(divideUpstream.getUpstreamUrl());
            if (pass) {
                if (!divideUpstream.isStatus()) {
                    divideUpstream.setTimestamp(System.currentTimeMillis());
                    divideUpstream.setStatus(true);
                    log.info("UpstreamCacheManager check success the url: {}, host: {} ", divideUpstream.getUpstreamUrl(), divideUpstream.getUpstreamHost());
                }
                successList.add(divideUpstream);
            } else {
                divideUpstream.setStatus(false);
                log.error("check the url={} is fail ", divideUpstream.getUpstreamUrl());
            }
        }
        if (successList.size() == upstreamList.size()) {
            return;
        }
        if (successList.size() > 0) {
            UPSTREAM_MAP.put(selectorName, successList);
            updateSelectorHandler(selectorName, successList);
        } else {
            UPSTREAM_MAP.remove(selectorName);
            updateSelectorHandler(selectorName, null);
        }
    }

    private void updateSelectorHandler(final String selectorName, final List<DivideUpstream> upstreams) {
        SelectorDO selector = selectorService.findByName(selectorName);
        if (Objects.nonNull(selector)) {
            SelectorData selectorData = selectorService.buildByName(selectorName);
            if (upstreams == null) {
                selector.setHandle("");
                selectorData.setHandle("");
            } else {
                String handler = GsonUtils.getInstance().toJson(upstreams);
                selector.setHandle(handler);
                selectorData.setHandle(handler);
            }
            selectorMapper.updateSelective(selector);
            // publish change event.
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                    Collections.singletonList(selectorData)));
        }
    }
}
