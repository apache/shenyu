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

package org.apache.shenyu.admin.discovery;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.discovery.parse.CustomDiscoveryUpstreamParser;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * DefaultDiscoveryProcessor.
 */
public class DefaultDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    private static final String DEFAULT_LISTENER_NODE = "/shenyu/discovery";

    private static final Logger LOG = LoggerFactory.getLogger(DefaultDiscoveryProcessor.class);

    private final Map<String, ShenyuDiscoveryService> discoveryServiceCache;

    private ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;


    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     */
    public DefaultDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryServiceCache = new ConcurrentHashMap<>();
    }

    @Override
    public void createDiscovery(final DiscoveryDO discoveryDO) {
        if (discoveryServiceCache.containsKey(discoveryDO.getId())) {
            LOG.info("shenyu DiscoveryProcessor {} discovery has been init", discoveryDO.getId());
            return;
        }
        String type = discoveryDO.getType();
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        String props = discoveryDO.getProps();
        DiscoveryConfig discoveryConfig = GsonUtils.getGson().fromJson(props, DiscoveryConfig.class);
        discoveryConfig.setServerList(discoveryDO.getServerList());
        discoveryService.init(discoveryConfig);
        discoveryServiceCache.put(discoveryDO.getId(), discoveryService);
    }

    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        if (Objects.isNull(shenyuDiscoveryService)) {
            LOG.warn("before start ProxySelector you need init DiscoveryId={}", discoveryHandlerDTO.getDiscoveryId());
            return;
        }
        String key = buildProxySelectorKey(discoveryHandlerDTO);
        if (StringUtils.isEmpty(shenyuDiscoveryService.getData(key))) {
            LOG.info("shenyu discovery {} is empty need register it ", key);
            shenyuDiscoveryService.register(key, GsonUtils.getInstance().toJson(discoveryHandlerDTO) + "|" + GsonUtils.getInstance().toJson(proxySelectorDTO));
        }
        shenyuDiscoveryService.watcher(key, getDiscoveryDataChangedEventListener(proxySelectorDTO.getType(), discoveryHandlerDTO.getProps()));
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    /**
     * removeDiscovery by ShenyuDiscoveryService#shutdown .
     *
     * @param discoveryDO discoveryDO
     */
    @Override
    public void removeDiscovery(final DiscoveryDO discoveryDO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryDO.getId());
        shenyuDiscoveryService.shutdown();
    }

    /**
     * removeProxySelector.
     *
     * @param proxySelectorDTO proxySelectorDTO
     */
    @Override
    public void removeProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        String key = buildProxySelectorKey(discoveryHandlerDTO);
        shenyuDiscoveryService.unWatcher(key);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void changeUpstream(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO, final List<DiscoveryUpstreamDTO> upstreamDTOS) {
        throw new NotImplementedException("shenyu discovery local mode do nothing in changeUpstream");
    }

    /**
     * buildProxySelectorKey.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @return key
     */
    private String buildProxySelectorKey(final DiscoveryHandlerDTO discoveryHandlerDTO) {
        return StringUtils.isNotBlank(discoveryHandlerDTO.getListenerNode()) ? discoveryHandlerDTO.getListenerNode() : DEFAULT_LISTENER_NODE;
    }

    /**
     * getDiscoveryDataChangedEventListener.
     *
     * @param discoveryType discoveryType
     * @param customProps   customProps
     * @return DataChangedEventListener
     */
    private DataChangedEventListener getDiscoveryDataChangedEventListener(final String discoveryType, final String customProps) {
        Map<String, String> customMap = GsonUtils.getInstance().toObjectMap(customProps, String.class);
        return new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper,
                new CustomDiscoveryUpstreamParser(customMap), !DiscoveryMode.LOCAL.name().equalsIgnoreCase(discoveryType));
    }

    @Override
    public void setApplicationEventPublisher(final ApplicationEventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }
}
