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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.event.discovery.DiscoveryStreamUpdatedEvent;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.apache.shenyu.admin.model.enums.EventTypeEnum.LOCAL_DISCOVERY_UPSTREAM_UPDATE;

/**
 * LocalDiscoveryProcessor.
 */
public class LocalDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    private static final Logger LOG = LoggerFactory.getLogger(LocalDiscoveryProcessor.class);

    private ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    public LocalDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
    }

    @Override
    public void createDiscovery(final DiscoveryDO discoveryDO) {
        LOG.info("shenyu discovery local mode do nothing in createDiscovery");
    }

    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void removeDiscovery(final DiscoveryDO discoveryDO) {
        LOG.info("shenyu discovery local mode do nothing in removeDiscovery");
    }

    @Override
    public void removeProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void removeSelectorUpstream(final ProxySelectorDTO proxySelectorDTO) {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setPluginName(proxySelectorDTO.getPluginName());
        discoverySyncData.setSelectorId(proxySelectorDTO.getId());
        discoverySyncData.setSelectorName(proxySelectorDTO.getName());
        discoverySyncData.setNamespaceId(proxySelectorDTO.getNamespaceId());
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.DELETE, Collections.singletonList(discoverySyncData));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void changeUpstream(final ProxySelectorDTO proxySelectorDTO, final List<DiscoveryUpstreamDTO> upstreamDTOS) {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setPluginName(proxySelectorDTO.getPluginName());
        discoverySyncData.setSelectorId(proxySelectorDTO.getId());
        discoverySyncData.setSelectorName(proxySelectorDTO.getName());
        List<DiscoveryUpstreamData> upstreamDataList = upstreamDTOS.stream().map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
        discoverySyncData.setUpstreamDataList(upstreamDataList);
        discoverySyncData.setNamespaceId(proxySelectorDTO.getNamespaceId());
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
        eventPublisher.publishEvent(dataChangedEvent);
        DiscoveryStreamUpdatedEvent discoveryStreamUpdatedEvent = new DiscoveryStreamUpdatedEvent(discoverySyncData, LOCAL_DISCOVERY_UPSTREAM_UPDATE);
        eventPublisher.publishEvent(discoveryStreamUpdatedEvent);
    }

    @Override
    public void setApplicationEventPublisher(final ApplicationEventPublisher applicationEventPublisher) {
        this.eventPublisher = applicationEventPublisher;
    }

    @Override
    public void fetchAll(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerDTO.getId());
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setPluginName(proxySelectorDTO.getPluginName());
        discoverySyncData.setSelectorId(proxySelectorDTO.getId());
        discoverySyncData.setSelectorName(proxySelectorDTO.getName());
        List<DiscoveryUpstreamData> upstreamDataList = discoveryUpstreamDOS.stream().map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
        discoverySyncData.setUpstreamDataList(upstreamDataList);
        discoverySyncData.setNamespaceId(proxySelectorDTO.getNamespaceId());
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
        eventPublisher.publishEvent(dataChangedEvent);
    }

}
