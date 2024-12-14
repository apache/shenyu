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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.discovery.parse.CustomDiscoveryUpstreamParser;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.Properties;
import java.util.HashSet;
import java.util.Collections;

public abstract class AbstractDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    protected static final String DEFAULT_LISTENER_NODE = "/shenyu/discovery";

    protected static final Logger LOG = LoggerFactory.getLogger(DefaultDiscoveryProcessor.class);

    private final Map<String, ShenyuDiscoveryService> discoveryServiceCache;

    private final Map<String, Set<String>> dataChangedEventListenerCache;

    private ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     */
    public AbstractDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryServiceCache = new ConcurrentHashMap<>();
        this.dataChangedEventListenerCache = new ConcurrentHashMap<>();
    }

    @Override
    public void createDiscovery(final DiscoveryDO discoveryDO) {
        if (discoveryServiceCache.containsKey(discoveryDO.getId())) {
            LOG.info("shenyu DiscoveryProcessor {} discovery has been init", discoveryDO.getId());
            return;
        }
        String type = discoveryDO.getType();
        String props = discoveryDO.getProps();
        Properties properties = GsonUtils.getGson().fromJson(props, Properties.class);
        DiscoveryConfig discoveryConfig = new DiscoveryConfig();
        discoveryConfig.setType(type);
        discoveryConfig.setProps(properties);
        discoveryConfig.setServerList(discoveryDO.getServerList());
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        discoveryService.init(discoveryConfig);
        discoveryServiceCache.put(discoveryDO.getId(), discoveryService);
        dataChangedEventListenerCache.put(discoveryDO.getId(), new HashSet<>());
    }


    /**
     * removeDiscovery by ShenyuDiscoveryService#shutdown .
     *
     * @param discoveryDO discoveryDO
     */
    @Override
    public void removeDiscovery(final DiscoveryDO discoveryDO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.remove(discoveryDO.getId());
        if (shenyuDiscoveryService == null) {
            return;
        }
        if (discoveryServiceCache.values().stream().noneMatch(p -> p.equals(shenyuDiscoveryService))) {
            shenyuDiscoveryService.shutdown();
            LOG.info("shenyu discovery shutdown [{}] discovery", discoveryDO.getName());
        }
    }

    /**
     * removeProxySelector.
     *
     * @param proxySelectorDTO proxySelectorDTO
     */
    @Override
    public void removeProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        String key = buildProxySelectorKey(discoveryHandlerDTO.getListenerNode());
        Optional.ofNullable(dataChangedEventListenerCache.get(discoveryHandlerDTO.getDiscoveryId())).ifPresent(cacheKey -> {
            cacheKey.remove(key);
            shenyuDiscoveryService.unwatch(key);
            DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE,
                    Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
            eventPublisher.publishEvent(dataChangedEvent);
        });
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
        discoverySyncData.setNamespaceId(proxySelectorDTO.getNamespaceId());
        List<DiscoveryUpstreamData> upstreamDataList = upstreamDTOS.stream().map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
        discoverySyncData.setUpstreamDataList(upstreamDataList);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void fetchAll(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        String discoveryId = discoveryHandlerDTO.getDiscoveryId();
        if (discoveryServiceCache.containsKey(discoveryId)) {
            ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryId);
            List<String> childData = shenyuDiscoveryService.getRegisterData(buildProxySelectorKey(discoveryHandlerDTO.getListenerNode()));
            List<DiscoveryUpstreamData> discoveryUpstreamDataList = childData.stream().map(s -> GsonUtils.getGson().fromJson(s, DiscoveryUpstreamData.class))
                    .collect(Collectors.toList());
            Set<String> urlList = discoveryUpstreamDataList.stream().map(DiscoveryUpstreamData::getUrl).collect(Collectors.toSet());
            List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerDTO.getId());
            Set<String> dbUrlList = discoveryUpstreamDOS.stream().map(DiscoveryUpstreamDO::getUrl).collect(Collectors.toSet());
            List<String> deleteIds = new ArrayList<>();
            for (DiscoveryUpstreamDO discoveryUpstreamDO : discoveryUpstreamDOS) {
                if (!urlList.contains(discoveryUpstreamDO.getUrl())) {
                    deleteIds.add(discoveryUpstreamDO.getId());
                }
            }
            if (!deleteIds.isEmpty()) {
                discoveryUpstreamMapper.deleteByIds(deleteIds);
            }
            for (DiscoveryUpstreamData currDiscoveryUpstreamDate : discoveryUpstreamDataList) {
                if (!dbUrlList.contains(currDiscoveryUpstreamDate.getUrl())) {
                    DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryTransfer.INSTANCE.mapToDo(currDiscoveryUpstreamDate);
                    discoveryUpstreamDO.setId(UUIDUtils.getInstance().generateShortUuid());
                    discoveryUpstreamDO.setDiscoveryHandlerId(discoveryHandlerDTO.getId());
                    discoveryUpstreamDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
                    discoveryUpstreamDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                    discoveryUpstreamMapper.insert(discoveryUpstreamDO);
                }
            }
            DiscoverySyncData discoverySyncData = new DiscoverySyncData();
            discoverySyncData.setSelectorId(proxySelectorDTO.getId());
            discoverySyncData.setSelectorName(proxySelectorDTO.getName());
            discoverySyncData.setPluginName(proxySelectorDTO.getPluginName());
            discoverySyncData.setUpstreamDataList(discoveryUpstreamDataList);
            discoverySyncData.setNamespaceId(proxySelectorDTO.getNamespaceId());
            DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
            eventPublisher.publishEvent(dataChangedEvent);
        }
    }

    /**
     * buildProxySelectorKey.
     *
     * @param listenerNode listenerNode
     * @return key
     */
    protected String buildProxySelectorKey(final String listenerNode) {
        return StringUtils.isNotBlank(listenerNode) ? listenerNode : DEFAULT_LISTENER_NODE;
    }

    /**
     * getDiscoveryDataChangedEventListener.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     * @return DataChangedEventListener
     */
    public DataChangedEventListener getDiscoveryDataChangedEventListener(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        final Map<String, String> customMap = GsonUtils.getInstance().toObjectMap(discoveryHandlerDTO.getHandler(), String.class);
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setPluginName(proxySelectorDTO.getPluginName());
        discoverySyncData.setSelectorName(proxySelectorDTO.getName());
        discoverySyncData.setSelectorId(proxySelectorDTO.getId());
        discoverySyncData.setNamespaceId(proxySelectorDTO.getNamespaceId());
        return new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper,
                new CustomDiscoveryUpstreamParser(customMap), discoveryHandlerDTO.getId(), discoverySyncData);
    }

    @Override
    public void setApplicationEventPublisher(final ApplicationEventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }

    /**
     * getShenyuDiscoveryService.
     *
     * @param discoveryId discoveryId
     * @return ShenyuDiscoveryService
     */
    public ShenyuDiscoveryService getShenyuDiscoveryService(final String discoveryId) {
        return discoveryServiceCache.get(discoveryId);
    }

    /**
     * getCacheKey.
     *
     * @param discoveryId discoveryId
     * @return set
     */
    public Set<String> getCacheKey(final String discoveryId) {
        return dataChangedEventListenerCache.get(discoveryId);
    }

    /**
     * publishEvent.
     *
     * @param dataChangedEvent dataChangedEvent
     */
    public void publishEvent(final DataChangedEvent dataChangedEvent) {
        this.eventPublisher.publishEvent(dataChangedEvent);
    }

}
