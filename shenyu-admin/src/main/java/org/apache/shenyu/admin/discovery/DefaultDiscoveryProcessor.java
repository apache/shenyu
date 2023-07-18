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
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * DefaultDiscoveryProcessor.
 */
public class DefaultDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    private static final String DEFAULT_LISTENER_NODE = "/shenyu/discovery";

    private static final Logger LOG = LoggerFactory.getLogger(DefaultDiscoveryProcessor.class);

    private final Map<String, ShenyuDiscoveryService> discoveryServiceCache;

    private final Map<String, Set<String>> dataChangedEventListenerCache;

    private ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryHandlerMapper discoveryHandlerMapper;

    private final ProxySelectorMapper proxySelectorMapper;

    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     */
    public DefaultDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                     final DiscoveryHandlerMapper discoveryHandlerMapper,
                                     final ProxySelectorMapper proxySelectorMapper) {
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryServiceCache = new ConcurrentHashMap<>();
        this.discoveryHandlerMapper = discoveryHandlerMapper;
        this.dataChangedEventListenerCache = new ConcurrentHashMap<>();
        this.proxySelectorMapper = proxySelectorMapper;
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

    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        String key = buildProxySelectorKey(discoveryHandlerDTO.getListenerNode());
        if (Objects.isNull(shenyuDiscoveryService)) {
            throw new ShenyuAdminException(String.format("before start ProxySelector you need init DiscoveryId=%s", discoveryHandlerDTO.getDiscoveryId()));
        }
        if (!shenyuDiscoveryService.exits(key)) {
            throw new ShenyuAdminException(String.format("shenyu discovery start watcher need you has this key %s in Discovery", key));
        }
        Set<String> cacheKey = dataChangedEventListenerCache.get(discoveryHandlerDTO.getDiscoveryId());
        if (Objects.nonNull(cacheKey) && cacheKey.contains(key)) {
            throw new ShenyuAdminException(String.format("shenyu discovery has watcher key = %s", key));
        }
        shenyuDiscoveryService.watcher(key, getDiscoveryDataChangedEventListener(discoveryHandlerDTO, proxySelectorDTO));
        cacheKey.add(key);
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
        LOG.info("shenyu discovery shutdown [{}] discovery", discoveryDO.getName());
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
        Set<String> cacheKey = dataChangedEventListenerCache.get(discoveryHandlerDTO.getDiscoveryId());
        cacheKey.remove(key);
        shenyuDiscoveryService.unWatcher(key);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void changeUpstream(final ProxySelectorDTO proxySelectorDTO, final List<DiscoveryUpstreamDTO> upstreamDTOS) {
        throw new NotImplementedException("shenyu discovery local mode do nothing in changeUpstream");
    }

    @Override
    public void fetchAll(final String discoveryHandlerId) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        String discoveryId = discoveryHandlerDO.getDiscoveryId();
        if (discoveryServiceCache.containsKey(discoveryId)) {
            ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryId);
            List<String> childData = shenyuDiscoveryService.getRegisterData(buildProxySelectorKey(discoveryHandlerDO.getListenerNode()));
            List<DiscoveryUpstreamData> discoveryUpstreamDataList = childData.stream().map(s -> GsonUtils.getGson().fromJson(s, DiscoveryUpstreamData.class))
                    .collect(Collectors.toList());
            Set<String> urlList = discoveryUpstreamDataList.stream().map(DiscoveryUpstreamData::getUrl).collect(Collectors.toSet());
            List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerId);
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
                    discoveryUpstreamDO.setDiscoveryHandlerId(discoveryHandlerId);
                    discoveryUpstreamDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
                    discoveryUpstreamDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                    discoveryUpstreamMapper.insert(discoveryUpstreamDO);
                }
            }

            ProxySelectorDO proxySelectorDO = proxySelectorMapper.selectByHandlerId(discoveryHandlerId);
            DiscoverySyncData discoverySyncData = new DiscoverySyncData();
            discoverySyncData.setSelectorId(proxySelectorDO.getId());
            discoverySyncData.setSelectorName(proxySelectorDO.getName());
            discoverySyncData.setPluginName(proxySelectorDO.getPluginName());
            discoverySyncData.setUpstreamDataList(discoveryUpstreamDataList);
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
    private String buildProxySelectorKey(final String listenerNode) {
        return StringUtils.isNotBlank(listenerNode) ? listenerNode : DEFAULT_LISTENER_NODE;
    }

    /**
     * getDiscoveryDataChangedEventListener.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     * @return DataChangedEventListener
     */
    private DataChangedEventListener getDiscoveryDataChangedEventListener(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        final Map<String, String> customMap = GsonUtils.getInstance().toObjectMap(discoveryHandlerDTO.getHandler(), String.class);
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setPluginName(proxySelectorDTO.getPluginName());
        discoverySyncData.setSelectorName(proxySelectorDTO.getName());
        discoverySyncData.setSelectorId(proxySelectorDTO.getId());
        return new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper,
                new CustomDiscoveryUpstreamParser(customMap), discoveryHandlerDTO.getId(), discoverySyncData);
    }

    @Override
    public void setApplicationEventPublisher(final ApplicationEventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }

}
