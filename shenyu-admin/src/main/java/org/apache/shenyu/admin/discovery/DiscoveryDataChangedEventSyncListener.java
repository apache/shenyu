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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.discovery.listener.DataChangedEventListener;
import org.apache.shenyu.admin.discovery.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.admin.discovery.parse.KeyValueParser;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * DiscoveryHandler.
 */
public class DiscoveryDataChangedEventSyncListener implements DataChangedEventListener {

    private static final Logger LOG = LoggerFactory.getLogger(DiscoveryDataChangedEventSyncListener.class);

    private final KeyValueParser keyValueParser;

    private final ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final List<DiscoverySyncData> discoverySyncDataList;

    private final String discoveryId;

    public DiscoveryDataChangedEventSyncListener(final ApplicationEventPublisher eventPublisher,
                                                 final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                                 final KeyValueParser keyValueParser,
                                                 final DiscoverySyncData contextInfo,
                                                 final String discoveryId) {
        this.discoverySyncDataList = new ArrayList<>();
        this.eventPublisher = eventPublisher;
        this.keyValueParser = keyValueParser;
        this.discoveryId = discoveryId;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        discoverySyncDataList.add(contextInfo);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void onChange(final DiscoveryDataChangedEvent event) {
        DiscoveryDataChangedEvent.Event currentEvent = event.getEvent();
        if (DiscoveryDataChangedEvent.Event.IGNORED.equals(currentEvent)) {
            return;
        }
        discoverySyncDataList.forEach(discoverySyncData -> {
            LOG.info("DiscoveryDataChangedEventSyncListener watch discoveryId {} discoveryHandlerId {} selectorId {} event {}", discoveryId,
                    discoverySyncData.getDiscoveryHandlerId(),
                    discoverySyncData.getSelectorId(), event);
            syncData0(event, discoverySyncData, currentEvent);
        });
    }

    private void syncData0(final DiscoveryDataChangedEvent event, final DiscoverySyncData discoverySyncData, final DiscoveryDataChangedEvent.Event currentEvent) {
        final DiscoverySyncData syncData = buildProxySelectorData(discoverySyncData, event.getValue());
        final List<DiscoveryUpstreamData> upstreamDataList = syncData.getUpstreamDataList();
        if (CollectionUtils.isEmpty(upstreamDataList)) {
            LOG.warn("Discover EventSync proxySelectorData discoveryUpstreamList is empty syncData {}", syncData);
            return;
        }
        final String discoveryHandlerId = discoverySyncData.getDiscoveryHandlerId();
        switch (currentEvent) {
            case ADDED:
                upstreamDataList.forEach(d -> {
                    try {
                        DiscoveryUpstreamDO discoveryUpstreamDO = discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl(discoveryHandlerId, d.getUrl());
                        if (Objects.isNull(discoveryUpstreamDO)) {
                            d.setId(UUIDUtils.getInstance().generateShortUuid());
                            d.setDateCreated(new Timestamp(System.currentTimeMillis()));
                            d.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                            discoveryUpstreamMapper.insert(DiscoveryTransfer.INSTANCE.mapToDo(d));
                            LOG.info("[DiscoveryDataChangedEventSyncListener] ADDED Upstream {}", d.getUrl());
                        }
                    } catch (DuplicateKeyException ex) {
                        LOG.info("[DiscoveryDataChangedEventSyncListener]  Upstream {} exist", d.getUrl());
                    }
                });
                break;
            case UPDATED:
                upstreamDataList.stream().map(DiscoveryTransfer.INSTANCE::mapToDo).forEach(discoveryUpstreamDO -> {
                    discoveryUpstreamDO.setDiscoveryHandlerId(discoveryHandlerId);
                    int effect = discoveryUpstreamMapper.updateDiscoveryHandlerIdAndUrl(discoveryUpstreamDO);
                    LOG.info("[DiscoveryDataChangedEventSyncListener] UPDATE Upstream {}, effect = {} ", discoveryUpstreamDO.getUrl(), effect);
                });
                break;
            case DELETED:
                if (CollectionUtils.isNotEmpty(upstreamDataList)) {
                    upstreamDataList.forEach(up -> {
                        discoveryUpstreamMapper.deleteByUrl(discoveryHandlerId, up.getUrl());
                        LOG.info("[DiscoveryDataChangedEventSyncListener] DELETE Upstream {}", up.getUrl());
                    });
                }
                break;
            default:
                throw new IllegalStateException("DiscoveryDataChangedEventSyncListener find IllegalState");
        }
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(syncData));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    private DiscoverySyncData buildProxySelectorData(final DiscoverySyncData discoverySyncData, final String value) {
        List<DiscoveryUpstreamData> discoveryUpstreamDTOS = keyValueParser.parseValue(value);
        discoveryUpstreamDTOS.forEach(discoveryUpstreamData -> {
            if (StringUtils.isBlank(discoveryUpstreamData.getNamespaceId())) {
                discoveryUpstreamData.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
            }
        });
        discoveryUpstreamDTOS = discoveryUpstreamDTOS.stream()
                .filter(upstreamData -> discoverySyncData.getNamespaceId().equals(upstreamData.getNamespaceId()))
                .collect(Collectors.toList());
        discoveryUpstreamDTOS.forEach(discoveryUpstreamData -> {
            discoveryUpstreamData.setDiscoveryHandlerId(discoverySyncData.getDiscoveryHandlerId());
            if (StringUtils.isBlank(discoveryUpstreamData.getProtocol())) {
                discoveryUpstreamData.setProtocol(discoverySupportProtocol(discoverySyncData.getPluginName()));
            }
        });
        DiscoverySyncData data = new DiscoverySyncData();
        data.setUpstreamDataList(discoveryUpstreamDTOS);
        data.setSelectorId(discoverySyncData.getSelectorId());
        data.setSelectorName(discoverySyncData.getSelectorName());
        data.setPluginName(discoverySyncData.getPluginName());
        data.setNamespaceId(discoverySyncData.getNamespaceId());
        return data;
    }

    @Override
    public void addListener(final DiscoverySyncData discoverySyncData) {
        if (discoverySyncDataList.stream().noneMatch(data -> data.getSelectorId().equals(discoverySyncData.getSelectorId())
                && data.getDiscoveryHandlerId().equals(discoverySyncData.getDiscoveryHandlerId()))) {
            discoverySyncDataList.add(discoverySyncData);
            LOG.info("[DiscoveryDataChangedEventSyncListener] add discoverySyncData {}", discoverySyncData);
        }
    }

    private String discoverySupportProtocol(final String pluginName) {
        String pluginNameLower = pluginName.toLowerCase();
        switch (pluginNameLower) {
            case "divide":
            case "grpc":
                return "http://";
            case "websocket":
                return "ws://";
            default:
                return "";
        }
    }

}
