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
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
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
                handleAdded(upstreamDataList, discoveryHandlerId);
                break;
            case UPDATED:
                handleUpdated(upstreamDataList, discoveryHandlerId);
                break;
            case DELETED:
                handleDeleted(upstreamDataList, discoveryHandlerId);
                break;
            default:
                throw new IllegalStateException("DiscoveryDataChangedEventSyncListener find IllegalState");
        }
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(syncData));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    private void handleAdded(final List<DiscoveryUpstreamData> upstreamDataList, final String discoveryHandlerId) {
        upstreamDataList.forEach(d -> {
            try {
                String normalizedUrl = CommonUpstreamUtils.normalizeUrl(d.getUrl());
                DiscoveryUpstreamDO existing = discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl(discoveryHandlerId, normalizedUrl);
                if (Objects.isNull(existing)) {
                    existing = CommonUpstreamUtils.matchByHostAndPort(discoveryUpstreamMapper, discoveryHandlerId, normalizedUrl);
                }
                if (Objects.isNull(existing)) {
                    d.setUrl(normalizedUrl);
                    d.setId(UUIDUtils.getInstance().generateShortUuid());
                    d.setDateCreated(new Timestamp(System.currentTimeMillis()));
                    d.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                    discoveryUpstreamMapper.insert(DiscoveryTransfer.INSTANCE.mapToDo(d));
                    LOG.info("[DiscoveryDataChangedEventSyncListener] ADDED Upstream {}", normalizedUrl);
                } else if (!normalizedUrl.equals(existing.getUpstreamUrl())) {
                    existing.setUpstreamUrl(normalizedUrl);
                    discoveryUpstreamMapper.updateSelective(existing);
                    LOG.info("[DiscoveryDataChangedEventSyncListener] Migrated old URL to {}", normalizedUrl);
                }
            } catch (DuplicateKeyException ex) {
                LOG.info("[DiscoveryDataChangedEventSyncListener]  Upstream {} exist", d.getUrl());
            }
        });
    }

    private void handleUpdated(final List<DiscoveryUpstreamData> upstreamDataList, final String discoveryHandlerId) {
        upstreamDataList.stream().map(DiscoveryTransfer.INSTANCE::mapToDo).forEach(discoveryUpstreamDO -> {
            try {
                discoveryUpstreamDO.setDiscoveryHandlerId(discoveryHandlerId);
                discoveryUpstreamDO.setUpstreamUrl(CommonUpstreamUtils.normalizeUrl(discoveryUpstreamDO.getUpstreamUrl()));
                int effect = discoveryUpstreamMapper.updateDiscoveryHandlerIdAndUrl(discoveryUpstreamDO);
                if (effect == 0) {
                    DiscoveryUpstreamDO oldRecord = CommonUpstreamUtils.matchByHostAndPort(
                            discoveryUpstreamMapper, discoveryHandlerId, discoveryUpstreamDO.getUpstreamUrl());
                    if (Objects.nonNull(oldRecord)) {
                        oldRecord.setUpstreamUrl(discoveryUpstreamDO.getUpstreamUrl());
                        oldRecord.setProtocol(discoveryUpstreamDO.getProtocol());
                        oldRecord.setUpstreamStatus(discoveryUpstreamDO.getUpstreamStatus());
                        oldRecord.setWeight(discoveryUpstreamDO.getWeight());
                        oldRecord.setProps(discoveryUpstreamDO.getProps());
                        oldRecord.setDateUpdated(discoveryUpstreamDO.getDateUpdated());
                        discoveryUpstreamMapper.updateSelective(oldRecord);
                        effect = 1;
                        LOG.info("[DiscoveryDataChangedEventSyncListener] Migrated old URL and updated Upstream {}", discoveryUpstreamDO.getUpstreamUrl());
                    }
                }
                LOG.info("[DiscoveryDataChangedEventSyncListener] UPDATE Upstream {}, effect = {} ", discoveryUpstreamDO.getUpstreamUrl(), effect);
            } catch (Exception e) {
                LOG.error("[DiscoveryDataChangedEventSyncListener] UPDATE Upstream failed: {}", discoveryUpstreamDO.getUpstreamUrl(), e);
            }
        });
    }

    private void handleDeleted(final List<DiscoveryUpstreamData> upstreamDataList, final String discoveryHandlerId) {
        if (CollectionUtils.isEmpty(upstreamDataList)) {
            return;
        }
        upstreamDataList.forEach(up -> {
            try {
                String normalizedUrl = CommonUpstreamUtils.normalizeUrl(up.getUrl());
                int effect = discoveryUpstreamMapper.deleteByUrl(discoveryHandlerId, normalizedUrl);
                if (effect == 0) {
                    DiscoveryUpstreamDO oldRecord = CommonUpstreamUtils.matchByHostAndPort(
                            discoveryUpstreamMapper, discoveryHandlerId, normalizedUrl);
                    if (Objects.nonNull(oldRecord)) {
                        discoveryUpstreamMapper.deleteByIds(Collections.singletonList(oldRecord.getId()));
                        effect = 1;
                        LOG.info("[DiscoveryDataChangedEventSyncListener] DELETE Upstream by fallback match {}", normalizedUrl);
                    }
                }
                LOG.info("[DiscoveryDataChangedEventSyncListener] DELETE Upstream {}, effect = {}", normalizedUrl, effect);
            } catch (Exception e) {
                LOG.error("[DiscoveryDataChangedEventSyncListener] DELETE Upstream failed: {}", up.getUrl(), e);
            }
        });
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
