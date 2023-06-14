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
import org.apache.shenyu.admin.discovery.parse.KeyValueParser;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * DiscoveryHandler.
 */
public class DiscoveryDataChangedEventSyncListener implements DataChangedEventListener {

    private static final Logger LOGGER = LoggerFactory.getLogger(DiscoveryDataChangedEventSyncListener.class);

    private final KeyValueParser keyValueParser;

    private final ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final Boolean needPersistence;

    public DiscoveryDataChangedEventSyncListener(final ApplicationEventPublisher eventPublisher,
                                                 final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                                 final KeyValueParser keyValueParser,
                                                 final Boolean needPersistence) {
        this.eventPublisher = eventPublisher;
        this.keyValueParser = keyValueParser;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.needPersistence = needPersistence;
    }

    @Override
    public void onChange(final DataChangedEvent event) {
        DataChangedEvent.Event currentEvent = event.getEvent();
        if (DataChangedEvent.Event.IGNORED.equals(currentEvent)) {
            return;
        }
        DiscoverySyncData discoverySyncData = buildProxySelectorData(event.getKey(), event.getValue());
        org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = null;
        List<DiscoveryUpstreamData> upstreamDataList = discoverySyncData.getUpstreamDataList();
        if (needPersistence) {
            if (CollectionUtils.isEmpty(upstreamDataList)) {
                LOGGER.warn("shenyu proxySelectorData#discoveryUpstreamList is empty");
                return;
            }
            switch (currentEvent) {
                case ADDED:
                    upstreamDataList.forEach(d -> {
                        d.setId(UUIDUtils.getInstance().generateShortUuid());
                        d.setDateCreated(new Timestamp(System.currentTimeMillis()));
                        d.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                        discoveryUpstreamMapper.insert(DiscoveryTransfer.INSTANCE.mapToDo(d));
                        LOGGER.info("shenyu [DiscoveryDataChangedEventSyncListener] ADDED Upstream {}", d.getUrl());
                    });
                    fillFullyDiscoverySyncData(discoverySyncData);
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, Collections.singletonList(discoverySyncData));
                    break;
                case UPDATED:
                    upstreamDataList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryTransfer.INSTANCE.mapToDo(d);
                        discoveryUpstreamMapper.update(discoveryUpstreamDO);
                        LOGGER.info("shenyu [DiscoveryDataChangedEventSyncListener] UPDATE Upstream {}", discoveryUpstreamDO.getUrl());
                    });
                    fillFullyDiscoverySyncData(discoverySyncData);
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
                    break;
                case DELETED:
                    if (CollectionUtils.isNotEmpty(upstreamDataList)) {
                        upstreamDataList.forEach(up -> {
                            discoveryUpstreamMapper.deleteByUrl(up.getUrl());
                            LOGGER.info("shenyu [DiscoveryDataChangedEventSyncListener] DELETE Upstream {}", up.getUrl());
                        });
                    }
                    fillFullyDiscoverySyncData(discoverySyncData);
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
                    break;
                default:
                    break;
            }
        }
        if (Objects.nonNull(dataChangedEvent)) {
            eventPublisher.publishEvent(dataChangedEvent);
        }
    }

    private void fillFullyDiscoverySyncData(final DiscoverySyncData discoverySyncData) {
        ProxySelectorData proxySelectorData = discoverySyncData.getProxySelectorData();
        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByProxySelectorId(proxySelectorData.getId());
        List<DiscoveryUpstreamData> collect = discoveryUpstreamDOS.stream().map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
        discoverySyncData.setUpstreamDataList(collect);
    }

    private DiscoverySyncData buildProxySelectorData(final String key, final String value) {
        List<DiscoveryUpstreamData> discoveryUpstreamDTOS = keyValueParser.parseValue(value);
        ProxySelectorData proxySelectorData = keyValueParser.parseKey(key);
        String[] split = key.split("/");
        String discoveryHandleId = split[split.length - 2];
        discoveryUpstreamDTOS.forEach(s -> s.setDiscoveryHandlerId(discoveryHandleId));
        DiscoverySyncData data = new DiscoverySyncData();
        data.setUpstreamDataList(discoveryUpstreamDTOS);
        data.setProxySelectorData(proxySelectorData);
        return data;
    }

}
