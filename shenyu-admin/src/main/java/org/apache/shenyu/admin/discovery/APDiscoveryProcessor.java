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

import org.apache.shenyu.admin.discovery.listener.DataChangedEventListener;
import org.apache.shenyu.admin.discovery.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.event.ChangedEventListener;

import java.util.Collections;
import java.util.Objects;
import java.util.Set;

public class APDiscoveryProcessor extends AbstractDiscoveryProcessor {

    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     */
    public APDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        super(discoveryUpstreamMapper);
    }

    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuInstanceRegisterRepository shenyuDiscoveryService = getShenyuDiscoveryService(discoveryHandlerDTO.getDiscoveryId());
        String key = super.buildProxySelectorKey(discoveryHandlerDTO.getListenerNode());
        if (Objects.isNull(shenyuDiscoveryService)) {
            throw new ShenyuAdminException(String.format("before start ProxySelector you need init DiscoveryId=%s", discoveryHandlerDTO.getDiscoveryId()));
        }
        Set<String> cacheKey = getCacheKey(discoveryHandlerDTO.getDiscoveryId());
        if (Objects.nonNull(cacheKey) && cacheKey.contains(key)) {
            LOG.info("shenyu discovery has watcher key = {}", key);
            super.addDiscoverySyncDataListener(discoveryHandlerDTO, proxySelectorDTO);
            return;
        }
        LOG.info("shenyu discovery id {} watch key = {}", discoveryHandlerDTO.getId(), key);
        final DataChangedEventListener discoveryDataChangedEventListener = getDiscoveryDataChangedEventListener(discoveryHandlerDTO, proxySelectorDTO);
        shenyuDiscoveryService.watchInstances(key, (selectKey, selectValue, event) -> {
            LOG.info("shenyu discovery receive watch discovery id {} key = {}, value = {}, event = {}", discoveryHandlerDTO.getId(), selectKey, selectValue, event);
            if (event.equals(ChangedEventListener.Event.ADDED)) {
                discoveryDataChangedEventListener.onChange(new DiscoveryDataChangedEvent(selectKey, selectValue, DiscoveryDataChangedEvent.Event.ADDED));
            } else if (event.equals(ChangedEventListener.Event.UPDATED)) {
                discoveryDataChangedEventListener.onChange(new DiscoveryDataChangedEvent(selectKey, selectValue, DiscoveryDataChangedEvent.Event.UPDATED));
            } else if (event.equals(ChangedEventListener.Event.DELETED)) {
                discoveryDataChangedEventListener.onChange(new DiscoveryDataChangedEvent(selectKey, selectValue, DiscoveryDataChangedEvent.Event.DELETED));
            } else {
                discoveryDataChangedEventListener.onChange(new DiscoveryDataChangedEvent(selectKey, selectValue, DiscoveryDataChangedEvent.Event.IGNORED));
            }
        });
        cacheKey.add(key);
        super.addChangedEventListener(discoveryHandlerDTO.getDiscoveryId(), discoveryDataChangedEventListener);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        publishEvent(dataChangedEvent);
    }

}
