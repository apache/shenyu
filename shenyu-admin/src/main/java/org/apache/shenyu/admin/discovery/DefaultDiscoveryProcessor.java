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

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;

import java.util.Collections;
import java.util.Objects;
import java.util.Set;

/**
 * DefaultDiscoveryProcessor.
 */
public class DefaultDiscoveryProcessor extends AbstractDiscoveryProcessor {

    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     */
    public DefaultDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        super(discoveryUpstreamMapper);
    }

    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = getShenyuDiscoveryService(discoveryHandlerDTO.getDiscoveryId());
        String key = buildProxySelectorKey(discoveryHandlerDTO.getListenerNode());
        if (Objects.isNull(shenyuDiscoveryService)) {
            throw new ShenyuAdminException(String.format("before start ProxySelector you need init DiscoveryId=%s", discoveryHandlerDTO.getDiscoveryId()));
        }
        if (!shenyuDiscoveryService.exists(key)) {
            throw new ShenyuAdminException(String.format("shenyu discovery start watcher need you has this key %s in Discovery", key));
        }
        Set<String> cacheKey = getCacheKey(discoveryHandlerDTO.getDiscoveryId());
        if (Objects.nonNull(cacheKey) && cacheKey.contains(key)) {
            LOG.info("shenyu discovery has watcher key = {}", key);
            return;
        }
        shenyuDiscoveryService.watch(key, getDiscoveryDataChangedEventListener(discoveryHandlerDTO, proxySelectorDTO));
        cacheKey.add(key);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        publishEvent(dataChangedEvent);
    }

}
