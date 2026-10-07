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

package org.apache.shenyu.plugin.websocket.handler;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.handler.AbstractDiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.base.utils.UpstreamProps;
import org.springframework.util.ObjectUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * upstreamList data change.
 */
public class WebSocketUpstreamDataHandler extends AbstractDiscoveryUpstreamDataHandler<Upstream> {

    @Override
    public String pluginName() {
        return PluginEnum.WEB_SOCKET.getName();
    }

    @Override
    protected List<Upstream> convertUpstreamList(final List<DiscoveryUpstreamData> upstreamList) {
        if (ObjectUtils.isEmpty(upstreamList)) {
            return Collections.emptyList();
        }
        return upstreamList.stream().map(u -> {
            UpstreamProps props = UpstreamProps.parse(u.getProps());
            return Upstream.builder()
                    .protocol(u.getProtocol())
                    .url(u.getUrl())
                    .weight(u.getWeight())
                    .warmup(props.getWarmup())
                    .healthCheckEnabled(props.isHealthCheckEnabled())
                    .status(0 == u.getStatus())
                    .timestamp(Optional.ofNullable(u.getDateCreated()).map(Timestamp::getTime).orElse(System.currentTimeMillis()))
                    .build();
        }).collect(Collectors.toList());
    }

    @Override
    protected boolean isGray(final Upstream upstream) {
        return upstream.isGray();
    }

    @Override
    protected void submitUpstreamData(final String selectorId, final List<Upstream> upstreamList) {
        UpstreamCacheManager.getInstance().submit(selectorId, upstreamList);
    }

    @Override
    protected void afterSubmitUpstreamData(final String selectorId) {
        MetaDataCache.getInstance().clean();
    }

    @Override
    protected void removeUpstreamData(final String selectorId) {
        UpstreamCacheManager.getInstance().removeByKey(selectorId);
    }
}
