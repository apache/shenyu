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

package org.apache.shenyu.plugin.grpc.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.base.handler.AbstractDiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.base.utils.UpstreamProps;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ObjectUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 *  GrpcDiscoveryUpstreamDataHandler.
 */
public class GrpcDiscoveryUpstreamDataHandler extends AbstractDiscoveryUpstreamDataHandler<GrpcUpstream> {

    private static final Logger LOG = LoggerFactory.getLogger(GrpcDiscoveryUpstreamDataHandler.class);

    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        if (Objects.nonNull(discoverySyncData) && Objects.nonNull(discoverySyncData.getSelectorId())) {
            LOG.info("discovery grpc upstream data:{}", JsonUtils.toJson(discoverySyncData));
        }
        super.handlerDiscoveryUpstreamData(discoverySyncData);
    }

    @Override
    public String pluginName() {
        return PluginEnum.GRPC.getName();
    }

    @Override
    protected List<GrpcUpstream> convertUpstreamList(final List<DiscoveryUpstreamData> upstreamList) {
        if (ObjectUtils.isEmpty(upstreamList)) {
            return Collections.emptyList();
        }
        return upstreamList.stream().map(u -> {
            UpstreamProps props = UpstreamProps.parse(u.getProps());
            return GrpcUpstream.builder()
                    .protocol(u.getProtocol())
                    .upstreamUrl(u.getUrl())
                    .weight(u.getWeight())
                    .status(0 == u.getStatus())
                    .timestamp(Optional.ofNullable(u.getDateCreated()).map(Timestamp::getTime).orElse(System.currentTimeMillis()))
                    .healthCheckEnabled(props.isHealthCheckEnabled())
                    .build();
        }).collect(Collectors.toList());
    }

    @Override
    protected boolean isGray(final GrpcUpstream upstream) {
        return upstream.isGray();
    }

    @Override
    protected void submitUpstreamData(final String selectorId, final List<GrpcUpstream> upstreamList) {
        ApplicationConfigCache.getInstance().handlerUpstream(selectorId, upstreamList);
    }

    @Override
    protected void afterSubmitUpstreamData(final String selectorId) {
        GrpcClientCache.initGrpcClient(selectorId);
    }

    @Override
    protected void removeUpstreamData(final String selectorId) {
        ApplicationConfigCache.getInstance().invalidate(selectorId);
    }
}
