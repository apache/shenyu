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
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
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
public class GrpcDiscoveryUpstreamDataHandler implements DiscoveryUpstreamDataHandler {
    
    private static final Logger LOG = LoggerFactory.getLogger(GrpcDiscoveryUpstreamDataHandler.class);

    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        if (Objects.isNull(discoverySyncData) || Objects.isNull(discoverySyncData.getSelectorId())) {
            return;
        }
        LOG.info("discovery grpc upstream data:{}", JsonUtils.toJson(discoverySyncData));
        final String selectorId = discoverySyncData.getSelectorId();
        final List<GrpcUpstream> upstreams = convertUpstreamList(discoverySyncData.getUpstreamDataList());
        final List<GrpcUpstream> grayUpstreamList = upstreams.stream().filter(GrpcUpstream::isGray).toList();
        if (!grayUpstreamList.isEmpty()) {
            ApplicationConfigCache.getInstance().handlerUpstream(discoverySyncData.getSelectorId(), grayUpstreamList);
        } else {
            ApplicationConfigCache.getInstance().handlerUpstream(discoverySyncData.getSelectorId(), upstreams);
        }
        GrpcClientCache.initGrpcClient(selectorId);
    }

    private List<GrpcUpstream> convertUpstreamList(final List<DiscoveryUpstreamData> upstreamList) {
        if (ObjectUtils.isEmpty(upstreamList)) {
            return Collections.emptyList();
        }
        return upstreamList.stream().map(u -> GrpcUpstream.builder()
                .protocol(u.getProtocol())
                .upstreamUrl(u.getUrl())
                .weight(u.getWeight())
                .status(0 == u.getStatus())
                .timestamp(Optional.ofNullable(u.getDateCreated()).map(Timestamp::getTime).orElse(System.currentTimeMillis()))
                .build()).collect(Collectors.toList());
    }

    @Override
    public String pluginName() {
        return PluginEnum.GRPC.getName();
    }
}
