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

package org.apache.shenyu.plugin.grpc.loadbalance.picker;

import io.grpc.LoadBalancer;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.GrpcRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.context.GrpcConstants;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannelCopy;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Shenyu picker.
 */
public class ShenyuPicker extends AbstractReadyPicker {

    private final RandomPicker randomPicker;

    public ShenyuPicker(final List<LoadBalancer.Subchannel> list) {
        super(list);
        randomPicker = new RandomPicker(list);
    }

    @Override
    protected SubChannelCopy pick(final List<SubChannelCopy> list) {

        String grpcRuleId = GrpcConstants.GRPC_RULE_ID.get();
        String selectorId = GrpcConstants.GRPC_SELECTOR_ID.get();
        String remoteAddressIp = GrpcConstants.GRPC_REMOTE_ADDRESS.get();
        final GrpcRuleHandle cacheRuleHandle = ApplicationConfigCache.getInstance().getCacheRuleHandle(grpcRuleId);
        List<GrpcUpstream> grpcUpstreams = ApplicationConfigCache.getInstance().getGrpcUpstreamListCache(selectorId);
        if (CollectionUtils.isNotEmpty(grpcUpstreams)) {
            Upstream upstream = LoadBalancerFactory.selector(convertUpstreamList(grpcUpstreams), cacheRuleHandle.getLoadBalance(), remoteAddressIp);
            if (StringUtils.isBlank(upstream.getUrl()) && StringUtils.isBlank(upstream.getGroup()) && StringUtils.isBlank(upstream.getVersion())) {
                return randomPicker.pick(list);
            }

            final List<SubChannelCopy> invokerGrays = list.stream().filter(each -> each.getUrl().equals(upstream.getUrl())).collect(Collectors.toList());
            return invokerGrays.stream().findFirst().orElse(null);
        }
        return randomPicker.pick(list);
    }

    private List<Upstream> convertUpstreamList(final List<GrpcUpstream> grpcUpstreams) {
        return grpcUpstreams.stream().map(u -> Upstream.builder()
                .protocol(u.getProtocol())
                .url(u.getUpstreamUrl())
                .weight(u.getWeight())
                .status(u.isStatus())
                .timestamp(u.getTimestamp())
                .build()).collect(Collectors.toList());
    }

}
