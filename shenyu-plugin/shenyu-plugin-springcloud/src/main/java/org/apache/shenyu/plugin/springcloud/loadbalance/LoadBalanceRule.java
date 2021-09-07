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

package org.apache.shenyu.plugin.springcloud.loadbalance;

import com.netflix.loadbalancer.Server;
import com.netflix.loadbalancer.ZoneAvoidanceRule;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.springcloud.cache.InstanceCacheManager;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The spring cloud loadbalancer rule.
 */
public class LoadBalanceRule extends ZoneAvoidanceRule {

    @Override
    public Server choose(final Object key) {
        LoadBalanceKey loadBalanceKey = key instanceof LoadBalanceKey ? (LoadBalanceKey) key : null;
        List<Server> available = getPredicate().getEligibleServers(getLoadBalancer().getReachableServers(), key);
        if (CollectionUtils.isEmpty(available)) {
            return null;
        }
        List<DivideUpstream> divideUpstreams = InstanceCacheManager.getInstance().findUpstreamListBySelectorId(loadBalanceKey.getSelectorId());
        if (CollectionUtils.isEmpty(divideUpstreams)) {
            return super.choose(Constants.DEFAULT);
        }
        List<DivideUpstream> grayUpstream = divideUpstreams.stream().filter(DivideUpstream::isGray).collect(Collectors.toList());
        //choose from gray
        if (CollectionUtils.isNotEmpty(grayUpstream)) {
            Upstream upstream = LoadBalancerFactory.selector(convert(grayUpstream), loadBalanceKey.getLoadBalance(), loadBalanceKey.getIp());
            return available.stream().filter(server -> server.getHostPort().equals(upstream.getUrl())).findFirst().orElse(null);
        }
        //select server from available to choose
        List<DivideUpstream> choose = new ArrayList<>(available.size());
        for (Server server : available) {
            Optional<DivideUpstream> divideUpstream = Optional.ofNullable(divideUpstreams.stream()
                    .filter(DivideUpstream::isStatus)
                    .filter(upstream -> upstream.getUpstreamUrl().equals(server.getHostPort()))
                    .findFirst()).orElse(Optional.empty());
            if (divideUpstream.isPresent()) {
                choose.add(divideUpstream.get());
            }
        }
        if (CollectionUtils.isEmpty(choose)) {
            return super.choose(Constants.DEFAULT);
        }
        Upstream upstream = LoadBalancerFactory.selector(convert(choose), loadBalanceKey.getLoadBalance(), loadBalanceKey.getIp());
        return available.stream().filter(server -> server.getHostPort().equals(upstream.getUrl())).findFirst().orElse(null);
    }

    private List<Upstream> convert(final List<DivideUpstream> upstreamList) {
        return upstreamList.stream().map(u -> Upstream.builder()
                .url(u.getUpstreamUrl())
                .weight(u.getWeight())
                .status(u.isStatus())
                .timestamp(u.getTimestamp())
                .warmup(u.getWarmup())
                .build()).collect(Collectors.toList());
    }
}
