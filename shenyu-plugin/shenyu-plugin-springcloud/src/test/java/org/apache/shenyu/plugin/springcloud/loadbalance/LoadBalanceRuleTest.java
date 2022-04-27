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

import com.netflix.loadbalancer.BaseLoadBalancer;
import com.netflix.loadbalancer.ILoadBalancer;
import com.netflix.loadbalancer.Server;
import com.netflix.loadbalancer.ZoneAvoidanceRule;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * The Test Case For LoadBalanceRule.
 */
public final class LoadBalanceRuleTest {

    private LoadBalanceRule loadBalanceRule = new LoadBalanceRule();

    private SpringCloudPluginDataHandler springCloudPluginDataHandler = new SpringCloudPluginDataHandler();

    @Test
    public void testChoose() {
        LoadBalanceKey loadBalanceKey = new LoadBalanceKey();
        loadBalanceKey.setIp("0.0.0.0");
        loadBalanceKey.setSelectorId("1");
        loadBalanceKey.setLoadBalance("random");
        LoadBalanceKeyHolder.setLoadBalanceKey(loadBalanceKey);
        ZoneAvoidanceRule zoneAvoidanceRule = new ZoneAvoidanceRule();
        ILoadBalancer lb = new BaseLoadBalancer();
        List<Server> list = new ArrayList<>();
        list.add(new Server("localhost", 8080));
        lb.addServers(list);
        loadBalanceRule.setLoadBalancer(lb);
        List<DivideUpstream> divideUpstreams = new ArrayList<>();
        DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamUrl("localhost:8080")
                .build();
        divideUpstreams.add(divideUpstream);
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .divideUpstreams(divideUpstreams)
                .gray(true)
                .build();
        final SelectorData selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorData);
        Server server = loadBalanceRule.choose(loadBalanceKey);
        Assertions.assertEquals(server.getHostPort(), "localhost:8080");
    }
}
