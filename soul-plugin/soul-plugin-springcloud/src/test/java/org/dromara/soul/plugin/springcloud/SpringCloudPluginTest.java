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

package org.dromara.soul.plugin.springcloud;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.dromara.soul.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.test.StepVerifier;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For SpringCloudPlugin.
 *
 * @author nuo-promise
 **/
@RunWith(MockitoJUnitRunner.class)
public class SpringCloudPluginTest {

    @Mock
    private LoadBalancerClient loadBalancerClient;

    private SpringCloudPlugin springCloudPlugin;

    private ServerWebExchange exchange;

    @Before
    public void setUp() {
        springCloudPlugin = new SpringCloudPlugin(loadBalancerClient);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/springcloud?type=cloud").build());
    }

    @Test(expected = NullPointerException.class)
    public void doExecute() {
        final SoulPluginChain chain = mock(SoulPluginChain.class);
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .build();
        final SelectorData selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .build();
        StepVerifier.create(springCloudPlugin.doExecute(exchange, chain, selectorData, null)).expectSubscription().verifyComplete();
        final SpringCloudRuleHandle springCloudRuleHandle = new SpringCloudRuleHandle();
        springCloudRuleHandle.setPath("/springcloud");
        springCloudRuleHandle.setTimeout(1000L);
        final RuleData rule = RuleData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudRuleHandle))
                .build();
        ServiceInstance serviceInstance = mock(ServiceInstance.class);
        when(loadBalancerClient.choose("serviceId")).thenReturn(serviceInstance);
        SoulContext soulContext = new SoulContext();
        soulContext.setRealUrl("http://localhost/test");
        soulContext.setHttpMethod(HttpMethod.GET.name());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        when(loadBalancerClient.reconstructURI(serviceInstance, URI.create(soulContext.getRealUrl()))).thenReturn(mock(URI.class));
        StepVerifier.create(springCloudPlugin.doExecute(exchange, chain, selectorData, rule)).expectSubscription().verifyComplete();
    }

    @Test
    public void getOrder() {
        final int result = springCloudPlugin.getOrder();
        assertEquals(PluginEnum.SPRING_CLOUD.getCode(), result);
    }

    @Test
    public void named() {
        final String result = springCloudPlugin.named();
        assertEquals(PluginEnum.SPRING_CLOUD.getName(), result);
    }

    @Test
    public void skip() {
        SoulContext soulContext = new SoulContext();
        soulContext.setRpcType(RpcTypeEnum.SPRING_CLOUD.getName());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        final Boolean result = springCloudPlugin.skip(exchange);
        assertFalse(result);
    }
}