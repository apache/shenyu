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

import com.google.common.collect.Lists;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.DefaultSoulResult;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.base.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.cloud.client.DefaultServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.net.URISyntaxException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link SpringCloudPlugin}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
public final class SpringCloudPluginTest {

    @Mock
    private LoadBalancerClient mockLoadBalancer;

    private SpringCloudPlugin springCloudPluginUnderTest;

    private ServerWebExchange exchange;

    private SoulPluginChain chain;

    private SelectorData selector;

    private RuleData rule;

    private SoulContext soulContext;

    @Before
    public void setUp() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(SoulResult.class)).thenReturn(new DefaultSoulResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setCfgContext(applicationContext);
        MultiValueMap<String, String> valueMap = new LinkedMultiValueMap<>(1);
        valueMap.put("param", Lists.newArrayList("123"));
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").queryParams(valueMap).build());
        soulContext = mock(SoulContext.class);
        when(soulContext.getRpcType()).thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        chain = mock(SoulPluginChain.class);
        when(this.chain.execute(exchange)).thenReturn(Mono.empty());
        selector = mock(SelectorData.class);
        rule = mock(RuleData.class);
        springCloudPluginUnderTest = new SpringCloudPlugin(mockLoadBalancer);
    }

    @Test
    public void testGetOrder() {
        final int result = springCloudPluginUnderTest.getOrder();
        assertEquals(PluginEnum.SPRING_CLOUD.getCode(), result);
    }

    @Test
    public void testNamed() {
        final String result = springCloudPluginUnderTest.named();
        assertEquals(PluginEnum.SPRING_CLOUD.getName(), result);
    }

    @Test
    public void testSkip() {
        final Boolean result = springCloudPluginUnderTest.skip(exchange);
        assertFalse(result);
    }

    @Test
    public void testSpringCloudPluginRuleEmpty() {
        Mono<Void> execute = springCloudPluginUnderTest.doExecute(exchange, chain, selector, null);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testSpringCloudPluginNotConfigServiceId() {

        when(selector.getHandle()).thenReturn("{}");
        when(rule.getHandle()).thenReturn("{}");
        Mono<Void> execute = springCloudPluginUnderTest.doExecute(exchange, chain, selector, rule);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testSpringCloudPluginErrorServiceId() {
        when(selector.getHandle()).thenReturn("{\"serviceId\":\"service1\"}");
        when(rule.getHandle()).thenReturn("{\"path\":\"service/\"}");
        when(mockLoadBalancer.choose(any())).thenReturn(null);
        Mono<Void> execute = springCloudPluginUnderTest.doExecute(exchange, chain, selector, rule);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testSpringCloudPluginNormal() throws URISyntaxException {
        when(soulContext.getRealUrl()).thenReturn("http://127.0.0.1");
        when(soulContext.getHttpMethod()).thenReturn(HttpMethod.GET.name());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        when(selector.getHandle()).thenReturn("{\"serviceId\":\"service1\"}");
        when(rule.getHandle()).thenReturn("{\"path\":\"service1/\"}");
        when(mockLoadBalancer.choose(any()))
                .thenReturn(new DefaultServiceInstance("instanceId", "service1", "127.0.0.1", 8080, true));
        when(mockLoadBalancer.reconstructURI(any(), any())).thenReturn(new URI("https://localhost:8080/service1/"));
        Mono<Void> execute = springCloudPluginUnderTest.doExecute(exchange, chain, selector, rule);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }
}
