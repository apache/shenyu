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

package org.apache.shenyu.plugin.springcloud;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.springcloud.cache.SpringCloudRuleHandleCache;
import org.apache.shenyu.plugin.springcloud.cache.SpringCloudSelectorHandleCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.cloud.netflix.ribbon.RibbonLoadBalancerClient;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.net.URISyntaxException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link SpringCloudPlugin}.
 */
@RunWith(MockitoJUnitRunner.class)
public class SpringCloudPluginTest {

    @Mock
    private RibbonLoadBalancerClient loadBalancerClient;

    private SpringCloudPlugin springCloudPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selector;

    private ShenyuContext shenyuContext;

    @Before
    public void setUp() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(applicationContext);
        MultiValueMap<String, String> valueMap = new LinkedMultiValueMap<>(1);
        valueMap.put("type", Lists.newArrayList("cloud"));
        exchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost/springcloud").queryParams(valueMap).remoteAddress(new InetSocketAddress(8090)).build());
        shenyuContext = mock(ShenyuContext.class);
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        chain = mock(ShenyuPluginChain.class);
        selector = mock(SelectorData.class);
        springCloudPlugin = new SpringCloudPlugin(loadBalancerClient);
    }

    @Test(expected = NullPointerException.class)
    public void doExecute() {
        final ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .build();
        final SelectorData selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .build();
        SpringCloudSelectorHandleCache.getInstance().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        StepVerifier.create(springCloudPlugin.doExecute(exchange, chain, selectorData, null)).expectSubscription().verifyComplete();
        final SpringCloudRuleHandle springCloudRuleHandle = new SpringCloudRuleHandle();
        springCloudRuleHandle.setPath("/springcloud");
        springCloudRuleHandle.setTimeout(1000L);
        final RuleData rule = RuleData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudRuleHandle))
                .build();
        SpringCloudRuleHandleCache.getInstance().cachedHandle(CacheKeyUtils.INST.getKey(rule), springCloudRuleHandle);
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRealUrl("http://localhost/test");
        shenyuContext.setHttpMethod(HttpMethod.GET.name());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
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
        final boolean result = springCloudPlugin.skip(exchange);
        assertFalse(result);
    }

    @Test
    public void testSpringCloudPluginRuleEmpty() {
        Mono<Void> execute = springCloudPlugin.doExecute(exchange, chain, selector, null);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testSpringCloudPluginNotConfigServiceId() {
        final SelectorData selectorData = SelectorData.builder()
                .id("springcloud")
                .handle("[]")
                .build();
        final RuleData rule = RuleData.builder()
                .id("springcloud")
                .selectorId("springcloud")
                .handle("{}")
                .build();

        SpringCloudSelectorHandle springCloudSelectorHandle = new SpringCloudSelectorHandle();
        SpringCloudSelectorHandleCache.getInstance().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        SpringCloudRuleHandle springCloudRuleHandle = GsonUtils.getGson().fromJson(rule.getHandle(), SpringCloudRuleHandle.class);
        SpringCloudRuleHandleCache.getInstance().cachedHandle(CacheKeyUtils.INST.getKey(rule), springCloudRuleHandle);
        Mono<Void> execute = springCloudPlugin.doExecute(exchange, chain, selectorData, rule);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testSpringCloudPluginErrorServiceId() {
        SpringCloudSelectorHandle springCloudSelectorHandle = new SpringCloudSelectorHandle();
        springCloudSelectorHandle.setServiceId("springcloud");
        List<DivideUpstream> divideUpstreams = Stream.of(3, 4, 5)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("divide-upstream-" + weight)
                        .build())
                .collect(Collectors.toList());
        springCloudSelectorHandle.setDivideUpstreams(divideUpstreams);
        final SelectorData selectorData = SelectorData.builder()
                .id("springcloud")
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .build();
        final RuleData rule = RuleData.builder()
                .id("springcloud")
                .selectorId("springcloud")
                .handle("{\"path\":\"service/\"}")
                .build();
        SpringCloudSelectorHandleCache.getInstance().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        SpringCloudRuleHandle springCloudRuleHandle = GsonUtils.getGson().fromJson(rule.getHandle(), SpringCloudRuleHandle.class);
        SpringCloudRuleHandleCache.getInstance().cachedHandle(CacheKeyUtils.INST.getKey(rule), springCloudRuleHandle);
        Mono<Void> execute = springCloudPlugin.doExecute(exchange, chain, selectorData, rule);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testSpringCloudPluginNormal() throws URISyntaxException {
        final SelectorData selectorData = SelectorData.builder()
                .id("springcloud")
                .handle("{\"serviceId\":\"service1\"}")
                .build();
        final RuleData rule = RuleData.builder()
                .id("springcloud")
                .selectorId("springcloud")
                .handle("{\"path\":\"service1/\"}")
                .build();
        SpringCloudSelectorHandle springCloudSelectorHandle = GsonUtils.getGson()
                .fromJson(selectorData.getHandle(), SpringCloudSelectorHandle.class);
        SpringCloudRuleHandle springCloudRuleHandle = GsonUtils.getGson()
                .fromJson(rule.getHandle(), SpringCloudRuleHandle.class);
        SpringCloudSelectorHandleCache.getInstance().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        SpringCloudRuleHandleCache.getInstance().cachedHandle(CacheKeyUtils.INST.getKey(rule), springCloudRuleHandle);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        Mono<Void> execute = springCloudPlugin.doExecute(exchange, chain, selectorData, rule);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }
}
