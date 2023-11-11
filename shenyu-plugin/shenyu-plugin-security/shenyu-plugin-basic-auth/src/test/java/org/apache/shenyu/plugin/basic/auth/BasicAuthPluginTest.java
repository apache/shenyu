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

package org.apache.shenyu.plugin.basic.auth;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.basic.auth.handle.BasicAuthPluginDataHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link BasicAuthPlugin}.
 */
public final class BasicAuthPluginTest {

    private BasicAuthPlugin basicAuthPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    private BasicAuthPluginDataHandler basicAuthPluginDataHandler;

    @BeforeEach
    public void setUp() {
        initContext();
        selectorData = mock(SelectorData.class);
        ruleData = new RuleData();
        basicAuthPlugin = new BasicAuthPlugin();
        exchange = createServerWebExchange();
        chain = mock(ShenyuPluginChain.class);
        basicAuthPluginDataHandler = new BasicAuthPluginDataHandler();
    }

    @Test
    public void testDoExecute() {
        ruleData.setHandle("{\"authorization\":\"test:test123\"}");
        basicAuthPluginDataHandler.handlerRule(ruleData);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(basicAuthPlugin.doExecute(exchange, chain, selectorData, ruleData)).expectSubscription().verifyComplete();

        verify(chain).execute(exchange);
    }

    @Test
    public void testDoExecuteWithCustomHandleType() {
        ruleData.setHandle("{\"handleType\":\"custom\",\"customAuthorization\":\"test:test123\"}");
        basicAuthPluginDataHandler.handlerRule(ruleData);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(basicAuthPlugin.doExecute(exchange, chain, selectorData, ruleData)).expectSubscription().verifyComplete();

        verify(chain).execute(exchange);
    }

    @Test
    public void testDoExecuteWithoutHandle() {
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        Mono<Void> mono = basicAuthPlugin.doExecute(exchange, chain, selectorData, ruleData);

        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    private static boolean hasHeader(final ServerWebExchange exchange, final String name, final String val) {
        return exchange.getRequest().getHeaders().get(name).contains(val);
    }

    @Test
    public void testNamed() {
        final String result = basicAuthPlugin.named();
        Assertions.assertEquals(PluginEnum.BASIC_AUTH.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = basicAuthPlugin.getOrder();
        Assertions.assertEquals(PluginEnum.BASIC_AUTH.getCode(), result);
    }

    private void initContext() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(context);
        PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"defaultHandleJson\":\"{\\\"authorization\\\":\\\"test:test123\\\"}\"}", "0", false, null);
        new BasicAuthPluginDataHandler().handlerPlugin(pluginData);
    }

    private ServerWebExchange createServerWebExchange() {
        return MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header(HttpHeaders.AUTHORIZATION, "test:test123")
                .build());
    }
}
