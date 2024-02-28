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

package org.apache.shenyu.plugin.key.auth;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.key.auth.handler.KeyAuthPluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link KeyAuthPlugin}.
 */
public class KeyAuthPluginTest {

    private KeyAuthPlugin keyAuthPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    private KeyAuthPluginDataHandler keyAuthPluginDataHandler;

    @BeforeEach
    public void setUp() {
        keyAuthPlugin = new KeyAuthPlugin();
        chain = mock(ShenyuPluginChain.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        selectorData = mock(SelectorData.class);
        ruleData = new RuleData();
        ruleData.setId("keyAuthRule");
        ruleData.setSelectorId("keyAuth");
        ruleData.setName("test");
        keyAuthPluginDataHandler = new KeyAuthPluginDataHandler();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @Test
    public void testNotConfigured() {
        ruleData.setHandle("{}");
        keyAuthPluginDataHandler.handlerRule(ruleData);
        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .build());
        Mono<Void> mono = keyAuthPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testKeyAuthWithHeaderCredentials() {
        ruleData.setHandle("{\"keyName\":\"apiKey\",\"key\":\"key\","
                + "\"hideCredentials\":\"false\"}");
        keyAuthPluginDataHandler.handlerRule(ruleData);
        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("apiKey", "key")
                .build());
        Mono<Void> mono = keyAuthPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testKeyAuthWithQueryCredentials() {
        ruleData.setHandle("{\"keyName\":\"apiKey\",\"key\":\"key\","
                + "\"hideCredentials\":\"false\"}");
        keyAuthPluginDataHandler.handlerRule(ruleData);
        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .queryParam("apiKey", "key")
                .build());
        Mono<Void> mono = keyAuthPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testKeyAuthWithIncorrectKey() {
        ruleData.setHandle("{\"keyName\":\"apiKey\",\"key\":\"key\","
                + "\"hideCredentials\":\"false\"}");
        keyAuthPluginDataHandler.handlerRule(ruleData);
        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("apiKey", "123456")
                .build());
        Mono<Void> mono = keyAuthPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.KEY_AUTH.getName(), keyAuthPlugin.named());
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.KEY_AUTH.getCode(), keyAuthPlugin.getOrder());
    }
}
