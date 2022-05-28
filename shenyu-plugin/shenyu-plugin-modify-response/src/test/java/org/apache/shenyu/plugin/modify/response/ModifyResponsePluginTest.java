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

package org.apache.shenyu.plugin.modify.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ModifyResponseRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.modify.response.handler.ModifyResponsePluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * The Test Case For ModifyResponse.
 */
@ExtendWith(MockitoExtension.class)
public final class ModifyResponsePluginTest {

    private ModifyResponsePlugin modifyResponsePlugin;

    private ServerWebExchange exchange;

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    @BeforeEach
    public void setUp() {
        this.modifyResponsePlugin = new ModifyResponsePlugin();
        this.ruleData = mock(RuleData.class);
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        this.exchange = spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
    }

    @Test
    public void testDoExecute() {
        when(chain.execute(any())).thenReturn(Mono.empty());
        final RuleData ruleDataTest = RuleData.builder().id("1")
                .name("test-modify-response-plugin")
                .pluginName("modifyResponse")
                .selectorId("1")
                .matchMode(1)
                .sort(1)
                .loged(true)
                .selectorId("test")
                .build();
        final ModifyResponseRuleHandle responseRuleHandle = new ModifyResponseRuleHandle();
        final Map<String, String> map = new HashMap<>();
        map.put("context-path-id", "1");
        responseRuleHandle.setAddHeaders(map);
        ModifyResponsePluginDataHandler.CACHED_HANDLE.get().cachedHandle("test_test-modify-response-plugin", responseRuleHandle);
        Mono<Void> result = modifyResponsePlugin.doExecute(exchange, chain, selectorData, ruleDataTest);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        assertEquals(modifyResponsePlugin.getOrder(), PluginEnum.MODIFY_RESPONSE.getCode());
    }

    @Test
    public void testNamed() {
        assertEquals(modifyResponsePlugin.named(), PluginEnum.MODIFY_RESPONSE.getName());
    }

    @Test
    public void testSkip() {
        assertFalse(modifyResponsePlugin.skip(exchange));
    }
}
