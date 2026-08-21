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

package org.apache.shenyu.plugin.mock;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.MockHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mock.handler.MockPluginHandler;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * test case for {@link MockPlugin}.
 */
@ExtendWith(MockitoExtension.class)
public final class MockPluginTest {

    private static final String SELECTOR_ID = "mock-selector";

    private static final String RULE_ID = "mock-rule";

    private final MockPlugin mockPlugin = new MockPlugin();

    @Mock
    private ShenyuPluginChain chain;

    private MockServerWebExchange exchange;

    @BeforeEach
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        MockPluginHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, RULE_ID));
    }

    @AfterEach
    public void clean() {
        MockPluginHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, RULE_ID));
    }

    /**
     * issue #6881: a mock rule whose handle omits {@code httpStatusCode} must not NPE
     * and should fall back to a 200 response with the mock content.
     */
    @Test
    public void testDoExecuteWithNullHttpStatusCode() {
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setHttpStatusCode(null);
        mockHandle.setResponseContent("{\"user\":\"test\"}");
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);

        StepVerifier.create(mockPlugin.doExecute(exchange, chain, mock(SelectorData.class), ruleData))
                .expectSubscription()
                .verifyComplete();

        assertThat(exchange.getResponse().getStatusCode(), is(HttpStatus.OK));
        assertThat(exchange.getResponse().getBodyAsString().block(), is("{\"user\":\"test\"}"));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testDoExecuteWithSpecifiedHttpStatusCode() {
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setHttpStatusCode(HttpStatus.NOT_FOUND.value());
        mockHandle.setResponseContent("not found");
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);

        StepVerifier.create(mockPlugin.doExecute(exchange, chain, mock(SelectorData.class), ruleData))
                .expectSubscription()
                .verifyComplete();

        assertThat(exchange.getResponse().getStatusCode(), is(HttpStatus.NOT_FOUND));
        assertThat(exchange.getResponse().getBodyAsString().block(), is("not found"));
        verify(chain, never()).execute(any());
    }

    @Test
    public void testDoExecuteWithoutCachedHandle() {
        RuleData ruleData = buildRuleData();
        when(chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(mockPlugin.doExecute(exchange, chain, mock(SelectorData.class), ruleData))
                .expectSubscription()
                .verifyComplete();

        verify(chain).execute(any());
    }

    @Test
    public void testGetOrder() {
        final int result = mockPlugin.getOrder();
        assertThat(PluginEnum.MOCK.getCode(), Matchers.is(result));
    }

    @Test
    public void testNamed() {
        final String result = mockPlugin.named();
        assertThat(PluginEnum.MOCK.getName(), Matchers.is(result));
    }

    private RuleData buildRuleData() {
        return new RuleData()
                .setSelectorId(SELECTOR_ID)
                .setId(RULE_ID);
    }
}
