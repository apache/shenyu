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
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.mock.handler.MockPluginHandler;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
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

    @ParameterizedTest
    @EnumSource(value = HttpStatus.class, names = {"PROCESSING", "NOT_FOUND"})
    public void testDoExecuteWithRecognizedHttpStatusCode(final HttpStatus status) {
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setHttpStatusCode(status.value());
        mockHandle.setResponseContent("{\"status\":\"mocked\"}");
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);

        StepVerifier.create(mockPlugin.doExecute(exchange, chain, mock(SelectorData.class), ruleData))
                .expectSubscription()
                .verifyComplete();

        assertThat(exchange.getResponse().getStatusCode(), is(status));
        assertThat(exchange.getResponse().getBodyAsString().block(), is("{\"status\":\"mocked\"}"));
        verify(chain, never()).execute(any());
    }

    @ParameterizedTest
    @ValueSource(ints = {404, 599})
    public void testWritesConfiguredHttpStatusOverHttp(final int configuredStatus) {
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setHttpStatusCode(configuredStatus);
        String content = "{\"status\":\"mocked\"}";
        mockHandle.setResponseContent(content);
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);

        SelectorData selector = mock(SelectorData.class);
        ReactorHttpHandlerAdapter handler = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder
                .webHandler(current -> mockPlugin.doExecute(current, chain, selector, ruleData)).build());
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle(handler).bindNow();
        try {
            StepVerifier.create(HttpClient.create().get().uri("http://127.0.0.1:" + server.port() + "/mock")
                    .responseSingle((response, bytes) -> {
                        assertEquals(configuredStatus, response.status().code());
                        return bytes.asString();
                    })).expectNext(content).expectComplete().verify(Duration.ofSeconds(5));
            verify(chain, never()).execute(any());
        } finally {
            server.disposeNow();
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {99, 199, 600, 999})
    public void testRejectsUnknownInformationalAndOutOfRangeHttpStatusCodes(final int configuredStatus) {
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setHttpStatusCode(configuredStatus);
        mockHandle.setResponseContent("{\"status\":\"mocked\"}");
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);
        SelectorData selector = mock(SelectorData.class);

        assertThrows(IllegalArgumentException.class, () -> mockPlugin.doExecute(exchange, chain, selector, ruleData));
    }

    @ParameterizedTest
    @ValueSource(strings = {"$5", "C:\\temp\\file", "say \"hello\"", "first\nsecond"})
    public void testGeneratedResponsePreservesJsonEscapes(final String value) {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/mock").queryParam("value", value).build());
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setResponseContent("{\"value\":${expression|#req.queries['value']}}");
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);

        StepVerifier.create(mockPlugin.doExecute(exchange, chain, mock(SelectorData.class), ruleData)).verifyComplete();

        String expected = "{\"value\":" + JsonUtils.toJson(value) + "}";
        assertEquals(expected, exchange.getResponse().getBodyAsString().block());
        assertEquals(expected.getBytes(StandardCharsets.UTF_8).length, exchange.getResponse().getHeaders().getContentLength());
        assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
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

    @ParameterizedTest
    @CsvSource({"0, 2147483647", "2147483647, 2147483647", "0, 0", "10, 15"})
    public void testIntegerResponseSupportsInclusiveBounds(final int min, final int max) {
        RuleData ruleData = buildRuleData();
        MockHandle mockHandle = new MockHandle();
        mockHandle.setResponseContent(String.format("${int|%d-%d}", min, max));
        MockPluginHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), mockHandle);

        StepVerifier.create(mockPlugin.doExecute(exchange, chain, mock(SelectorData.class), ruleData)).verifyComplete();

        String body = exchange.getResponse().getBodyAsString().block();
        int value = Integer.parseInt(body);
        assertTrue(value >= min && value <= max);
        assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
        assertEquals(body.getBytes(StandardCharsets.UTF_8).length, exchange.getResponse().getHeaders().getContentLength());
        verify(chain, never()).execute(any());
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
