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

package org.apache.shenyu.plugin.record;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.HttpRecordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.record.body.RecordServerHttpRequest;
import org.apache.shenyu.plugin.record.body.RecordServerHttpResponse;
import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.apache.shenyu.plugin.record.handler.HttpRecordPluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class HttpRecordPluginTest {

    private HttpRecordPlugin httpRecordPlugin;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    private ArgumentCaptor<ServerWebExchange> exchangeCaptor;

    @BeforeEach
    public void setUp() {
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(new HttpRecordCollectConfig.RecordConfig());
        httpRecordPlugin = new HttpRecordPlugin();
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test").build());
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        selectorData = SelectorData.builder().id("selectorId").build();
        ruleData = new RuleData();
        ruleData.setId("ruleId");
        ruleData.setSelectorId("selectorId");
        exchangeCaptor = ArgumentCaptor.forClass(ServerWebExchange.class);
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.HTTP_RECORD.getName(), httpRecordPlugin.named());
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.HTTP_RECORD.getCode(), httpRecordPlugin.getOrder());
    }

    @Test
    public void testDoExecuteWithNullHandle() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test?test=test")
                .header("X-test", "test")
                .build());
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
        Mono<Void> result = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        assertFalse(exchangeCaptor.getValue().getRequest() instanceof RecordServerHttpRequest);
    }

    @Test
    public void testDoExecuteWithReplayHeader() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now - 60000);
        handle.setEndTime(now + 60000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        ServerWebExchange replayExchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost/test")
                        .header(Constants.X_SHENYU_REPLAY, "true")
                        .build());
        Mono<Void> result = httpRecordPlugin.doExecute(replayExchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        assertFalse(exchangeCaptor.getValue().getRequest() instanceof RecordServerHttpRequest);
    }

    @Test
    public void testDoExecuteWithTimeWindowExpired() {
        HttpRecordHandle handle = new HttpRecordHandle();
        handle.setStartTime(System.currentTimeMillis() - 120000);
        handle.setEndTime(System.currentTimeMillis() - 60000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .build());
        Mono<Void> result = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        assertFalse(exchangeCaptor.getValue().getRequest() instanceof RecordServerHttpRequest);
    }

    @Test
    public void testDoExecuteWithinTimeWindow() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now - 60000);
        handle.setEndTime(now + 60000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .header("test", "test")
                .build());

        Mono<Void> result = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        ServerWebExchange captured = exchangeCaptor.getValue();

        assertTrue(captured.getRequest() instanceof RecordServerHttpRequest,
                "Request should be wrapped with RecordServerHttpRequest within time window");
        assertTrue(captured.getResponse() instanceof RecordServerHttpResponse,
                "Response should be wrapped with RecordServerHttpResponse within time window");

        ShenyuHttpRequestRecord record = extractRecord((RecordServerHttpRequest) captured.getRequest());
        assertNotNull(record.getTraceId(), "traceId should be generated");
        assertFalse(record.getTraceId().isEmpty(), "traceId should not be empty");
        assertEquals("GET", record.getMethod(), "method should be GET");
        assertEquals("/test", record.getRequestUri(), "requestUri should be /test");
        assertNull(record.getQueryParams(), "queryParams should be null for request without query string");
        assertNotNull(record.getRequestHeaders(), "requestHeaders should not be null");
        assertFalse(record.getRequestHeaders().isEmpty(), "requestHeaders should not be empty");
    }

    @Test
    public void testDoExecuteWithinTimeWindowWithQueryParams() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now - 60000);
        handle.setEndTime(now + 60000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        ServerWebExchange queryExchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost/test")
                        .queryParam("foo", "bar")
                        .queryParam("baz", "qux")
                        .build());

        Mono<Void> result = httpRecordPlugin.doExecute(queryExchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        ServerWebExchange captured = exchangeCaptor.getValue();
        ShenyuHttpRequestRecord record = extractRecord((RecordServerHttpRequest) captured.getRequest());

        assertEquals("foo=bar&baz=qux", record.getQueryParams(), "queryParams should capture the full query string");
        assertEquals("/test", record.getRequestUri(), "requestUri should be /test");
    }

    @Test
    public void testDoExecuteWithinTimeWindowWithTaskId() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now - 60000);
        handle.setEndTime(now + 60000);
        handle.setTaskId("myTaskId123");
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .build());

        Mono<Void> result = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        ServerWebExchange captured = exchangeCaptor.getValue();
        ShenyuHttpRequestRecord record = extractRecord((RecordServerHttpRequest) captured.getRequest());

        assertEquals("myTaskId123", record.getTaskId(), "taskId should match the handle's taskId");
    }

    @Test
    public void testDoExecuteWithinTimeWindowWithCustomHeaders() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now - 60000);
        handle.setEndTime(now + 60000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        ServerWebExchange headerExchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost/test")
                        .header("X-Custom-Header", "customValue")
                        .header("Accept", "application/json")
                        .build());

        Mono<Void> result = httpRecordPlugin.doExecute(headerExchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        ServerWebExchange captured = exchangeCaptor.getValue();
        ShenyuHttpRequestRecord record = extractRecord((RecordServerHttpRequest) captured.getRequest());

        assertNotNull(record.getRequestHeaders(), "requestHeaders should not be null");
        assertTrue(record.getRequestHeaders().containsKey("X-Custom-Header"),
                "requestHeaders should contain X-Custom-Header");
        assertEquals("customValue", record.getRequestHeaders().get("X-Custom-Header"),
                "X-Custom-Header value should match");
        assertTrue(record.getRequestHeaders().containsKey("Accept"),
                "requestHeaders should contain Accept header");
    }

    @Test
    public void testDoExecuteWithStartTimeNotYetReached() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now + 60000);
        handle.setEndTime(now + 120000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test")
                .build());
        Mono<Void> result = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        verify(chain).execute(exchangeCaptor.capture());
        assertFalse(exchangeCaptor.getValue().getRequest() instanceof RecordServerHttpRequest);
    }

    @Test
    public void testDoExecuteWithinTimeWindowEachCallGeneratesUniqueTraceId() {
        HttpRecordHandle handle = new HttpRecordHandle();
        long now = System.currentTimeMillis();
        handle.setStartTime(now - 60000);
        handle.setEndTime(now + 60000);
        HttpRecordPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test?test=test")
                .build());

        Mono<Void> result1 = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result1).expectSubscription().verifyComplete();

        Mono<Void> result2 = httpRecordPlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result2).expectSubscription().verifyComplete();

        verify(chain, times(2)).execute(exchangeCaptor.capture());

        ShenyuHttpRequestRecord record1 = extractRecord((RecordServerHttpRequest) exchangeCaptor.getAllValues().get(0).getRequest());
        ShenyuHttpRequestRecord record2 = extractRecord((RecordServerHttpRequest) exchangeCaptor.getAllValues().get(1).getRequest());

        assertNotNull(record1.getTraceId());
        assertNotNull(record2.getTraceId());
        assertNotEquals(record1.getTraceId(), record2.getTraceId(),
                "Each request should get a unique traceId");
    }

    private ShenyuHttpRequestRecord extractRecord(final RecordServerHttpRequest recordRequest) {
        try {
            Field recordField = RecordServerHttpRequest.class.getDeclaredField("record");
            recordField.setAccessible(true);
            return (ShenyuHttpRequestRecord) recordField.get(recordRequest);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException("Failed to extract record from RecordServerHttpRequest", e);
        }
    }
}
