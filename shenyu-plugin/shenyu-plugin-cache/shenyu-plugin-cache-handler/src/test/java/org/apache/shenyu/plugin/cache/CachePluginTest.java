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

package org.apache.shenyu.plugin.cache;

import io.netty.buffer.PooledByteBufAllocator;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.memory.MemoryCache;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * CachePluginTest.
 */
public class CachePluginTest {

    @Test
    public void cacheUtilsTest() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        Assertions.assertDoesNotThrow(() -> CacheUtils.dataKey(exchange));
        Assertions.assertDoesNotThrow(() -> CacheUtils.contentTypeKey(exchange));
    }

    @Test
    public void getOrderTest() {
        final CachePlugin cachePlugin = new CachePlugin();
        Assertions.assertEquals(cachePlugin.getOrder(), PluginEnum.CACHE.getCode());
    }

    @Test
    public void namedTest() {
        final CachePlugin cachePlugin = new CachePlugin();
        Assertions.assertEquals(cachePlugin.named(), PluginEnum.CACHE.getName());
    }

    @ParameterizedTest
    @CsvSource({"1, false", "2, false", "1, true"})
    public void testCacheMissReleasesJoinedBufferOnce(final int chunks, final boolean retainExtraReference) {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/buffer-release").build());
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        MemoryCache memoryCache = new MemoryCache();
        NettyDataBufferFactory factory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
        NettyDataBuffer[] buffers = new NettyDataBuffer[chunks];
        for (int i = 0; i < chunks; i++) {
            String content = chunks == 1 ? "body" : (i == 0 ? "bo" : "dy");
            buffers[i] = factory.allocateBuffer();
            buffers[i].write(content.getBytes(StandardCharsets.UTF_8));
            if (retainExtraReference) {
                buffers[i].retain();
            }
        }
        try (MockedStatic<CacheUtils> cacheUtils = Mockito.mockStatic(CacheUtils.class, Mockito.CALLS_REAL_METHODS)) {
            cacheUtils.when(CacheUtils::getCache).thenReturn(memoryCache);
            CachePlugin.CacheHttpResponse response = new CachePlugin.CacheHttpResponse(exchange, new CacheRuleHandle(), "buffer-release");
            StepVerifier.create(response.writeWith(chunks == 1 ? Mono.just(buffers[0]) : Flux.fromArray(buffers))).verifyComplete();

            for (NettyDataBuffer buffer : buffers) {
                Assertions.assertEquals(retainExtraReference ? 1 : 0, buffer.getNativeBuffer().refCnt());
            }
            Assertions.assertEquals("body", exchange.getResponse().getBodyAsString().block());
            Assertions.assertEquals(4L, exchange.getResponse().getHeaders().getContentLength());
            Assertions.assertArrayEquals("body".getBytes(StandardCharsets.UTF_8), memoryCache.getData(CacheUtils.dataKey(exchange)).block());
            Assertions.assertArrayEquals(MediaType.APPLICATION_JSON_VALUE.getBytes(StandardCharsets.UTF_8),
                    memoryCache.getData(CacheUtils.contentTypeKey(exchange)).block());
        } finally {
            for (NettyDataBuffer buffer : buffers) {
                if (buffer.getNativeBuffer().refCnt() > 0) {
                    buffer.getNativeBuffer().release(buffer.getNativeBuffer().refCnt());
                }
            }
            memoryCache.close();
        }
    }

    @Test
    public void pluginTest() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        final CachePlugin cachePlugin = new CachePlugin();
        final ShenyuPluginChain shenyuPluginChain = mock(ShenyuPluginChain.class);
        final RuleData ruleData = new RuleData();
        CachePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), new CacheRuleHandle());
        Mockito.when(shenyuPluginChain.execute(any())).thenReturn(Mono.empty());
        SelectorData selectorData = mock(SelectorData.class);
        final Mono<Void> result = cachePlugin.doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        final MemoryCache memoryCache = new MemoryCache();
        Singleton.INST.single(ICache.class, memoryCache);
        final Mono<Void> result2 = cachePlugin.doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        StepVerifier.create(result2).expectSubscription().verifyComplete();

        memoryCache.cacheData(CacheUtils.dataKey(exchange), MediaType.APPLICATION_JSON_VALUE.getBytes(StandardCharsets.UTF_8),
                60L).subscribeOn(Schedulers.boundedElastic()).subscribe();

        memoryCache.cacheData(CacheUtils.contentTypeKey(exchange), MediaType.APPLICATION_JSON_VALUE.getBytes(StandardCharsets.UTF_8),
                60L).subscribeOn(Schedulers.boundedElastic()).subscribe();
        final Mono<Void> result3 = cachePlugin.doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        StepVerifier.create(result3).expectSubscription().verifyComplete();
    }

    @Test
    public void testDoExecuteSkipsCacheWhenRuleHandleIsNull() {
        final ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/cache-null-handle-path").build());
        final MemoryCache memoryCache = new MemoryCache();
        Singleton.INST.single(ICache.class, memoryCache);
        final RuleData ruleData = new RuleData();
        ruleData.setSelectorId("cache-null-handle-selector");
        ruleData.setId("cache-null-handle-rule");
        final String key = CacheKeyUtils.INST.getKey(ruleData);
        try {
            CachePluginDataHandler.CACHED_HANDLE.get().removeHandle(key);
            final SelectorData selectorData = new SelectorData();
            selectorData.setId("cache-null-handle-selector");
            final ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
            when(chain.execute(any())).thenAnswer(invocation -> {
                final ServerWebExchange downstream = invocation.getArgument(0);
                downstream.getResponse().getHeaders().setContentType(MediaType.TEXT_PLAIN);
                return downstream.getResponse().writeWith(Mono.just(
                        downstream.getResponse().bufferFactory().wrap("body".getBytes(StandardCharsets.UTF_8))));
            });
            final Mono<Void> result = new CachePlugin().doExecute(exchange, chain, selectorData, ruleData);
            StepVerifier.create(result).verifyComplete();
            Assertions.assertEquals("body", exchange.getResponse().getBodyAsString().block());
            Assertions.assertNull(memoryCache.getData(CacheUtils.dataKey(exchange)).block());
        } finally {
            CachePluginDataHandler.CACHED_HANDLE.get().removeHandle(key);
            memoryCache.close();
        }
    }

    @Test
    public void testDoExecuteCachesResponseWhenRuleHandleIsPresent() {
        final ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/cache-with-handle-path").build());
        final MemoryCache memoryCache = new MemoryCache();
        Singleton.INST.single(ICache.class, memoryCache);
        final RuleData ruleData = new RuleData();
        ruleData.setSelectorId("cache-with-handle-selector");
        ruleData.setId("cache-with-handle-rule");
        final String key = CacheKeyUtils.INST.getKey(ruleData);
        try {
            CachePluginDataHandler.CACHED_HANDLE.get().cachedHandle(key, new CacheRuleHandle());
            final SelectorData selectorData = new SelectorData();
            selectorData.setId("cache-with-handle-selector");
            final ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
            when(chain.execute(any())).thenAnswer(invocation -> {
                final ServerWebExchange downstream = invocation.getArgument(0);
                downstream.getResponse().getHeaders().setContentType(MediaType.TEXT_PLAIN);
                return downstream.getResponse().writeWith(Mono.just(
                        downstream.getResponse().bufferFactory().wrap("body".getBytes(StandardCharsets.UTF_8))));
            });
            final Mono<Void> result = new CachePlugin().doExecute(exchange, chain, selectorData, ruleData);
            StepVerifier.create(result).verifyComplete();
            Assertions.assertEquals("body", exchange.getResponse().getBodyAsString().block());
            Assertions.assertArrayEquals("body".getBytes(StandardCharsets.UTF_8), memoryCache.getData(CacheUtils.dataKey(exchange)).block());
        } finally {
            CachePluginDataHandler.CACHED_HANDLE.get().removeHandle(key);
            memoryCache.close();
        }
    }

}
