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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.memory.MemoryCache;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.client.reactive.MockClientHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;

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

    @Test
    public void httpResponseTest() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        MockClientHttpResponse clientResponse = new MockClientHttpResponse(HttpStatus.OK);
        clientResponse.setBody("body");
        final CacheRuleHandle cacheRuleHandle = new CacheRuleHandle();
        CachePlugin.CacheHttpResponse cacheHttpResponse = new CachePlugin.CacheHttpResponse(exchange, cacheRuleHandle);
        cacheHttpResponse.getHeaders().add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
        final Mono<Void> mono = cacheHttpResponse.writeWith(clientResponse.getBody());
        StepVerifier.create(mono).expectSubscription().verifyComplete();
    }

    @Test
    public void pluginTest() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        final CachePlugin cachePlugin = new CachePlugin();
        final ShenyuPluginChain shenyuPluginChain = mock(ShenyuPluginChain.class);
        final RuleData ruleData = new RuleData();
        CachePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), new CacheRuleHandle());
        Mockito.when(shenyuPluginChain.execute(any())).thenReturn(Mono.empty());
        final Mono<Void> result = cachePlugin.doExecute(exchange, shenyuPluginChain, null, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        final MemoryCache memoryCache = new MemoryCache();
        Singleton.INST.single(ICache.class, memoryCache);
        final Mono<Void> result2 = cachePlugin.doExecute(exchange, shenyuPluginChain, null, ruleData);
        StepVerifier.create(result2).expectSubscription().verifyComplete();

        memoryCache.cacheData(CacheUtils.dataKey(exchange), MediaType.APPLICATION_JSON_VALUE.getBytes(StandardCharsets.UTF_8),
                60L).subscribeOn(Schedulers.boundedElastic()).subscribe();

        memoryCache.cacheData(CacheUtils.contentTypeKey(exchange), MediaType.APPLICATION_JSON_VALUE.getBytes(StandardCharsets.UTF_8),
                60L).subscribeOn(Schedulers.boundedElastic()).subscribe();
        final Mono<Void> result3 = cachePlugin.doExecute(exchange, shenyuPluginChain, null, ruleData);
        StepVerifier.create(result3).expectSubscription().verifyComplete();
    }

}
