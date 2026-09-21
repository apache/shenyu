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

package org.apache.shenyu.plugin.ai.sensitive.word;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.SensitiveWordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.sensitive.word.handler.SensitiveWordPluginDataHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.ReactiveSetOperations;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link SensitiveWordPlugin}.
 */
public final class SensitiveWordPluginTest {

    private static final String REDIS_KEY = "test:sensitive:words";

    private SensitiveWordPlugin plugin;

    private ShenyuPluginChain chain;

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);

        plugin = new SensitiveWordPlugin(ServerCodecConfigurer.create().getReaders());
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());
        ruleData = new RuleData();
        ruleData.setId("rule-1");
        ruleData.setName("rule-1");
        ruleData.setSelectorId("selector-1");
        ruleData.setPluginName(PluginEnum.SENSITIVE_WORD.getName());
        ruleData.setHandle("{\"redisKey\":\"" + REDIS_KEY + "\"}");
        SensitiveWordHandle handle = SensitiveWordHandle.newDefaultInstance();
        handle.setRedisKey(REDIS_KEY);
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
    }

    @AfterEach
    public void tearDown() {
        SpringBeanUtils.getInstance().setApplicationContext(null);
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
        SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get().removeHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME);
        SensitiveWordPluginDataHandler.DICTIONARIES.get().removeHandle(REDIS_KEY);
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.SENSITIVE_WORD.getName(), plugin.named());
    }

    @Test
    public void testGetOrder() {
        assertEquals(PluginEnum.SENSITIVE_WORD.getCode(), plugin.getOrder());
    }

    @Test
    public void testPassThroughWhenNoRuleHandle() {
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
        StepVerifier.create(plugin.doExecute(exchange("a clean request"), chain, null, ruleData)).verifyComplete();
        verify(chain).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testPassThroughWhenRedisIsNotInitialized() {
        StepVerifier.create(plugin.doExecute(exchange("a clean request"), chain, null, ruleData)).verifyComplete();
        verify(chain).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testPassThroughWhenTheBodyIsClean() {
        mockRedisDictionary("forbidden", "banned");
        StepVerifier.create(plugin.doExecute(exchange("a clean request"), chain, null, ruleData)).verifyComplete();
        verify(chain).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testRejectTheRequestContainingASensitiveWord() {
        mockRedisDictionary("forbidden", "banned");
        MockServerWebExchange exchange = exchange("this request is banned");
        StepVerifier.create(plugin.doExecute(exchange, chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        StepVerifier.create(exchange.getResponse().getBodyAsString())
                .expectNextMatches(body -> body.contains("sensitive words") && body.contains("banned"))
                .verifyComplete();
    }

    @Test
    public void testPassThroughWhenRedisFails() {
        ReactiveRedisTemplate<String, String> redisTemplate = mock(ReactiveRedisTemplate.class);
        ReactiveSetOperations<String, String> setOperations = mock(ReactiveSetOperations.class);
        when(redisTemplate.opsForSet()).thenReturn(setOperations);
        when(setOperations.members(REDIS_KEY)).thenReturn(Flux.error(new IllegalStateException("redis is down")));
        cacheRedisTemplate(redisTemplate);

        // fail open: a broken redis must not stop the traffic
        StepVerifier.create(plugin.doExecute(exchange("this request is banned"), chain, null, ruleData)).verifyComplete();
        verify(chain).execute(any(ServerWebExchange.class));
    }

    @SuppressWarnings("unchecked")
    private void mockRedisDictionary(final String... words) {
        ReactiveRedisTemplate<String, String> redisTemplate = mock(ReactiveRedisTemplate.class);
        ReactiveSetOperations<String, String> setOperations = mock(ReactiveSetOperations.class);
        when(redisTemplate.opsForSet()).thenReturn(setOperations);
        when(setOperations.members(REDIS_KEY)).thenReturn(Flux.fromArray(words));
        cacheRedisTemplate(redisTemplate);
    }

    @SuppressWarnings("unchecked")
    private void cacheRedisTemplate(final ReactiveRedisTemplate<String, String> redisTemplate) {
        SensitiveWordPluginDataHandler.REDIS_TEMPLATES.get()
                .cachedHandle(SensitiveWordPluginDataHandler.PLUGIN_NAME, redisTemplate);
    }

    private MockServerWebExchange exchange(final String body) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/ai/chat")
                .contentType(MediaType.TEXT_PLAIN)
                .body(body));
    }
}
