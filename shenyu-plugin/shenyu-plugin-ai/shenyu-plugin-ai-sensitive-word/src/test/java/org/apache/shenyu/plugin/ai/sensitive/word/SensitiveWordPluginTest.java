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
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
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

import java.nio.charset.StandardCharsets;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
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
        SensitiveWordPluginDataHandler.DICTIONARIES.get().getAllCache().clear();
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
        // the client is told that the request was rejected, never which word matched
        StepVerifier.create(exchange.getResponse().getBodyAsString())
                .expectNextMatches(body -> body.contains("sensitive content detected") && !body.contains("banned"))
                .verifyComplete();
    }

    @Test
    public void testFailClosedRejectsTheRequestWhenTheDictionaryIsUnavailable() {
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .cachedHandle(CacheKeyUtils.INST.getKey(ruleData), failClosedHandle());
        ReactiveRedisTemplate<String, String> redisTemplate = mock(ReactiveRedisTemplate.class);
        ReactiveSetOperations<String, String> setOperations = mock(ReactiveSetOperations.class);
        when(redisTemplate.opsForSet()).thenReturn(setOperations);
        when(setOperations.members(REDIS_KEY)).thenReturn(Flux.error(new IllegalStateException("redis is down")));
        cacheRedisTemplate(redisTemplate);

        MockServerWebExchange exchange = exchange("a clean request");
        StepVerifier.create(plugin.doExecute(exchange, chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        StepVerifier.create(exchange.getResponse().getBodyAsString())
                .expectNextMatches(body -> body.contains("sensitive content detected"))
                .verifyComplete();
    }

    @Test
    public void testTheStaleDictionaryIsUsedWhenRedisFails() {
        mockRedisDictionary("forbidden", "banned");
        MockServerWebExchange first = exchange("a clean request");
        StepVerifier.create(plugin.doExecute(first, chain, null, ruleData)).verifyComplete();
        // the first request legitimately went through, only what follows matters here
        clearInvocations(chain);

        // failClosed + refreshIntervalSeconds = 0: the dictionary is read again on every request
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .cachedHandle(CacheKeyUtils.INST.getKey(ruleData), failClosedHandle());
        ReactiveRedisTemplate<String, String> brokenTemplate = mock(ReactiveRedisTemplate.class);
        ReactiveSetOperations<String, String> setOperations = mock(ReactiveSetOperations.class);
        when(brokenTemplate.opsForSet()).thenReturn(setOperations);
        when(setOperations.members(REDIS_KEY)).thenReturn(Flux.error(new IllegalStateException("redis is down")));
        cacheRedisTemplate(brokenTemplate);

        MockServerWebExchange exchange = exchange("this request is banned");
        StepVerifier.create(plugin.doExecute(exchange, chain, null, ruleData)).verifyComplete();
        // the stale dictionary is still enforced instead of letting the request through
        verify(chain, never()).execute(any(ServerWebExchange.class));
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

    @Test
    public void testTheWordsOfTheRuleAreEnforced() {
        // the redis set is empty, the rule carries its own words
        mockRedisDictionary();
        cacheHandle(handle(customized -> customized.setWords("forbidden, banned")));
        MockServerWebExchange exchange = exchange("this request is banned");
        StepVerifier.create(plugin.doExecute(exchange, chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        StepVerifier.create(exchange.getResponse().getBodyAsString())
                .expectNextMatches(body -> body.contains("sensitive content detected") && !body.contains("banned"))
                .verifyComplete();
    }

    @Test
    public void testTheWordsOfTheRuleAreSeparatedByNewLines() {
        mockRedisDictionary();
        cacheHandle(handle(customized -> customized.setWords("forbidden\nbanned")));
        StepVerifier.create(plugin.doExecute(exchange("this request is banned"), chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testTheWordsOfTheRuleAndTheRedisSetAreBothEnforced() {
        mockRedisDictionary("forbidden");
        cacheHandle(handle(customized -> customized.setWords("banned")));
        // the word of the redis set is still enforced
        StepVerifier.create(plugin.doExecute(exchange("this request is forbidden"), chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        // and so is the word configured on the rule
        StepVerifier.create(plugin.doExecute(exchange("this request is banned"), chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testTheWordsOfTheRuleAreEnforcedWhenRedisFails() {
        cacheHandle(handle(customized -> customized.setWords("banned")));
        ReactiveRedisTemplate<String, String> redisTemplate = mock(ReactiveRedisTemplate.class);
        ReactiveSetOperations<String, String> setOperations = mock(ReactiveSetOperations.class);
        when(redisTemplate.opsForSet()).thenReturn(setOperations);
        when(setOperations.members(REDIS_KEY)).thenReturn(Flux.error(new IllegalStateException("redis is down")));
        cacheRedisTemplate(redisTemplate);

        // the words of the rule do not depend on redis, they must still be enforced
        StepVerifier.create(plugin.doExecute(exchange("this request is banned"), chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testRejectTheOversizedRequestWhenFailClosed() {
        mockRedisDictionary("forbidden");
        cacheHandle(handle(customized -> {
            customized.setMaxBodySize(10L);
            customized.setFailClosed(true);
        }));
        MockServerWebExchange exchange = exchange("this request is not scanned");
        StepVerifier.create(plugin.doExecute(exchange, chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
        StepVerifier.create(exchange.getResponse().getBodyAsString())
                .expectNextMatches(body -> body.contains("sensitive content detected"))
                .verifyComplete();
    }

    @Test
    public void testPassThroughTheOversizedRequestWhenFailOpen() {
        mockRedisDictionary("banned");
        cacheHandle(handle(customized -> customized.setMaxBodySize(10L)));
        // the body is above the bound, so it is not scanned even though it contains a sensitive word
        StepVerifier.create(plugin.doExecute(exchange("this request is banned"), chain, null, ruleData)).verifyComplete();
        verify(chain).execute(any(ServerWebExchange.class));
    }

    @Test
    public void testRejectTheOversizedBodyWhoseSizeIsNotDeclared() {
        mockRedisDictionary("forbidden");
        cacheHandle(handle(customized -> {
            customized.setMaxBodySize(10L);
            customized.setFailClosed(true);
        }));
        // no content length: the size is only known once the body has been read
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/ai/chat")
                .contentType(MediaType.TEXT_PLAIN)
                .body(Flux.just(new DefaultDataBufferFactory().wrap("this request is not scanned".getBytes(StandardCharsets.UTF_8)))));
        StepVerifier.create(plugin.doExecute(exchange, chain, null, ruleData)).verifyComplete();
        verify(chain, never()).execute(any(ServerWebExchange.class));
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

    private SensitiveWordHandle failClosedHandle() {
        SensitiveWordHandle handle = SensitiveWordHandle.newDefaultInstance();
        handle.setRedisKey(REDIS_KEY);
        handle.setRefreshIntervalSeconds(0L);
        handle.setFailClosed(true);
        return handle;
    }

    private SensitiveWordHandle handle(final Consumer<SensitiveWordHandle> customizer) {
        SensitiveWordHandle handle = SensitiveWordHandle.newDefaultInstance();
        handle.setRedisKey(REDIS_KEY);
        customizer.accept(handle);
        return handle;
    }

    private void cacheHandle(final SensitiveWordHandle handle) {
        SensitiveWordPluginDataHandler.CACHED_HANDLE.get()
                .cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
    }

    private MockServerWebExchange exchange(final String body) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/ai/chat")
                .contentType(MediaType.TEXT_PLAIN)
                .body(body));
    }
}
