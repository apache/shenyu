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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiExactResponseCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScope;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiResponseCacheScopeResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Cache authorization is explicitly granted, never restored from wire identity.
 */
class AiProxyCacheScopeTest {

    private static final AiResponseCacheScope SCOPE = new AiResponseCacheScope("tenant", "subject", "access", "p1", "v1", "untrusted", "untrusted");

    private static final String REQUEST = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private static final String RESPONSE = "{\"id\":\"test\",\"object\":\"chat.completion\",\"model\":\"model\","
            + "\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"answer\"},\"finish_reason\":\"stop\"}]}";

    private final AtomicInteger calls = new AtomicInteger();

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private final AiProxyPluginHandler handler = new AiProxyPluginHandler();

    private AiProxyEngine engine;

    @BeforeEach
    void setUp() {
        selector.setId("cache-selector");
        rule.setId("cache-rule");
        AiProxyHandle handle = new AiProxyHandle();
        handle.setProvider("OpenAI");
        handle.setBaseUrl("https://example.org");
        handle.setApiKey("test-only-key");
        handle.setModel("model");
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE), handle);
        AiResponseCacheProperties properties = new AiResponseCacheProperties();
        properties.setEnabled(true);
        engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi())), protocolRouteResolver(), request -> {
                    calls.incrementAndGet();
                    return Mono.just(new AiUpstreamResponse(200, Map.of("Content-Type", List.of("application/json")),
                            Flux.just(RESPONSE.getBytes(StandardCharsets.UTF_8))));
                }, new AiProxyRetryProperties(), null, event -> { }, new AiExactResponseCache(properties));
    }

    @Test
    void missingOrEmptyResolverCannotRestoreInjectedScope() {
        for (AiProxyPlugin plugin : List.of(plugin(null), plugin((exchange, selectorId, ruleId) -> Mono.empty()))) {
            for (int index = 0; index < 2; index++) {
                MockServerWebExchange exchange = exchange();
                exchange.getAttributes().put(AiResponseCacheScope.REACTOR_KEY, SCOPE);
                plugin.doExecute(exchange, ignored -> Mono.empty(), selector, rule)
                        .contextWrite(context -> context.put(AiResponseCacheScope.REACTOR_KEY, SCOPE)).block();
            }
        }
        assertEquals(4, calls.get());
    }

    @Test
    void resolverRunsOnHitsAndMatchedRuleOverridesReturnedRoute() {
        AtomicInteger authorizations = new AtomicInteger();
        AiProxyPlugin plugin = plugin((exchange, selectorId, ruleId) -> {
            authorizations.incrementAndGet();
            return Mono.just(SCOPE);
        });
        plugin.doExecute(exchange(), ignored -> Mono.empty(), selector, rule).block();
        plugin.doExecute(exchange(), ignored -> Mono.empty(), selector, rule).block();
        assertEquals(1, calls.get());
        rule.setId("other-rule");
        plugin.doExecute(exchange(), ignored -> Mono.empty(), selector, rule).block();
        assertEquals(2, calls.get());
        assertEquals(3, authorizations.get());
    }

    @Test
    void resolverErrorDoesNotFallBackToUpstream() {
        AiProxyPlugin plugin = plugin((exchange, selectorId, ruleId) -> Mono.error(new IllegalStateException("scope unavailable")));
        plugin.doExecute(exchange(), ignored -> Mono.empty(), selector, rule).block();
        assertEquals(0, calls.get());
    }

    @Test
    void clientNoCacheBypassesEvenAnExistingEntry() {
        AiProxyPlugin plugin = plugin((exchange, selectorId, ruleId) -> Mono.just(SCOPE));
        plugin.doExecute(exchange(), ignored -> Mono.empty(), selector, rule).block();
        for (int index = 0; index < 2; index++) {
            MockServerWebExchange noCache = MockServerWebExchange.from(MockServerHttpRequest.post("/chat")
                    .header("Cache-Control", "no-cache").body(REQUEST));
            plugin.doExecute(noCache, ignored -> Mono.empty(), selector, rule).block();
        }
        assertEquals(3, calls.get());
    }

    private AiProxyPlugin plugin(final AiResponseCacheScopeResolver resolver) {
        return new AiProxyPlugin(new AiProxyConfigService(handler), engine, handler, null, resolver);
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/chat").header("X-Tenant", "forged").body(REQUEST));
    }
}
