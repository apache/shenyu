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

import org.apache.shenyu.common.ai.AiNormalizedRequestView;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.common.view.AiRequestViewOperations;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The AI proxy publishes the body it is about to decode as the authoritative view
 * version: a view published by the prompt plugin for the same body stays at its
 * generation, a proxy-only request gets the wire generation, and proxy behavior is
 * unchanged.
 */
class AiProxyPluginViewTest {

    private static final String SELECTOR_ID = "selector-view";

    private static final String REQUEST_BODY = "{\"model\":\"primary-model\","
            + "\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}]}";

    private final AiProxyPluginHandler handler = new AiProxyPluginHandler();

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private MockServerWebExchange exchange;

    @BeforeEach
    void setUp() {
        selector.setId(SELECTOR_ID);
        AiProxyHandle handle = new AiProxyHandle();
        handle.setProvider("OpenAI");
        handle.setBaseUrl("https://main.example");
        handle.setApiKey("primary-key");
        handle.setModel("primary-model");
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE), handle);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(REQUEST_BODY));
    }

    @AfterEach
    void cleanup() {
        handler.getSelectorCachedHandle().removeHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE));
    }

    @Test
    void proxyOnlyRequestPublishesTheWireGeneration() {
        StepVerifier.create(execute(plugin())).verifyComplete();

        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(1L, view.generation());
        assertEquals(AiNormalizedRequestView.ViewKind.WIRE, view.viewKind());
        assertEquals("openai-chat", view.protocol());
        assertEquals("primary-model", view.model());
        assertEquals(1, view.messages().size());
        assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
    }

    @Test
    void theEffectiveViewOfThePromptMutationStaysCurrent() {
        String decorated = REQUEST_BODY.replace("\"messages\":[",
                "\"messages\":[{\"role\":\"system\",\"content\":\"be brief\"},");
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(decorated));
        AiRequestViewOperations.ensurePublished(exchange, REQUEST_BODY, null);
        AiRequestViewOperations.ensurePublished(exchange, decorated, PluginEnum.AI_PROMPT.getName());
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, PluginEnum.AI_TOKEN_LIMITER.getName());

        StepVerifier.create(execute(plugin())).verifyComplete();

        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(2L, view.generation());
        assertEquals(PluginEnum.AI_PROMPT.getName(), view.mutator());
        assertTrue(AiRequestViewOperations.inspectionReport(exchange).stream()
                .allMatch(AiRequestViewOperations.Inspection::current));
        assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
    }

    @Test
    void aBodyChangedWithoutPublicationGetsANewGenerationAtTheProxy() {
        String undetectedMutation = REQUEST_BODY.replace("Hello", "Hello rewritten");
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(undetectedMutation));
        // an earlier stage published the pre-mutation body and a consumer inspected it
        AiRequestViewOperations.ensurePublished(exchange, REQUEST_BODY, null);
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, "staleConsumer");

        StepVerifier.create(execute(plugin())).verifyComplete();

        assertEquals(2L, AiRequestViewOperations.currentGeneration(exchange));
        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(PluginEnum.AI_PROXY.getName(), view.mutator());
        assertFalse(AiRequestViewOperations.inspectionReport(exchange).get(0).current());
    }

    private AiProxyPlugin plugin() {
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi())), protocolRouteResolver(),
                request -> Mono.just(new AiUpstreamResponse(HttpStatus.OK.value(), Map.of(),
                        Flux.just("{}".getBytes(StandardCharsets.UTF_8)))),
                Duration.ZERO, null);
        return new AiProxyPlugin(new AiProxyConfigService(handler), engine, handler, null);
    }

    private Mono<Void> execute(final AiProxyPlugin plugin) {
        return plugin.doExecute(exchange, current -> Mono.empty(), selector, rule);
    }
}
