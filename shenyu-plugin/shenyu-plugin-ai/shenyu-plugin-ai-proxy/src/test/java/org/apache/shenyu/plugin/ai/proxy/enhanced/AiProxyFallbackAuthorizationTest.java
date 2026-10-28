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

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Fallback authorization exercised through the plugin, engine and real local HTTP transport.
 */
class AiProxyFallbackAuthorizationTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final String SELECTOR_ID = "fallback-authorization";

    private static final String PAYLOAD = "\"messages\":[{\"role\":\"user\",\"content\":\"synthetic prompt\"}],\"stream\":false";

    @Test
    void rejectsInlineFallbackByDefaultBeforeAnyHttp() {
        try (Upstream primary = new Upstream(503); Upstream other = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, "");
            MockServerWebExchange exchange = execute(handle, "{\"baseUrl\":\"" + other.url() + "\",\"model\":\"backup\"}");
            assertEquals(400, exchange.getResponse().getStatusCode().value());
            assertEquals(0, primary.calls.get());
            assertEquals(0, other.calls.get());
        }
    }

    @Test
    void rejectsUnknownTargetRatherThanFallingBackToAdminOrPrimary() {
        try (Upstream primary = new Upstream(503); Upstream admin = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, ",\"dynamicFallbackEnabled\":true,\"fallbackConfig\":{\"baseUrl\":\""
                    + admin.url() + "\",\"apiKey\":\"admin-fixture-key\"}");
            MockServerWebExchange exchange = execute(handle, "{\"target\":\"unknown\"}");
            assertEquals(400, exchange.getResponse().getStatusCode().value());
            assertEquals(0, primary.calls.get());
            assertEquals(0, admin.calls.get());
        }
    }

    @Test
    void rejectsDestinationOverrideEvenWhenTargetIsManaged() {
        try (Upstream primary = new Upstream(503); Upstream other = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, ",\"dynamicFallbackEnabled\":true,\"fallbackTargets\":{\"backup\":{\"model\":\"backup\"}}");
            MockServerWebExchange exchange = execute(handle, "{\"target\":\"backup\",\"baseUrl\":\"" + other.url() + "\"}");
            assertEquals(400, exchange.getResponse().getStatusCode().value());
            assertEquals(0, primary.calls.get());
            assertEquals(0, other.calls.get());
        }
    }

    @Test
    void managedTargetUsesOnlyItsAssignedCredentialAndOverridesAdminFallback() {
        try (Upstream primary = new Upstream(503); Upstream backup = new Upstream(200); Upstream admin = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, ",\"dynamicFallbackEnabled\":true,\"fallbackTargets\":{\"backup\":{\"baseUrl\":\"" + backup.url()
                    + "\",\"apiKey\":\"backup-fixture-key\",\"model\":\"backup\"}},\"fallbackConfig\":{\"baseUrl\":\"" + admin.url() + "\",\"apiKey\":\"admin-fixture-key\"}");
            MockServerWebExchange exchange = execute(handle, "{\"target\":\"backup\"}");
            assertEquals(200, exchange.getResponse().getStatusCode().value());
            assertEquals(1, primary.calls.get());
            assertEquals(1, backup.calls.get());
            assertEquals(0, admin.calls.get());
            assertEquals("Bearer primary-fixture-key", primary.authorization.get());
            assertEquals("Bearer backup-fixture-key", backup.authorization.get());
            assertFalse(backup.body.get().contains("fallbackConfig"));
            assertFalse(backup.body.get().contains("fixture-key"));
            assertEquals("primary-fixture-key", handle.getApiKey());
        }
    }

    @Test
    void partialManagedDestinationCannotReceiveTheResolvedProxyCredential() {
        try (Upstream primary = new Upstream(503); Upstream backup = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, ",\"proxyEnabled\":\"true\",\"dynamicFallbackEnabled\":true,\"fallbackTargets\":{\"backup\":{\"baseUrl\":\"" + backup.url() + "\"}}");
            ProxyApiKeyData mapping = mapping("caller", "mapped-fixture-key");
            AiProxyApiKeyCache.getInstance().cache(mapping);
            try {
                MockServerWebExchange exchange = execute(handle, "{\"target\":\"backup\"}", "caller");
                assertEquals(400, exchange.getResponse().getStatusCode().value());
                assertEquals(0, primary.calls.get());
                assertEquals(0, backup.calls.get());
                assertEquals("primary-fixture-key", handle.getApiKey());
            } finally {
                AiProxyApiKeyCache.getInstance().remove(mapping);
            }
        }
    }

    @Test
    void sameIdentityFallbackKeepsEachCallersProxyCredentialUnderConcurrency() {
        try (Upstream upstream = new Upstream(content -> content.contains("\"model\":\"backup\"") ? 200 : 503, null)) {
            AiProxyHandle handle = handle(upstream, ",\"proxyEnabled\":\"true\",\"dynamicFallbackEnabled\":true,\"fallbackTargets\":{\"backup\":{\"model\":\"backup\"}}");
            AiProxyPluginHandler handler = new AiProxyPluginHandler();
            handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE), handle);
            AiProxyPlugin plugin = plugin(handler);
            for (String caller : List.of("a", "b")) {
                AiProxyApiKeyCache.getInstance().cache(mapping(caller, "mapped-" + caller));
            }
            try {
                StepVerifier.create(Flux.range(0, 16).flatMap(index -> {
                    String caller = index % 2 == 0 ? "a" : "b";
                    MockServerWebExchange exchange = exchange("{\"target\":\"backup\"}", caller);
                    return run(plugin, exchange).then(Mono.fromRunnable(() -> {
                        assertEquals(200, exchange.getResponse().getStatusCode().value());
                        assertEquals("Bearer mapped-" + caller, exchange.getResponse().getHeaders().getFirst("X-Observed-Key"));
                    }));
                }, 8).then()).expectComplete().verify(Duration.ofSeconds(15));
                assertEquals(32, upstream.calls.get());
                assertEquals("primary-fixture-key", handle.getApiKey());
                assertEquals(null, handle.getFallbackTargets().get("backup").getApiKey());
            } finally {
                for (String caller : List.of("a", "b")) {
                    AiProxyApiKeyCache.getInstance().remove(mapping(caller, "mapped-" + caller));
                }
            }
        }
    }

    @Test
    void managedTargetRedirectIsNotFollowedToAnotherUpstream() {
        try (Upstream primary = new Upstream(503); Upstream other = new Upstream(200);
                Upstream redirect = new Upstream(content -> 307, other.url())) {
            AiProxyHandle handle = handle(primary, ",\"dynamicFallbackEnabled\":true,\"fallbackTargets\":{\"backup\":{\"baseUrl\":\"" + redirect.url()
                    + "\",\"apiKey\":\"backup-fixture-key\"}}");
            MockServerWebExchange exchange = execute(handle, "{\"target\":\"backup\"}");
            assertEquals(307, exchange.getResponse().getStatusCode().value());
            assertEquals(1, redirect.calls.get());
            assertEquals("Bearer backup-fixture-key", redirect.authorization.get());
            assertEquals(0, other.calls.get());
        }
    }

    @Test
    void noRequestedTargetPreservesExplicitAdminFallback() {
        try (Upstream primary = new Upstream(503); Upstream backup = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, ",\"fallbackConfig\":{\"baseUrl\":\"" + backup.url() + "\",\"apiKey\":\"admin-fixture-key\"}");
            AiProxyPluginHandler handler = new AiProxyPluginHandler();
            handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE), handle);
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/chat").body("{" + PAYLOAD + "}"));
            StepVerifier.create(run(plugin(handler), exchange)).expectComplete().verify(TIMEOUT);
            assertEquals(200, exchange.getResponse().getStatusCode().value());
            assertEquals("Bearer admin-fixture-key", backup.authorization.get());
            assertEquals(1, primary.calls.get());
            assertEquals(1, backup.calls.get());
        }
    }

    @Test
    void selectorConfigurationUpdatesRevokeRequestSelectedTargets() {
        try (Upstream primary = new Upstream(503); Upstream backup = new Upstream(200)) {
            AiProxyHandle handle = handle(primary, ",\"dynamicFallbackEnabled\":true,\"fallbackTargets\":{\"backup\":{\"baseUrl\":\"" + backup.url()
                    + "\",\"apiKey\":\"backup-fixture-key\"}}");
            AiProxyPluginHandler handler = new AiProxyPluginHandler();
            SelectorData selector = new SelectorData();
            selector.setId(SELECTOR_ID);
            selector.setHandle(GsonUtils.getInstance().toJson(handle));
            handler.handlerSelector(selector);
            AiProxyPlugin plugin = plugin(handler);
            MockServerWebExchange allowed = exchange("{\"target\":\"backup\"}", "");
            StepVerifier.create(run(plugin, allowed)).expectComplete().verify(TIMEOUT);
            assertEquals(200, allowed.getResponse().getStatusCode().value());
            handle.setDynamicFallbackEnabled(false);
            selector.setHandle(GsonUtils.getInstance().toJson(handle));
            handler.handlerSelector(selector);
            MockServerWebExchange revoked = exchange("{\"target\":\"backup\"}", "");
            StepVerifier.create(run(plugin, revoked)).expectComplete().verify(TIMEOUT);
            assertEquals(400, revoked.getResponse().getStatusCode().value());
            assertEquals(1, primary.calls.get());
            assertEquals(1, backup.calls.get());
        }
    }

    @Test
    void rejectionDiagnosticsContainNeitherCredentialsNorPayloads() {
        Logger logger = (Logger) LoggerFactory.getLogger(AiProxyPlugin.class);
        ListAppender<ILoggingEvent> messages = new ListAppender<>();
        messages.start();
        logger.addAppender(messages);
        try (Upstream primary = new Upstream(503)) {
            AiProxyHandle handle = handle(primary, ",\"dynamicFallbackEnabled\":true");
            MockServerWebExchange exchange = execute(handle, "{\"target\":\"unknown\",\"apiKey\":\"request-fixture-key\"}");
            assertEquals(400, exchange.getResponse().getStatusCode().value());
            assertFalse(messages.list.isEmpty());
            String rendered = messages.list.stream().map(ILoggingEvent::getFormattedMessage).reduce("", String::concat);
            assertTrue(rendered.contains("ShenyuException"));
            assertFalse(rendered.contains("fixture-key"));
            assertFalse(rendered.contains("synthetic prompt"));
            assertFalse(rendered.contains("fallbackConfig"));
            assertEquals(0, primary.calls.get());
        } finally {
            logger.detachAppender(messages);
            messages.stop();
        }
    }

    private AiProxyHandle handle(final Upstream primary, final String extra) {
        return GsonUtils.getInstance().fromJson("{\"provider\":\"OpenAI\",\"baseUrl\":\"" + primary.url()
                + "\",\"apiKey\":\"primary-fixture-key\",\"model\":\"primary\",\"stream\":false" + extra + "}", AiProxyHandle.class);
    }

    private MockServerWebExchange execute(final AiProxyHandle handle, final String fallback) {
        return execute(handle, fallback, "");
    }

    private MockServerWebExchange execute(final AiProxyHandle handle, final String fallback, final String proxyKey) {
        AiProxyPluginHandler handler = new AiProxyPluginHandler();
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE), handle);
        MockServerWebExchange exchange = exchange(fallback, proxyKey);
        StepVerifier.create(run(plugin(handler), exchange)).expectComplete().verify(TIMEOUT);
        return exchange;
    }

    private MockServerWebExchange exchange(final String fallback, final String proxyKey) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/chat").header(Constants.X_API_KEY, proxyKey)
                .body("{" + PAYLOAD + ",\"fallbackConfig\":" + fallback + "}"));
    }

    private Mono<Void> run(final AiProxyPlugin plugin, final MockServerWebExchange exchange) {
        SelectorData selector = new SelectorData();
        selector.setId(SELECTOR_ID);
        return plugin.doExecute(exchange, mock(ShenyuPluginChain.class), selector, new RuleData());
    }

    private AiProxyPlugin plugin(final AiProxyPluginHandler handler) {
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())), new AiProxyProviderFactory(List.of(new OpenAi())),
                protocolRouteResolver(), new WebClientAiProxyTransport(WebClient.builder().build()), new AiProxyRetryProperties(0, 0, Duration.ZERO));
        return new AiProxyPlugin(new AiProxyConfigService(handler), engine, handler);
    }

    private ProxyApiKeyData mapping(final String proxyKey, final String realKey) {
        return ProxyApiKeyData.builder().selectorId(SELECTOR_ID).proxyApiKey(proxyKey).realApiKey(realKey).enabled(true).build();
    }

    private static final class Upstream implements AutoCloseable {

        private final AtomicInteger calls = new AtomicInteger();

        private final AtomicReference<String> authorization = new AtomicReference<>();

        private final AtomicReference<String> body = new AtomicReference<>();

        private final DisposableServer server;

        private Upstream(final int status) {
            this(content -> status, null);
        }

        private Upstream(final Function<String, Integer> status, final String redirect) {
            server = HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
                calls.incrementAndGet();
                String credential = request.requestHeaders().get("Authorization");
                authorization.set(credential);
                return request.receive().aggregate().asString().flatMap(content -> {
                    body.set(content);
                    if (Objects.nonNull(redirect)) {
                        response.header("Location", redirect);
                    }
                    return response.status(status.apply(content)).header("Content-Type", "application/json").header("X-Observed-Key", credential)
                            .sendString(Mono.just("{\"choices\":[]}")).then();
                });
            }).bindNow(TIMEOUT);
        }

        private String url() {
            return "http://127.0.0.1:" + server.port();
        }

        @Override
        public void close() {
            server.disposeNow(TIMEOUT);
        }
    }
}
