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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import io.opentelemetry.api.trace.SpanKind;
import io.opentelemetry.sdk.OpenTelemetrySdk;
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter;
import io.opentelemetry.sdk.trace.SdkTracerProvider;
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsRegister;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import io.prometheus.client.CollectorRegistry;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayPlugin;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.ai.prompt.AiPromptPlugin;
import org.apache.shenyu.plugin.ai.prompt.handler.AiPromptPluginDataHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.ai.token.limiter.AiTokenLimiterPlugin;
import org.apache.shenyu.plugin.ai.token.limiter.handler.AiTokenLimiterPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.key.auth.KeyAuthPlugin;
import org.apache.shenyu.plugin.key.auth.handler.KeyAuthPluginDataHandler;
import org.apache.shenyu.plugin.tracing.TracingPlugin;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.ReactiveValueOperations;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Loopback HTTP through actual sorted plugin matching and real AI transport.
 * Redis is a controlled fixture, not a distributed-budget acceptance test.
 */
class AgentGovernanceHttpTest {

    @Test
    void invalidKeyIsCountedBeforeAgentRoutingWithoutCreatingALogicalAiCall() {
        try (Fixture fixture = new Fixture(false, false)) {
            fixture.request("wrong").expectBody().consumeWith(ignored -> { });
            assertEquals(0, fixture.upstreamCalls.get());
            assertEquals(0, fixture.terminals.size());
            assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AUTHENTICATION_REJECTIONS_TOTAL,
                    new String[]{"source", "reason"}, new String[]{"key_auth", "credential_rejected"}));
            assertNull(CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_REQUESTS_TOTAL,
                    new String[]{"traffic", "outcome", "reason"}, new String[]{"llm", "rejected", "authentication"}));
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void keyAuthToLimiterToProxyHasOneTerminalAndOneBodySubscription(final boolean stream) {
        try (Fixture fixture = new Fixture(stream, false)) {
            String response = fixture.request().expectStatus().isOk().expectBody(String.class).returnResult().getResponseBody();
            assertEquals(fixture.response, response);
            await().atMost(Duration.ofSeconds(10)).untilAsserted(() -> assertEquals(1, fixture.terminals.size()));
            assertEquals(1, fixture.attempts.size());
            assertEquals(1, fixture.upstreamCalls.get());
            assertEquals(1, fixture.bodySubscriptions.get());
            AiTerminalEvent event = fixture.terminals.element();
            assertEquals(AiCallOutcome.SUCCESS, event.outcome());
            assertEquals("tenant", event.context().getTenant());
            assertEquals("service", event.context().getSubject());
            assertEquals("model", event.context().getTarget());
            assertEquals(event.context().getRequestId(), fixture.limiterContext.get().getRequestId());
            assertEquals(1, event.attemptCount());
            assertEquals(3L, event.usage().promptTokens());
            assertEquals(2L, event.usage().completionTokens());
            assertEquals(5L, event.usage().totalTokens());
            assertNull(event.context().getSessionId());
            assertEquals(event.context().getRequestId(), fixture.attempts.element().context().getRequestId());
            assertEquals(List.of("tracing", "keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "aiProxy"), fixture.handler.getPlugins().stream().map(ShenyuPlugin::named).toList());
            await().atMost(Duration.ofSeconds(10)).untilAsserted(() -> assertEquals(1, fixture.exporter.getFinishedSpanItems().size()));
            var span = fixture.exporter.getFinishedSpanItems().get(0);
            assertEquals(SpanKind.SERVER, span.getKind());
            assertEquals("12345678901234567890123456789012", span.getTraceId());
            assertEquals("1234567890123456", span.getParentSpanId());
            assertEquals("00-" + span.getTraceId() + "-" + span.getSpanId() + "-01", fixture.outboundParent.get());
            await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> assertEquals(0.0, CollectorRegistry.defaultRegistry.getSampleValue(
                    LabelNames.AGENT_REQUESTS_INFLIGHT, new String[]{"traffic"}, new String[]{"llm"})));
            assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_REQUESTS_TOTAL,
                    new String[]{"traffic", "outcome", "reason"}, new String[]{"llm", "success", "none"}));
            assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_AI_ATTEMPTS_TOTAL,
                    new String[]{"kind", "outcome"}, new String[]{"primary", "success"}));
        }
    }

    @Test
    void tokenDenialHasTrustedContextWithoutCallingModel() {
        try (Fixture fixture = new Fixture(false, true)) {
            fixture.request().expectStatus().isEqualTo(429).expectBody().consumeWith(ignored -> { });
            await().atMost(Duration.ofSeconds(10)).untilAsserted(() -> assertEquals(1, fixture.terminals.size()));
            AiTerminalEvent event = fixture.terminals.element();
            assertEquals(AiCallOutcome.RATE_LIMITED, event.outcome());
            assertEquals("tenant", event.context().getTenant());
            assertEquals(0, event.attemptCount());
            assertEquals(AiUsage.UNKNOWN, event.usage());
            await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(
                    LabelNames.AGENT_REQUESTS_TOTAL, new String[]{"traffic", "outcome", "reason"}, new String[]{"llm", "rate_limited", "resource_limit"})));
            assertNull(CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.AGENT_AI_ATTEMPTS_TOTAL,
                    new String[]{"kind", "outcome"}, new String[]{"primary", "success"}));
            assertEquals(1, fixture.bodySubscriptions.get());
            assertEquals(0, fixture.upstreamCalls.get());
            assertTrue(fixture.attempts.isEmpty());
        }
    }

    private static final class Fixture implements AutoCloseable {

        private final LoopResources loops = LoopResources.create("governance-http", 2, true);

        private final ConcurrentLinkedQueue<AiTerminalEvent> terminals = new ConcurrentLinkedQueue<>();

        private final ConcurrentLinkedQueue<AiAttemptEvent> attempts = new ConcurrentLinkedQueue<>();

        private final AtomicInteger upstreamCalls = new AtomicInteger();

        private final AtomicInteger bodySubscriptions = new AtomicInteger();

        private final AtomicReference<AgentRequestContext> limiterContext = new AtomicReference<>();

        private final AtomicReference<String> outboundParent = new AtomicReference<>();

        private final InMemorySpanExporter exporter = InMemorySpanExporter.create();

        private final SdkTracerProvider tracerProvider = SdkTracerProvider.builder().addSpanProcessor(SimpleSpanProcessor.create(exporter)).build();

        private final OpenTelemetrySdk telemetry = OpenTelemetrySdk.builder().setTracerProvider(tracerProvider).build();

        private final DisposableServer upstream;

        private final DisposableServer gateway;

        private final ShenyuWebHandler handler;

        private final String response;

        private final boolean stream;

        private final AiProxyPluginHandler proxyHandler = new AiProxyPluginHandler();

        private final AiPromptPluginDataHandler promptHandler = new AiPromptPluginDataHandler();

        private final KeyAuthPluginDataHandler keyHandler = new KeyAuthPluginDataHandler();

        private final AgentGatewayPluginDataHandler agentHandler = new AgentGatewayPluginDataHandler();

        @SuppressWarnings("unchecked")
        private Fixture(final boolean stream, final boolean deny) {
            this.stream = stream;
            String json = "{\"choices\":[],\"usage\":{\"prompt_tokens\":3,\"completion_tokens\":2,\"total_tokens\":5}}";
            response = stream ? "data: " + json + "\n\ndata: [DONE]\n\n" : json;
            upstream = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle((request, result) -> {
                upstreamCalls.incrementAndGet();
                outboundParent.set(request.requestHeaders().get("traceparent"));
                assertNull(request.requestHeaders().get("baggage"));
                assertNull(request.requestHeaders().get("X-Tenant-Id"));
                assertNull(request.requestHeaders().get("X-Subject"));
                assertNull(request.requestHeaders().get("X-Api-Key"));
                assertEquals("Bearer upstream-service-key", request.requestHeaders().get("Authorization"));
                return request.receive().then(result.header("Content-Type", stream ? "text/event-stream" : "application/json")
                        .sendString(Flux.just(response.substring(0, 29), response.substring(29))).then());
            }).bindNow(Duration.ofSeconds(20));
            ShenyuConfig config = new ShenyuConfig();
            config.getSelectorMatchCache().getCache().setEnabled(false);
            config.getRuleMatchCache().getCache().setEnabled(false);
            ConfigurableApplicationContext application = mock(ConfigurableApplicationContext.class);
            when(application.getBean(ShenyuConfig.class)).thenReturn(config);
            when(application.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
            SpringBeanUtils.getInstance().setApplicationContext(application);
            KeyAuthPlugin key = new KeyAuthPlugin();
            RuleData keyRule = register(key, "{\"keyName\":\"X-Api-Key\",\"key\":\"fixture-key\"}");
            keyHandler.handlerRule(keyRule);
            final KeyAuthMcpSecurityProperties properties = new KeyAuthMcpSecurityProperties();
            KeyAuthMcpSecurityProperties.Binding binding = new KeyAuthMcpSecurityProperties.Binding();
            binding.setNamespaceId("namespace");
            binding.setSelectorId(keyRule.getSelectorId());
            binding.setRuleId(keyRule.getId());
            binding.setTenant("tenant");
            binding.setSubject("service");
            binding.setToolGrants(Set.of());
            binding.setToolDenials(Set.of());
            properties.setBindings(List.of(binding));
            KeyAuthMcpSecurityResolver resolver = new KeyAuthMcpSecurityResolver(properties);
            AiCallObserver observer = new AiCallObserver() {
                @Override
                public void record(final AiTerminalEvent event) {
                    terminals.add(event);
                }

                @Override
                public void recordAttempt(final AiAttemptEvent event) {
                    attempts.add(event);
                }
            };
            AgentGatewayPlugin agent = new AgentGatewayPlugin(new AgentMcpHttpHandler(
                    new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "fixture", "1"), resolver), resolver, observer);
            agentHandler.handlerRule(register(agent, "{\"trafficType\":\"LLM\"}"));
            AiPromptPlugin prompt = new AiPromptPlugin(ServerCodecConfigurer.create().getReaders());
            RuleData promptRule = register(prompt, "{\"prepend\":\"governed\",\"preRole\":\"system\"}");
            promptHandler.handlerRule(promptRule);
            AiTokenLimiterPlugin limiter = new AiTokenLimiterPlugin();
            new AiTokenLimiterPluginHandler().handlerRule(register(limiter, "{\"aiTokenLimitType\":\"uri\",\"keyName\":\"unused\",\"tokenLimit\":100,\"timeWindowSeconds\":60}"));
            ReactiveRedisTemplate<String, String> redis = mock(ReactiveRedisTemplate.class);
            ReactiveValueOperations<String, String> values = mock(ReactiveValueOperations.class);
            when(redis.opsForValue()).thenReturn(values);
            when(values.get(anyString())).thenReturn(Mono.deferContextual(context -> {
                limiterContext.set(context.get(AgentRequestContext.REACTOR_KEY));
                return Mono.just(deny ? "100" : "0");
            }));
            when(values.setIfAbsent(anyString(), anyString(), any(Duration.class))).thenReturn(Mono.just(true));
            when(values.increment(anyString(), anyLong())).thenReturn(Mono.just(2L));
            AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().cachedHandle(limiter.named(), redis);
            AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                    new AiProxyProviderFactory(List.of(new OpenAi())), new WebClientAiProxyTransport(WebClient.builder().build()), Duration.ZERO);
            AiProxyPlugin proxy = new AiProxyPlugin(new AiProxyConfigService(proxyHandler), engine, proxyHandler);
            final RuleData proxyRule = register(proxy, "{}");
            AiProxyHandle handle = new AiProxyHandle();
            handle.setProvider("OpenAI");
            handle.setBaseUrl("http://127.0.0.1:" + upstream.port());
            handle.setApiKey("upstream-service-key");
            handle.setModel("model");
            proxyHandler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(proxyRule.getSelectorId(), Constants.DEFAULT_RULE), handle);
            TracingPlugin tracing = new TracingPlugin(telemetry.getTracer("governance-fixture"));
            handler = new ShenyuWebHandler(List.of(proxy, limiter, prompt, agent, key, tracing), mock(ShenyuLoaderService.class), config);
            handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, BaseDataCache.getInstance().obtainPluginData(agent.named())));
            MetricsReporter.clean();
            CollectorRegistry.defaultRegistry.clear();
            MetricsReporter.register(new PrometheusMetricsRegister());
            gateway = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle(new ReactorHttpHandlerAdapter(
                    WebHttpHandlerBuilder.webHandler(exchange -> {
                        exchange.getAttributes().put(AgentRuntimeMetrics.KEY, MetricsReporter.agentRuntimeMetrics());
                        org.springframework.http.server.reactive.ServerHttpRequestDecorator request = new org.springframework.http.server.reactive.ServerHttpRequestDecorator(exchange.getRequest()) {
                            @Override
                            public Flux<DataBuffer> getBody() {
                                return Flux.defer(() -> {
                                    bodySubscriptions.incrementAndGet();
                                    return super.getBody();
                                });
                            }
                        };
                        return handler.handle(exchange.mutate().request(request).build());
                    }).build())).bindNow(Duration.ofSeconds(20));
        }

        private RuleData register(final ShenyuPlugin plugin, final String handle) {
            PluginData data = new PluginData();
            data.setName(plugin.named());
            data.setEnabled(true);
            data.setSort(plugin.getOrder());
            BaseDataCache cache = BaseDataCache.getInstance();
            cache.cachePluginData(data);
            SelectorData selector = SelectorData.builder().id("governance-" + plugin.named()).pluginName(plugin.named()).enabled(true).continued(true)
                    .logged(false).sort(0).type(0).matchMode(0).build();
            cache.cacheSelectData(selector);
            RuleData rule = RuleData.builder().id(selector.getId() + "-rule").selectorId(selector.getId()).pluginName(plugin.named())
                    .namespaceId("namespace").enabled(true).logged(false).sort(0).matchMode(0).handle(handle).build();
            cache.cacheRuleData(rule);
            return rule;
        }

        private WebTestClient.ResponseSpec request() {
            return request("fixture-key");
        }

        private WebTestClient.ResponseSpec request(final String key) {
            return WebTestClient.bindToServer().baseUrl("http://127.0.0.1:" + gateway.port()).responseTimeout(Duration.ofSeconds(20)).build()
                    .post().uri("/chat").header("X-Api-Key", key).header("X-Tenant-Id", "forged").header("X-Subject", "forged")
                    .header("traceparent", "00-12345678901234567890123456789012-1234567890123456-01").header("baggage", "private=never-forward")
                    .header("Content-Type", "application/json").bodyValue("{\"messages\":[{\"role\":\"user\",\"content\":\"private\"}],\"stream\":" + stream + "}").exchange();
        }

        @Override
        public void close() {
            gateway.disposeNow(Duration.ofSeconds(10));
            upstream.disposeNow(Duration.ofSeconds(10));
            loops.disposeLater().block(Duration.ofSeconds(10));
            telemetry.close();
            MetricsReporter.clean();
            CollectorRegistry.defaultRegistry.clear();
            BaseDataCache cache = BaseDataCache.getInstance();
            for (String name : List.of("keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "aiProxy")) {
                cache.removeRuleDataBySelectorId("governance-" + name);
                cache.removeSelectDataByPluginName(name);
                cache.removePluginDataByPluginName(name);
            }
            KeyAuthPluginDataHandler.CACHED_HANDLE.get().removeHandle("governance-keyAuth_governance-keyAuth-rule");
            AgentGatewayPluginDataHandler.CACHED_HANDLE.get().removeHandle("governance-agentGateway_governance-agentGateway-rule");
            promptHandler.CACHED_HANDLE.get().removeHandle("governance-aiPrompt_governance-aiPrompt-rule");
            AiTokenLimiterPluginHandler.CACHED_HANDLE.get().removeHandle("governance-aiTokenLimiter_governance-aiTokenLimiter-rule");
            AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().removeHandle("aiTokenLimiter");
            proxyHandler.getSelectorCachedHandle().removeHandle(CacheKeyUtils.INST.getKey("governance-aiProxy", Constants.DEFAULT_RULE));
        }
    }
}
