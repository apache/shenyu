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

package org.apache.shenyu.springboot.starter.ai.ledger;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.ledger.LedgerPriceBook;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AnthropicMessages;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiProviderOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.MapPropertySource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.server.WebHandler;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Real HTTP producer/exports and upstream, using the existing AI Proxy engine and transport. */
class LedgerNetworkTest {

    private static final Duration WAIT = Duration.ofSeconds(30);

    private static final HttpClient CLIENT = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).connectTimeout(WAIT).build();

    private final ObjectMapper json = new ObjectMapper().findAndRegisterModules();

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void jsonAndSseReuseProductionMeteringWithoutChangingResponseBytes(final boolean stream) throws Exception {
        try (Fixture control = new Fixture(false, Scenario.PRIMARY, stream, 100);
                Fixture enabled = new Fixture(true, Scenario.PRIMARY, stream, 100)) {
            HttpResponse<String> expected = send(control, "POST", "/generate", "token-a");
            HttpResponse<String> actual = send(enabled, "POST", "/generate", "token-a");
            assertEquals(200, actual.statusCode(), actual.body());
            assertEquals(expected.body(), actual.body());
            await().atMost(WAIT).until(() -> enabled.store().diagnostics().retained() == 2);
            UsageLedgerService.Export exported = enabled.service().export(LedgerTestFixtures.scope("a"), 100);
            assertEquals(1, exported.summary().actualAttempts());
            assertEquals(0, new BigDecimal("0.000126").compareTo(exported.summary().costs().get("USD").knownEstimatedSubtotal()));
            assertEquals(1, enabled.calls.get());
            String rows = json.writeValueAsString(exported);
            for (String secret : List.of("provider-secret", "private-prompt", "private-answer", "private-cookie", "token-a")) {
                assertFalse(rows.contains(secret), secret);
            }
        }
    }

    @ParameterizedTest
    @CsvSource({"RETRY,0.000016", "FALLBACK,0.000048"})
    void actualRetryAndFallbackWorkIsCountedOnceBeyondTheFinalResponse(final String scenario, final String expected) throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.valueOf(scenario), false, 100)) {
            assertEquals(200, send(fixture, "POST", "/generate", "token-a").statusCode());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 3);
            UsageLedgerService.Summary result = fixture.service().export(LedgerTestFixtures.scope("a"), 100).summary();
            assertEquals(2, result.actualAttempts());
            assertEquals(2, fixture.calls.get());
            assertEquals(0, new BigDecimal(expected).compareTo(result.costs().get("USD").knownEstimatedSubtotal()));
            assertTrue(result.possibleUnobservedConsumption(), "failed work retains possible unobserved consumption");
        }
    }

    @Test
    void authenticatedExportsRejectTenantInjectionAndNeverReturnAnotherTenantsRows() throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.PRIMARY, false, 100)) {
            assertEquals(200, send(fixture, "POST", "/generate", "token-a").statusCode());
            assertEquals(200, send(fixture, "POST", "/generate", "token-b").statusCode());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 4);
            HttpResponse<String> visible = send(fixture, "GET", "/shenyu/ai/usage-ledger", "token-a");
            assertEquals(200, visible.statusCode(), visible.body());
            JsonNode body = json.readTree(visible.body());
            assertEquals(2, body.path("rows").size());
            body.path("rows").forEach(row -> assertEquals("a", row.path("facts").path("tenant").asText()));
            assertEquals(401, send(fixture, "GET", "/shenyu/ai/usage-ledger", "").statusCode());
            assertEquals(403, send(fixture, "GET", "/shenyu/ai/usage-ledger", "denied").statusCode());
            assertEquals(400, send(fixture, "GET", "/shenyu/ai/usage-ledger?tenant=a", "token-b").statusCode());
            assertEquals(400, send(fixture, "GET", "/shenyu/ai/usage-ledger?limit=1001", "token-a").statusCode());
            assertEquals(400, send(fixture, "GET", "/shenyu/ai/usage-ledger?limit=abc", "token-a").statusCode());
        }
    }

    @Test
    void clientDisconnectKeepsObservedUsageAndDoesNotInventZeroFinalConsumption() throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.CANCEL, true, 100)) {
            var pending = CLIENT.sendAsync(request(fixture, "POST", "/generate", "token-a"), HttpResponse.BodyHandlers.ofString());
            try {
                assertTrue(fixture.forwarded.await(30, TimeUnit.SECONDS));
                assertTrue(pending.cancel(true));
                await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 2);
                UsageLedgerService.Summary result = fixture.service().export(LedgerTestFixtures.scope("a"), 100).summary();
                assertEquals(1, result.actualAttempts());
                assertEquals(1, fixture.calls.get());
                assertTrue(result.possibleUnobservedConsumption(), () -> fixture.service().export(LedgerTestFixtures.scope("a"), 100).rows().toString());
                assertEquals(java.math.BigInteger.valueOf(5), result.knownProviderInput());
            } finally {
                fixture.release.countDown();
                pending.cancel(true);
            }
        }
    }

    @Test
    void fullLedgerDoesNotChangeARealBusinessResponse() throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.PRIMARY, false, 1)) {
            assertEquals(200, send(fixture, "POST", "/generate", "token-a").statusCode());
            assertEquals(200, send(fixture, "POST", "/generate", "token-b").statusCode());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().rejected() >= 3);
            assertEquals(1, fixture.store().diagnostics().retained());
            assertTrue(fixture.context.getBean(UsageLedgerCollector.class).failedRecords() >= 3);
        }
    }

    @ParameterizedTest
    @CsvSource({"MISSING,200", "FAILURE,503"})
    void missingUsageAndFailedResponsesNeverBecomeKnownZero(final String scenario, final int status) throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.valueOf(scenario), false, 100)) {
            assertEquals(status, send(fixture, "POST", "/generate", "token-a").statusCode());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 2);
            UsageLedgerService.Summary result = fixture.service().export(LedgerTestFixtures.scope("a"), 100).summary();
            assertEquals(1, result.actualAttempts());
            assertTrue(result.possibleUnobservedConsumption());
            assertEquals("MISSING".equals(scenario) ? 1 : 0, result.unknownUsageAttempts());
            assertEquals("MISSING".equals(scenario) ? 1 : 0, result.costs().get("USD").unknownAttempts());
        }
    }

    @ParameterizedTest
    @CsvSource({"RESPONSES,false,0.000252", "RESPONSES,true,0.000252", "ANTHROPIC,false,0.000381", "ANTHROPIC,true,0.000381"})
    void nativeProtocolsUseTheirOwnMeasuredPricesAndPreserveWireBytes(final String scenario, final boolean stream, final String estimate) throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.valueOf(scenario), stream, 100);
                Fixture control = new Fixture(false, Scenario.valueOf(scenario), stream, 100)) {
            HttpResponse<String> actual = send(fixture, "POST", "/generate", "token-a");
            assertEquals(200, actual.statusCode(), actual.body());
            assertEquals(send(control, "POST", "/generate", "token-a").body(), actual.body());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 2);
            UsageLedgerService.Export result = fixture.service().export(LedgerTestFixtures.scope("a"), 100);
            assertEquals(0, result.summary().unknownUsageAttempts());
            assertEquals(0, result.summary().costs().get("USD").unknownAttempts());
            assertEquals(0, new BigDecimal(estimate).compareTo(result.summary().costs().get("USD").knownEstimatedSubtotal()));
            assertEquals(fixture.anthropic() ? "anthropic-messages" : "openai-responses", result.rows().get(0).facts().clientProtocol());
            assertEquals(fixture.anthropic() ? "anthropic" : "openai", result.rows().get(0).facts().provider());
            assertEquals(stream, result.summary().possibleUnobservedConsumption());
            String exported = json.writeValueAsString(result);
            assertFalse(exported.contains("private-prompt"));
            assertFalse(exported.contains("private-answer"));
            assertFalse(exported.contains("provider-secret"));
        }
    }

    @ParameterizedTest
    @CsvSource({"RESPONSES_RETRY,0.000256", "ANTHROPIC_RETRY,0.000388"})
    void nativeRetriesIncludeMeasuredFailedPrimaryOnce(final String scenario, final String expected) throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.valueOf(scenario), false, 100)) {
            assertEquals(200, send(fixture, "POST", "/generate", "token-a").statusCode());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 3);
            UsageLedgerService.Summary result = fixture.service().export(LedgerTestFixtures.scope("a"), 100).summary();
            assertEquals(2, result.actualAttempts());
            assertEquals(2, fixture.calls.get());
            assertEquals(0, new BigDecimal(expected).compareTo(result.costs().get("USD").knownEstimatedSubtotal()));
            assertTrue(result.possibleUnobservedConsumption());
        }
    }

    @ParameterizedTest
    @CsvSource({"RESPONSES_CACHED,false", "RESPONSES_CACHED,true", "ANTHROPIC_CACHED,false", "ANTHROPIC_CACHED,true",
        "ANTHROPIC_MISSING,false", "ANTHROPIC_MISSING,true"})
    void nativeUnpricedCacheOrMissingUsageIsNeverARegularTokenQuote(final String scenario, final boolean stream) throws Exception {
        try (Fixture fixture = new Fixture(true, Scenario.valueOf(scenario), stream, 100)) {
            assertEquals(200, send(fixture, "POST", "/generate", "token-a").statusCode());
            await().atMost(WAIT).until(() -> fixture.store().diagnostics().retained() == 2);
            UsageLedgerService.Export result = fixture.service().export(LedgerTestFixtures.scope("a"), 100);
            assertEquals(1, result.summary().costs().get("USD").unknownAttempts());
            assertEquals(scenario.endsWith("MISSING") ? org.apache.shenyu.common.ledger.LedgerRecord.PriceStatus.UNKNOWN_USAGE
                    : org.apache.shenyu.common.ledger.LedgerRecord.PriceStatus.UNSUPPORTED_CATEGORY, result.rows().get(0).estimate().status());
            assertTrue(java.util.Objects.isNull(result.rows().get(0).estimate().amount()));
        }
    }

    private HttpResponse<String> send(final Fixture fixture, final String method, final String path, final String credential) throws Exception {
        return CLIENT.send(request(fixture, method, path, credential), HttpResponse.BodyHandlers.ofString());
    }

    private HttpRequest request(final Fixture fixture, final String method, final String path, final String credential) {
        HttpRequest.Builder request = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + fixture.frontend.port() + path)).timeout(WAIT)
                .header("Authorization", "Bearer " + credential).header("X-Tenant", "forged-tenant").header("Cookie", "private-cookie");
        if ("POST".equals(method)) {
            if (fixture.responses()) {
                return request.header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(
                        "{\"input\":\"private-prompt\",\"stream\":" + fixture.stream + "}")).build();
            }
            return request.header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(
                    "{\"messages\":[{\"role\":\"user\",\"content\":\"private-prompt\"}],\"max_tokens\":64,\"stream\":" + fixture.stream + "}")).build();
        }
        return request.GET().build();
    }

    private enum Scenario {
        PRIMARY, RETRY, FALLBACK, CANCEL, MISSING, FAILURE, RESPONSES, RESPONSES_RETRY, RESPONSES_CACHED,
        ANTHROPIC, ANTHROPIC_RETRY, ANTHROPIC_CACHED, ANTHROPIC_MISSING
    }

    private static final class Fixture implements AutoCloseable {

        private final ObjectMapper json = new ObjectMapper();

        private final AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();

        private final HttpServer upstream;

        private final ExecutorService workers = Executors.newCachedThreadPool();

        private final AtomicInteger calls = new AtomicInteger();

        private final CountDownLatch release = new CountDownLatch(1);

        private final CountDownLatch forwarded = new CountDownLatch(1);

        private final DisposableServer frontend;

        private final boolean stream;

        private final Scenario scenario;

        @SuppressWarnings("unchecked")
        private Fixture(final boolean enabled, final Scenario scenario, final boolean stream, final int capacity) throws Exception {
            this.stream = stream;
            this.scenario = scenario;
            context.getEnvironment().getPropertySources().addFirst(new MapPropertySource("ledger-test", Map.of(
                    "shenyu.ai.usage-ledger.enabled", Boolean.toString(enabled),
                    "shenyu.ai.usage-ledger.exports-enabled", "true",
                    "shenyu.ai.usage-ledger.namespace", "namespace",
                    "shenyu.ai.usage-ledger.store-capacity", Integer.toString(capacity))));
            context.registerBean(LedgerPriceBook.class, LedgerTestFixtures::prices);
            context.registerBean("aiUsageLedgerClock", java.time.Clock.class, () -> LedgerTestFixtures.CLOCK);
            context.registerBean(LedgerExportAuthorizer.class, () -> principal -> "denied".equals(principal.getName())
                    ? Mono.empty() : Mono.just(LedgerTestFixtures.scope(principal.getName())));
            context.register(AiUsageLedgerConfiguration.class);
            context.refresh();
            upstream = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            upstream.createContext("/", this::upstream);
            upstream.setExecutor(workers);
            upstream.start();
            AiProxyRetryProperties retries = new AiProxyRetryProperties(scenario.name().endsWith("RETRY") ? 1 : 0, 0, Duration.ZERO);
            retries.setRequestDeadline(Duration.ofSeconds(20));
            AiCallObserver observer = enabled ? AiCallObserver.select(context.getBeansOfType(AiCallObserver.class).values().stream().toList()) : null;
            AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat(), new OpenAiResponses(), new AnthropicMessages())),
                    new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("anthropic", OpenAiProviderOptions.anthropicMessages()))),
                    new WebClientAiProxyTransport(WebClient.create()), retries, observer);
            WebHandler exports = enabled ? RouterFunctions.toWebHandler((RouterFunction<ServerResponse>) context.getBean("ledgerExports"))
                    : exchange -> exchange.getResponse().setComplete();
            WebHandler routes = exchange -> {
                if (exchange.getRequest().getPath().value().equals("/shenyu/ai/usage-ledger")) {
                    return exports.handle(exchange);
                }
                return exchange.getPrincipal().switchIfEmpty(Mono.error(new SecurityException()))
                        .flatMap(principal -> DataBufferUtils.join(exchange.getRequest().getBody()).flatMap(buffer -> {
                            String body = buffer.toString(StandardCharsets.UTF_8);
                            DataBufferUtils.release(buffer);
                            AgentRequestContext trusted = LedgerTestFixtures.context(UUID.randomUUID().toString(), principal.getName());
                            return engine.execute(body, config("main"), scenario == Scenario.FALLBACK ? Optional.of(config("fallback")) : Optional.empty(),
                                    response -> {
                                        exchange.getResponse().setStatusCode(org.springframework.http.HttpStatusCode.valueOf(response.getStatusCode()));
                                        response.getHeaders().forEach((name, values) -> {
                                            if ("content-type".equalsIgnoreCase(name)) {
                                                exchange.getResponse().getHeaders().put(name, values);
                                            }
                                        });
                                        return exchange.getResponse().writeWith(response.getBody().doOnNext(bytes -> forwarded.countDown())
                                                .map(exchange.getResponse().bufferFactory()::wrap));
                                    }).contextWrite(value -> value.put(AgentRequestContext.REACTOR_KEY, trusted));
                        })).onErrorResume(SecurityException.class, error -> {
                            exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                            return exchange.getResponse().setComplete();
                        });
            };
            ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(routes).filter((exchange, chain) -> {
                String token = exchange.getRequest().getHeaders().getFirst("Authorization");
                String tenant = Map.of("Bearer token-a", "a", "Bearer token-b", "b", "Bearer denied", "denied").get(Objects.requireNonNullElse(token, ""));
                return chain.filter(Objects.isNull(tenant) ? exchange : exchange.mutate().principal(Mono.just((Principal) () -> tenant)).build());
            }).build());
            frontend = reactor.netty.http.server.HttpServer.create().host("127.0.0.1").port(0).handle(adapter).bindNow(WAIT);
        }

        private AiCommonConfig config(final String model) {
            AiCommonConfig config = new AiCommonConfig();
            config.setProvider(anthropic() ? "anthropic" : "OpenAI");
            if (responses()) {
                config.setProtocol(OpenAiResponses.NAME);
            } else if (anthropic()) {
                config.setProtocol(AnthropicMessages.NAME);
            }
            config.setBaseUrl("http://127.0.0.1:" + upstream.getAddress().getPort() + "/v1");
            config.setApiKey("provider-secret");
            config.setModel(model);
            config.setStream(stream);
            return config;
        }

        private void upstream(final HttpExchange exchange) throws IOException {
            try (exchange) {
                JsonNode request = json.readTree(exchange.getRequestBody());
                if (anthropic()) {
                    assertEquals("provider-secret", exchange.getRequestHeaders().getFirst("x-api-key"));
                    assertEquals("2023-06-01", exchange.getRequestHeaders().getFirst("anthropic-version"));
                    assertTrue(java.util.Objects.isNull(exchange.getRequestHeaders().getFirst("Authorization")));
                    assertEquals("/v1/messages", exchange.getRequestURI().getPath());
                    assertFalse(request.has("stream_options"));
                } else {
                    assertEquals("Bearer provider-secret", exchange.getRequestHeaders().getFirst("Authorization"));
                }
                int call = calls.incrementAndGet();
                String model = request.path("model").asText();
                int status = scenario == Scenario.FAILURE || scenario.name().endsWith("RETRY") && call == 1 || scenario == Scenario.FALLBACK && "main".equals(model) ? 503 : 200;
                long input = status != 200 ? 1 : scenario == Scenario.CANCEL ? 5 : scenario == Scenario.RETRY ? 2 : 3;
                long output = status != 200 || scenario == Scenario.CANCEL ? 0 : scenario == Scenario.RETRY ? 1 : scenario == Scenario.FALLBACK ? 4 : 12;
                var result = json.createObjectNode().put("id", "fixture-response").put("object", stream ? "chat.completion.chunk" : "chat.completion").put("model", model);
                var choice = result.putArray("choices").addObject().put("index", 0);
                choice.putObject(stream ? "delta" : "message").put("role", "assistant").put("content", "private-answer");
                result.putObject("usage").put("prompt_tokens", input).put("completion_tokens", output).put("total_tokens", input + output);
                if (scenario == Scenario.MISSING) {
                    result.remove("usage");
                }
                String wire = stream ? "data: " + result + "\n\n" + (scenario == Scenario.CANCEL ? "" : "data: [DONE]\n\n") : result.toString();
                if (responses()) {
                    var response = json.createObjectNode().put("id", "response-fixture").put("object", "response").put("status", "completed");
                    response.putArray("output");
                    response.putObject("usage").put("input_tokens", input).put("output_tokens", output).put("total_tokens", input + output);
                    response.with("usage").putObject("input_tokens_details").put("cached_tokens", scenario == Scenario.RESPONSES_CACHED ? 1 : 0);
                    wire = stream ? "event: response.completed\ndata: " + json.createObjectNode().put("type", "response.completed")
                            .set("response", response) + "\n\n" : response.toString();
                }
                if (anthropic()) {
                    var response = json.createObjectNode().put("id", "message-fixture").put("type", "message").put("role", "assistant");
                    response.putArray("content").addObject().put("type", "text").put("text", "private-answer");
                    response.putObject("usage").put("input_tokens", input).put("output_tokens", stream ? 0 : output)
                            .put("cache_read_input_tokens", scenario == Scenario.ANTHROPIC_CACHED ? 5 : 0).put("cache_creation_input_tokens", 0);
                    if (scenario == Scenario.ANTHROPIC_MISSING) {
                        response.remove("usage");
                    }
                    wire = stream ? "event: message_start\ndata: " + json.createObjectNode().put("type", "message_start").set("message", response) + "\n\n"
                            + (scenario == Scenario.ANTHROPIC_MISSING ? "" : "event: message_delta\ndata: {\"type\":\"message_delta\",\"usage\":{\"output_tokens\":" + output + "}}\n\n")
                            + "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n" : response.toString();
                }
                byte[] bytes = wire.getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", stream ? "text/event-stream" : "application/json");
                exchange.sendResponseHeaders(status, scenario == Scenario.CANCEL ? 0 : bytes.length);
                exchange.getResponseBody().write(bytes);
                exchange.getResponseBody().flush();
                if (scenario == Scenario.CANCEL) {
                    try {
                        release.await(30, TimeUnit.SECONDS);
                    } catch (InterruptedException error) {
                        Thread.currentThread().interrupt();
                    }
                }
            }
        }

        private UsageLedgerStore store() {
            return context.getBean(UsageLedgerStore.class);
        }

        private boolean anthropic() {
            return scenario.name().startsWith("ANTHROPIC");
        }

        private boolean responses() {
            return scenario.name().startsWith("RESPONSES");
        }

        private UsageLedgerService service() {
            return context.getBean(UsageLedgerService.class);
        }

        @Override
        public void close() {
            release.countDown();
            frontend.disposeNow(WAIT);
            upstream.stop(0);
            workers.shutdownNow();
            context.close();
        }
    }
}
