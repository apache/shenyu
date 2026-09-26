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

package org.apache.shenyu.plugin.metrics;

import io.prometheus.client.CollectorRegistry;
import org.apache.shenyu.common.config.ShenyuConfig.MetricsConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.CanaryContext;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.metrics.constant.CanaryMetric;
import org.apache.shenyu.plugin.metrics.constant.LabelNames;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsRegister;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsService;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

/**
 * Verifies exported samples across routing, commit, error, cancellation and retry boundaries.
 */
class CanaryMetricsPluginTest {

    private final MetricsPlugin plugin = new MetricsPlugin();

    private MockServerWebExchange exchange;

    @BeforeEach
    void setUp() {
        MetricsReporter.clean();
        new PrometheusMetricsRegister().clean();
        CollectorRegistry.defaultRegistry.clear();
        MetricsReporter.register(new PrometheusMetricsRegister());
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        ShenyuContext context = new ShenyuContext();
        context.setStartDateTime(LocalDateTime.now().minusSeconds(1));
        context.setRpcType("http");
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }

    @AfterEach
    void clean() {
        MetricsReporter.clean();
        CollectorRegistry.defaultRegistry.clear();
    }

    @ParameterizedTest
    @ValueSource(ints = {200, 204, 302, 404, 503})
    void recordsHttpOutcomeAndFractionalDecision(final int status) {
        plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            current.getResponse().setStatusCode(HttpStatus.valueOf(status));
            return current.getResponse().setComplete();
        }).block();
        assertRequest("canary", status < 400 ? "success" : "error");
        assertEquals(0.00008, ruleSample(CanaryMetric.DECISION_DURATION, "_sum"), 0.000000001);
        assertEquals(1.0, ruleSample(CanaryMetric.DECISION_DURATION, "_count"));
        assertEquals(1.0, latency("canary", "_count"));
        assertTrue(latency("canary", "_sum") >= 1000.0);
        assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(CanaryMetric.DECISION_DURATION.getName() + "_bucket",
                new String[]{"selector", "rule", "le"}, new String[]{"selector-a", "rule-a", "1.0E-4"}));
    }

    @Test
    void recordsFallbackAtRoutingBeforeTheRequestFinishesAndOnlyOnce() {
        StepVerifier.create(plugin.execute(exchange, current -> {
            CanaryContext context = context("stable", CanaryContext.CANARY_POOL_EMPTY, null);
            publish(context);
            publish(context);
            assertEquals(1.0, fallback());
            assertEquals(1.0, ruleSample(CanaryMetric.DECISION_DURATION, "_count"));
            return Mono.never();
        })).thenCancel().verify();
        assertRequest("stable", "cancelled");
        assertEquals(1.0, fallback());
        assertNull(latency("stable", "_count"));
    }

    @ParameterizedTest
    @ValueSource(strings = {CanaryContext.CANARY_POOL_EMPTY, CanaryContext.STABLE_POOL_EMPTY, CanaryContext.NO_UPSTREAM_SELECTED})
    void recordsExplicitRejectionEvenWhenHttpStatusDefaultsToSuccess(final String reason) {
        plugin.execute(exchange, current -> {
            publish(context(null, null, reason));
            return current.getResponse().setComplete();
        }).block();
        assertRequest("canary", "reject");
        assertNull(fallback());
    }

    @Test
    void recordsStableIntentRejectionAsStable() {
        plugin.execute(exchange, current -> {
            publish(new CanaryContext("selector-a", "rule-a", "stable", null, null, CanaryContext.STABLE_POOL_EMPTY, 80_000));
            return current.getResponse().setComplete();
        }).block();
        assertRequest("stable", "reject");
    }

    @Test
    void recordsErrorBeforeAnOuterHandlerCommitsItsResponse() {
        IllegalStateException failure = new IllegalStateException("backend failed");
        plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return Mono.error(failure);
        }).onErrorResume(error -> {
            assertEquals(failure, error);
            exchange.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
            return exchange.getResponse().setComplete();
        }).block();
        assertRequest("canary", "error");
        assertEquals(1.0, latency("canary", "_count"));
    }

    @Test
    void capturesDownstreamPluginFailureFromDeferredChain() {
        IllegalStateException failure = new IllegalStateException("selection failed");
        StepVerifier.create(plugin.execute(exchange, current -> Mono.defer(() -> {
            publish(context(null, null, null));
            throw failure;
        }))).expectErrorMatches(error -> error == failure).verify();
        assertRequest("canary", "error");
        assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.REQUEST_THROW_TOTAL));
        assertNull(latency("canary", "_count"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void committedHeadersWaitForChainTerminationToRecordLatencyAndOutcome(final boolean bodyFails) {
        Mono<Void> result = plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return current.getResponse().setComplete().then(Mono.defer(() -> {
                assertTrue(current.getResponse().isCommitted());
                assertNull(latency("canary", "_count"));
                assertNull(request("canary", "success"));
                return bodyFails ? Mono.error(new IllegalStateException("response body failed")) : Mono.empty();
            }));
        });
        if (bodyFails) {
            StepVerifier.create(result).expectError(IllegalStateException.class).verify();
        } else {
            StepVerifier.create(result).verifyComplete();
        }
        assertRequest("canary", bodyFails ? "error" : "success");
        assertNull(request("canary", bodyFails ? "success" : "error"));
        assertEquals(1.0, latency("canary", "_count"));
    }

    @Test
    void cancellationAfterCommitRecordsCancelledRequestWithoutLatency() {
        Disposable subscription = plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return current.getResponse().setComplete().then(Mono.never());
        }).subscribe();
        assertTrue(exchange.getResponse().isCommitted());
        subscription.dispose();
        assertRequest("canary", "cancelled");
        assertNull(request("canary", "success"));
        assertNull(latency("canary", "_count"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void internalRetriesProduceOneFinalRequest(final boolean eventuallySucceeds) {
        AtomicInteger attempts = new AtomicInteger();
        Mono<Void> result = plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return Mono.<Void>defer(() -> {
                int attempt = attempts.incrementAndGet();
                return eventuallySucceeds && attempt == 3 ? current.getResponse().setComplete() : Mono.error(new IllegalStateException("retry"));
            }).retry(2);
        });
        if (eventuallySucceeds) {
            StepVerifier.create(result).verifyComplete();
        } else {
            StepVerifier.create(result).expectError(IllegalStateException.class).verify();
        }
        assertEquals(3, attempts.get());
        assertRequest("canary", eventuallySucceeds ? "success" : "error");
        assertEquals(1.0, ruleSample(CanaryMetric.DECISION_DURATION, "_count"));
        assertNull(fallback());
    }

    @Test
    void reenteringMetricsWithTheSameExchangeDoesNotDuplicateCanarySamples() {
        plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return plugin.execute(current, inner -> inner.getResponse().setComplete());
        }).block();
        assertRequest("canary", "success");
        assertEquals(1.0, ruleSample(CanaryMetric.DECISION_DURATION, "_count"));
        assertEquals(1.0, latency("canary", "_count"));
    }

    @Test
    void legacyRequestOnlyRecordsExistingMetrics() {
        plugin.execute(exchange, current -> current.getResponse().setComplete()).block();
        assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.REQUEST_TOTAL));
        assertEquals(1.0, latency("none", "_count"));
        assertNull(latency("canary", "_count"));
        assertNull(ruleSample(CanaryMetric.DECISION_DURATION, "_count"));
        assertNull(request("canary", "success"));
    }

    @Test
    void existingLatencyHistogramSeparatesStableAndCanary() {
        plugin.execute(exchange, current -> {
            publish(context("stable", CanaryContext.CANARY_POOL_EMPTY, null));
            return current.getResponse().setComplete();
        }).block();
        setUpExchange();
        plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return current.getResponse().setComplete();
        }).block();
        assertEquals(1.0, latency("stable", "_count"));
        assertEquals(1.0, latency("canary", "_count"));
        assertNull(latency("none", "_count"));
        assertEquals(1.0, fallback());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void distinctSelectorsOrRulesKeepSeparateCounters(final boolean differentSelector) {
        plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return current.getResponse().setComplete();
        }).block();
        setUpExchange();
        String selector = differentSelector ? "selector-b" : "selector-a";
        String rule = differentSelector ? "rule-a" : "rule-b";
        plugin.execute(exchange, current -> {
            publish(new CanaryContext(selector, rule, "canary", "canary", null, null, 80_000));
            current.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
            return current.getResponse().setComplete();
        }).block();
        assertRequest("canary", "success");
        assertNull(request("canary", "error"));
        assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(CanaryMetric.REQUESTS.getName(),
                new String[]{"selector", "rule", "partition", "outcome"}, new String[]{selector, rule, "canary", "error"}));
        assertNull(CollectorRegistry.defaultRegistry.getSampleValue(CanaryMetric.REQUESTS.getName(),
                new String[]{"selector", "rule", "partition", "outcome"}, new String[]{selector, rule, "canary", "success"}));
        assertEquals(1.0, ruleSample(CanaryMetric.DECISION_DURATION, "_count"));
        assertEquals(1.0, CollectorRegistry.defaultRegistry.getSampleValue(CanaryMetric.DECISION_DURATION.getName() + "_count",
                new String[]{"selector", "rule"}, new String[]{selector, rule}));
        assertEquals(2.0, latency("canary", "_count"));
    }

    @Test
    void httpExportKeepsCanaryLabelsBoundedAcrossDifferentRequests() throws Exception {
        for (int index = 0; index < 3; index++) {
            exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/sensitive-path-" + index)
                    .header("Authorization", "Bearer sensitive-token-" + index)
                    .header("X-Request-ID", "sensitive-request-" + index)
                    .header("X-Sticky-Key", "sensitive-sticky-" + index)
                    .cookie(new HttpCookie("session", "sensitive-cookie-" + index)));
            ShenyuContext context = new ShenyuContext();
            context.setStartDateTime(LocalDateTime.now().minusSeconds(1));
            context.setRpcType("http");
            exchange.getAttributes().put(Constants.CONTEXT, context);
            plugin.execute(exchange, current -> {
                publish(context("stable", CanaryContext.CANARY_POOL_EMPTY, null));
                return current.getResponse().setComplete();
            }).block();
        }
        MetricsConfig config = new MetricsConfig();
        config.setHost("127.0.0.1");
        config.setPort(0);
        config.setProps(new Properties());
        PrometheusMetricsService service = new PrometheusMetricsService();
        try {
            service.start(config, new PrometheusMetricsRegister());
            assertNotNull(service.getServer());
            URI uri = URI.create("http://127.0.0.1:" + service.getServer().getPort() + "/metrics");
            HttpRequest request = HttpRequest.newBuilder(uri).timeout(Duration.ofSeconds(5)).GET().build();
            HttpResponse<String> response = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build()
                    .send(request, HttpResponse.BodyHandlers.ofString());
            assertEquals(200, response.statusCode());
            List<String> canarySamples = response.body().lines().filter(line -> line.startsWith("shenyu_canary_")).toList();
            assertFalse(canarySamples.isEmpty());
            assertFalse(String.join("\n", canarySamples).contains("sensitive-"));
            List<String> requests = canarySamples.stream().filter(line -> line.startsWith("shenyu_canary_requests_total{")).toList();
            assertEquals(1, requests.size());
            assertTrue(requests.get(0).contains("selector=\"selector-a\""));
            assertTrue(requests.get(0).contains("rule=\"rule-a\""));
            assertTrue(requests.get(0).contains("partition=\"stable\""));
            assertTrue(requests.get(0).contains("outcome=\"success\""));
            assertEquals(4, requests.get(0).chars().filter(character -> character == '=').count());
            assertTrue(requests.get(0).endsWith(" 3.0"));
            assertTrue(canarySamples.stream().anyMatch(line -> line.startsWith("shenyu_canary_fallback_total{")
                    && line.contains("reason=\"canary_pool_empty\"") && line.endsWith(" 3.0")));
            assertTrue(canarySamples.stream().anyMatch(line -> line.startsWith("shenyu_canary_decision_duration_seconds_count{") && line.endsWith(" 3.0")));
            assertTrue(response.body().lines().anyMatch(line -> line.startsWith("shenyu_execute_latency_millis_count{")
                    && line.contains("partition=\"stable\"") && line.endsWith(" 3.0")));
            String decisionSum = canarySamples.stream().filter(line -> line.startsWith("shenyu_canary_decision_duration_seconds_sum{")).findFirst().orElseThrow();
            assertEquals(0.00024, Double.parseDouble(decisionSum.substring(decisionSum.lastIndexOf(' ') + 1)), 0.000000001);
        } finally {
            service.stop();
        }
    }

    @Test
    void missingReporterDoesNotAffectTheResponse() {
        MetricsReporter.clean();
        StepVerifier.create(plugin.execute(exchange, current -> {
            publish(context("canary", null, null));
            return current.getResponse().setComplete();
        })).verifyComplete();
        assertTrue(exchange.getResponse().isCommitted());
    }

    @Test
    void reporterFailurePropagatesFromRequestCounter() {
        MetricsRegister failing = mock(MetricsRegister.class);
        MetricsReporter.register(failing);
        IllegalStateException failure = new IllegalStateException("metrics unavailable");
        doThrow(failure).when(failing).counterIncrement(anyString(), any(), anyLong());
        assertSame(failure, assertThrows(IllegalStateException.class,
                () -> plugin.execute(exchange, current -> current.getResponse().setComplete())));
    }

    private void setUpExchange() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        ShenyuContext context = new ShenyuContext();
        context.setRpcType("http");
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }

    private CanaryContext context(final String actual, final String fallbackReason, final String rejectReason) {
        return new CanaryContext("selector-a", "rule-a", "canary", actual, fallbackReason, rejectReason, 80_000);
    }

    private void publish(final CanaryContext context) {
        exchange.getAttributes().put(Constants.SHENYU_CANARY_CONTEXT, context);
        Consumer<CanaryContext> consumer = exchange.getAttribute(Constants.METRICS_CANARY);
        consumer.accept(context);
    }

    private void assertRequest(final String partition, final String outcome) {
        assertEquals(1.0, request(partition, outcome));
    }

    private Double request(final String partition, final String outcome) {
        return CollectorRegistry.defaultRegistry.getSampleValue(CanaryMetric.REQUESTS.getName(),
                new String[]{"selector", "rule", "partition", "outcome"}, new String[]{"selector-a", "rule-a", partition, outcome});
    }

    private Double ruleSample(final CanaryMetric metric, final String suffix) {
        return CollectorRegistry.defaultRegistry.getSampleValue(metric.getName() + suffix,
                new String[]{"selector", "rule"}, new String[]{"selector-a", "rule-a"});
    }

    private Double fallback() {
        return CollectorRegistry.defaultRegistry.getSampleValue(CanaryMetric.FALLBACK.getName(),
                new String[]{"selector", "rule", "reason"}, new String[]{"selector-a", "rule-a", CanaryContext.CANARY_POOL_EMPTY});
    }

    private Double latency(final String partition, final String suffix) {
        return CollectorRegistry.defaultRegistry.getSampleValue(LabelNames.EXECUTE_LATENCY_NAME + suffix,
                new String[]{"partition"}, new String[]{partition});
    }
}
