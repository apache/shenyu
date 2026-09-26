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

package org.apache.shenyu.web.handler;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.prometheus.client.CollectorRegistry;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.context.DivideShenyuContextDecorator;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.apache.shenyu.plugin.global.DefaultShenyuContextBuilder;
import org.apache.shenyu.plugin.global.GlobalPlugin;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.metrics.MetricsPlugin;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsRegister;
import org.apache.shenyu.plugin.metrics.prometheus.PrometheusMetricsService;
import org.apache.shenyu.plugin.metrics.reporter.MetricsReporter;
import org.apache.shenyu.plugin.response.ResponsePlugin;
import org.apache.shenyu.plugin.response.strategy.NettyClientMessageWriter;
import org.apache.shenyu.plugin.uri.URIPlugin;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import org.springframework.web.server.handler.ResponseStatusExceptionHandler;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs the real gateway plugin chain, HTTP backends and a native Prometheus process.
 * Invoke explicitly with -Dtest=CanaryPrometheusIT -Dprometheus.binary=/path/to/prometheus.
 */
class CanaryPrometheusIT {

    private static final Duration WAIT = Duration.ofSeconds(30);

    private static final String JOB = "shenyu-canary-e2e";

    private final HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build();

    private final Map<String, AtomicInteger> backendHits = new ConcurrentHashMap<>();

    private final List<SelectorData> selectors = new ArrayList<>();

    private final List<RuleData> rules = new ArrayList<>();

    private final PrometheusMetricsService metricsService = new PrometheusMetricsService();

    private GenericApplicationContext context;

    private ApplicationContext previousContext;

    private CommonPluginDataSubscriber subscriber;

    private DisposableServer stable;

    private DisposableServer canary;

    private DisposableServer gateway;

    private Process prometheus;

    private String prometheusUrl;

    private Path evidence;

    @Test
    void testCanaryMetricsThroughPrometheus() throws Exception {
        String binary = System.getProperty("prometheus.binary");
        assertNotNull(binary, "Pass -Dprometheus.binary=/absolute/path/to/prometheus; this test never substitutes a mock server");
        assertTrue(Files.isExecutable(Path.of(binary)), "Prometheus binary must be executable");
        Path output = Path.of("target", "prometheus-e2e");
        Files.createDirectories(output);
        evidence = Files.createTempDirectory(output, "run-").toAbsolutePath();
        startGateway();
        startPrometheus(binary);
        assertValue("up{job=\"" + JOB + "\"}", 1);
        JsonObject target = getJson("/api/v1/targets").getAsJsonObject("data").getAsJsonArray("activeTargets").get(0).getAsJsonObject();
        assertEquals("up", target.get("health").getAsString());
        assertEquals("", target.get("lastError").getAsString());

        configure("stable", 0, "STABLE", true, false);
        configure("canary", 100, "STABLE", true, false);
        configure("fallback", 100, "STABLE", false, false);
        configure("reject", 100, "REJECT", false, false);
        configure("retry", 100, "STABLE", true, false);
        configure("legacy", 0, "STABLE", false, true);
        for (int i = 0; i < 3; i++) {
            request("/stable/sensitive-" + i + "/ok", 200, "stable");
            request("/canary/sensitive-" + i + "/ok", 200, "canary");
        }
        request("/stable/error", 502, "stable");
        request("/canary/error", 502, "canary");
        for (int i = 0; i < 2; i++) {
            request("/fallback/ok", 200, "stable");
            request("/reject/ok", 503, null);
        }
        request("/retry/ok", 200, "canary");
        request("/legacy/ok", 200, "stable");
        assertEquals(2, hits("canary", "/retry/ok"));
        assertEquals(0, hits("stable", "/retry/ok"));
        assertEquals(2, hits("stable", "/fallback/ok"));
        assertEquals(0, hits("canary", "/fallback/ok"));
        assertEquals(0, hits("stable", "/reject/ok") + hits("canary", "/reject/ok"));

        assertRequest("stable", "stable", "success", 3);
        assertRequest("stable", "stable", "error", 1);
        assertRequest("canary", "canary", "success", 3);
        assertRequest("canary", "canary", "error", 1);
        assertRequest("fallback", "stable", "success", 2);
        assertRequest("reject", "canary", "reject", 2);
        assertRequest("retry", "canary", "success", 1);
        assertValue("sum(shenyu_canary_requests_total)", 13);
        assertValue("count(shenyu_canary_requests_total)", 7);
        assertValue("shenyu_canary_fallback_total{rule=\"e2e-fallback-rule\",reason=\"canary_pool_empty\"}", 2);
        assertValue("count(shenyu_canary_fallback_total)", 1);
        assertValue("sum(shenyu_canary_decision_duration_seconds_count)", 13);
        assertValue("shenyu_canary_decision_duration_seconds_count{rule=\"e2e-retry-rule\"}", 1);
        assertTrue(value("sum(shenyu_canary_decision_duration_seconds_sum)") > 0);
        assertValue("sum(shenyu_canary_decision_duration_seconds_bucket{le=\"+Inf\"})", 13);
        assertValue("shenyu_execute_latency_millis_count{partition=\"stable\"}", 6);
        assertValue("shenyu_execute_latency_millis_count{partition=\"canary\"}", 7);
        assertValue("shenyu_execute_latency_millis_count{partition=\"none\"}", 1);
        assertTrue(value("shenyu_execute_latency_millis_sum{partition=\"stable\"}") >= 240);
        assertTrue(value("shenyu_execute_latency_millis_sum{partition=\"canary\"}") >= 400);
        assertValue("sum(shenyu_canary_requests_total{partition=\"stable\",outcome=\"error\"}) / sum(shenyu_canary_requests_total{partition=\"stable\"})", 1.0 / 6);
        assertValue("sum(shenyu_canary_requests_total{partition=\"canary\",outcome=\"error\"})"
                + " / sum(shenyu_canary_requests_total{partition=\"canary\",outcome=~\"success|error\"})", 1.0 / 5);
        for (String partition : List.of("stable", "canary")) {
            double p95 = value("histogram_quantile(0.95, sum by (le) (shenyu_execute_latency_millis_bucket{partition=\"" + partition + "\"}))");
            assertTrue(Double.isFinite(p95) && p95 > 0, "Prometheus must calculate latency quantiles for " + partition);
        }
        assertLabels("shenyu_canary_requests_total", Set.of("selector", "rule", "partition", "outcome"));
        assertLabels("shenyu_canary_fallback_total", Set.of("selector", "rule", "reason"));
        assertLabels("shenyu_canary_decision_duration_seconds_count", Set.of("selector", "rule"));
        assertLabels("shenyu_execute_latency_millis_count", Set.of("partition"));
        Files.writeString(evidence.resolve("backend-hits.json"), GsonUtils.getGson().toJson(backendHits));
        Files.writeString(evidence.resolve("result.txt"), "PASS: 14 real gateway requests, 13 Canary observations, real Prometheus scrape and PromQL assertions.\n");
    }

    private void startGateway() {
        ShenyuConfig config = new ShenyuConfig();
        config.getScheduler().setEnabled(false);
        config.getExtPlugin().setEnabled(false);
        context = new GenericApplicationContext();
        context.registerBean(ShenyuConfig.class, () -> config);
        context.registerBean(ShenyuResult.class, DefaultShenyuResult::new);
        context.refresh();
        previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        subscriber = new CommonPluginDataSubscriber(List.of(new DividePluginDataHandler()), context,
                config.getSelectorMatchCache(), config.getRuleMatchCache());
        subscriber.onSubscribe(PluginData.builder().id("e2e-divide").name("divide").enabled(true).sort(PluginEnum.DIVIDE.getCode()).build());
        stable = startBackend("stable");
        canary = startBackend("canary");
        List<ShenyuPlugin> plugins = new ArrayList<>(List.of(
                new GlobalPlugin(new DefaultShenyuContextBuilder(Map.of("http", new DivideShenyuContextDecorator()))),
                new MetricsPlugin(), new DividePlugin(), new URIPlugin(),
                new NettyHttpClientPlugin(reactor.netty.http.client.HttpClient.create()),
                new ResponsePlugin(Map.of("http", new NettyClientMessageWriter()))));
        plugins.sort(Comparator.comparingInt(ShenyuPlugin::getOrder));
        ShenyuLoaderService loader = new ShenyuLoaderService(null, subscriber, config);
        ShenyuWebHandler handler = new ShenyuWebHandler(plugins, loader, config);
        gateway = HttpServer.create().host("127.0.0.1").port(0)
                .handle(new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(handler)
                        .exceptionHandler(new ResponseStatusExceptionHandler()).build())).bindNow(WAIT);
        MetricsReporter.clean();
        new PrometheusMetricsRegister().clean();
        CollectorRegistry.defaultRegistry.clear();
        PrometheusMetricsRegister register = new PrometheusMetricsRegister();
        MetricsReporter.register(register);
        ShenyuConfig.MetricsConfig metricsConfig = new ShenyuConfig.MetricsConfig();
        metricsConfig.setHost("127.0.0.1");
        metricsConfig.setPort(0);
        metricsConfig.setProps(new Properties());
        metricsService.start(metricsConfig, register);
        assertNotNull(metricsService.getServer());
    }

    private DisposableServer startBackend(final String partition) {
        return HttpServer.create().host("127.0.0.1").port(0).handle((request, response) -> {
            String path = URI.create(request.uri()).getPath();
            int attempt = backendHits.computeIfAbsent(partition + ":" + path, key -> new AtomicInteger()).incrementAndGet();
            long delay = "/retry/ok".equals(path) && attempt == 1 ? 800 : 40;
            return Mono.delay(Duration.ofMillis(delay)).then(Mono.defer(() -> response.status(path.endsWith("/error") ? 502 : 200)
                    .header("Content-Type", "text/plain").sendString(Mono.just(partition)).then()));
        }).bindNow(WAIT);
    }

    private void configure(final String name, final int percentage, final String fallback, final boolean includeCanary, final boolean legacy) {
        ConditionData condition = new ConditionData();
        condition.setParamType(ParamTypeEnum.URI.getName());
        condition.setOperator(OperatorEnum.MATCH.getAlias());
        condition.setParamName("/");
        condition.setParamValue("/" + name + "/**");
        final SelectorData selector = SelectorData.builder().id("e2e-" + name + "-selector").name(name).pluginName("divide")
                .pluginId("e2e-divide").enabled(true).logged(false).continued(true).sort(selectors.size())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).matchMode(MatchModeEnum.AND.getCode())
                .conditionList(List.of(condition)).build();
        CanaryConfig canaryConfig = new CanaryConfig();
        canaryConfig.setEnabled(true);
        canaryConfig.setPercentage(percentage);
        canaryConfig.setCanaryLabels(Map.of("release", "canary"));
        canaryConfig.setStableLabels(Map.of("release", "stable"));
        canaryConfig.setFallbackPolicy(fallback);
        DivideRuleHandle handle = new DivideRuleHandle();
        handle.setCanary(legacy ? null : canaryConfig);
        handle.setTimeout("retry".equals(name) ? 200 : 3000);
        handle.setRetry("retry".equals(name) ? 1 : 0);
        RuleData rule = RuleData.builder().id("e2e-" + name + "-rule").name(name).pluginName("divide").selectorId(selector.getId())
                .enabled(true).loged(false).sort(0).matchMode(MatchModeEnum.AND.getCode()).conditionDataList(List.of(condition))
                .handle(GsonUtils.getGson().toJson(handle)).build();
        selectors.add(selector);
        rules.add(rule);
        subscriber.onSelectorSubscribe(selector);
        subscriber.onRuleSubscribe(rule);
        List<DiscoveryUpstreamData> upstreams = new ArrayList<>();
        upstreams.add(upstream(stable, "stable"));
        if (includeCanary) {
            upstreams.add(upstream(canary, "canary"));
        }
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId(selector.getId());
        data.setUpstreamDataList(upstreams);
        new DivideUpstreamDataHandler().handlerDiscoveryUpstreamData(data);
    }

    private DiscoveryUpstreamData upstream(final DisposableServer server, final String partition) {
        return DiscoveryUpstreamData.builder().url("127.0.0.1:" + server.port()).protocol("http://").status(0).weight(100)
                .props(GsonUtils.getGson().toJson(Map.of("healthCheckEnabled", false, "labels", Map.of("release", partition)))).build();
    }

    private void startPrometheus(final String binary) throws Exception {
        int port;
        try (ServerSocket socket = new ServerSocket(0, 0, InetAddress.getLoopbackAddress())) {
            port = socket.getLocalPort();
        }
        prometheusUrl = "http://127.0.0.1:" + port;
        Path config = evidence.resolve("prometheus.yml");
        Files.writeString(config, "global:\n  scrape_interval: 1s\n  scrape_timeout: 1s\nscrape_configs:\n  - job_name: " + JOB
                + "\n    static_configs:\n      - targets: ['127.0.0.1:" + metricsService.getServer().getPort() + "']\n");
        prometheus = new ProcessBuilder(binary, "--config.file=" + config, "--storage.tsdb.path=" + evidence.resolve("data"),
                "--web.listen-address=127.0.0.1:" + port, "--storage.tsdb.retention.time=1h")
                .redirectErrorStream(true).redirectOutput(evidence.resolve("prometheus.log").toFile()).start();
        await().atMost(WAIT).pollInterval(Duration.ofMillis(200)).ignoreExceptions().untilAsserted(() -> {
            assertTrue(prometheus.isAlive(), "Prometheus exited; inspect " + evidence.resolve("prometheus.log"));
            assertEquals(200, client.send(HttpRequest.newBuilder(URI.create(prometheusUrl + "/-/ready"))
                    .timeout(Duration.ofSeconds(2)).build(), HttpResponse.BodyHandlers.ofString()).statusCode());
        });
        getJson("/api/v1/status/buildinfo");
    }

    private void request(final String path, final int status, final String partition) throws Exception {
        HttpResponse<String> response = client.send(HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + gateway.port() + path))
                .timeout(Duration.ofSeconds(10)).header("Authorization", "Bearer sensitive-" + path)
                .header("Cookie", "user=sensitive-" + path).header("X-Sticky-Key", "sensitive-" + path)
                .header("X-Request-ID", "sensitive-" + path).build(), HttpResponse.BodyHandlers.ofString());
        Files.writeString(evidence.resolve("requests.jsonl"), GsonUtils.getGson().toJson(Map.of("path", path,
                "status", response.statusCode(), "body", response.body())) + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        assertEquals(status, response.statusCode(), path + ": " + response.body());
        if (Objects.nonNull(partition)) {
            assertEquals(partition, response.body(), "Response must come from the expected backend");
        }
    }

    private int hits(final String partition, final String path) {
        return backendHits.getOrDefault(partition + ":" + path, new AtomicInteger()).get();
    }

    private void assertRequest(final String rule, final String partition, final String outcome, final int count) {
        assertValue("shenyu_canary_requests_total{selector=\"e2e-" + rule + "-selector\",rule=\"e2e-" + rule
                + "-rule\",partition=\"" + partition + "\",outcome=\"" + outcome + "\"}", count);
    }

    private void assertValue(final String query, final double expected) {
        await().atMost(WAIT).pollInterval(Duration.ofMillis(250)).untilAsserted(() -> assertEquals(expected, value(query), 0.000001, query));
    }

    private double value(final String query) throws Exception {
        JsonArray result = query(query);
        assertEquals(1, result.size(), "Expected exactly one PromQL result for " + query);
        return result.get(0).getAsJsonObject().getAsJsonArray("value").get(1).getAsDouble();
    }

    private JsonArray query(final String query) throws Exception {
        JsonObject response = getJson("/api/v1/query?query=" + URLEncoder.encode(query, StandardCharsets.UTF_8));
        assertEquals("vector", response.getAsJsonObject("data").get("resultType").getAsString());
        return response.getAsJsonObject("data").getAsJsonArray("result");
    }

    private JsonObject getJson(final String endpoint) throws Exception {
        HttpResponse<String> response = client.send(HttpRequest.newBuilder(URI.create(prometheusUrl + endpoint))
                .timeout(Duration.ofSeconds(5)).build(), HttpResponse.BodyHandlers.ofString());
        Files.writeString(evidence.resolve("prometheus-api.jsonl"), GsonUtils.getGson().toJson(Map.of("endpoint", endpoint,
                "response", response.body())) + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        assertEquals(200, response.statusCode(), response.body());
        JsonObject json = JsonParser.parseString(response.body()).getAsJsonObject();
        assertEquals("success", json.get("status").getAsString(), response.body());
        return json;
    }

    private void assertLabels(final String metric, final Set<String> expected) throws Exception {
        JsonArray series = query(metric);
        assertFalse(series.isEmpty());
        for (JsonElement element : series) {
            JsonObject labels = element.getAsJsonObject().getAsJsonObject("metric");
            assertFalse(labels.toString().contains("sensitive-"), "Request values must never become Canary labels");
            assertEquals(JOB, labels.remove("job").getAsString());
            assertNotNull(labels.remove("instance"));
            assertEquals(metric, labels.remove("__name__").getAsString());
            assertEquals(expected, labels.keySet());
        }
    }

    @AfterEach
    void clean() throws IOException, InterruptedException {
        if (Objects.nonNull(prometheus)) {
            prometheus.destroy();
            if (!prometheus.waitFor(5, TimeUnit.SECONDS)) {
                prometheus.destroyForcibly();
                assertTrue(prometheus.waitFor(5, TimeUnit.SECONDS), "Prometheus must exit after the test");
            }
        }
        if (Objects.nonNull(gateway)) {
            gateway.disposeNow(WAIT);
        }
        if (Objects.nonNull(stable)) {
            stable.disposeNow(WAIT);
        }
        if (Objects.nonNull(canary)) {
            canary.disposeNow(WAIT);
        }
        metricsService.stop();
        MetricsReporter.clean();
        CollectorRegistry.defaultRegistry.clear();
        if (Objects.nonNull(subscriber)) {
            rules.forEach(subscriber::unRuleSubscribe);
            selectors.forEach(selector -> {
                subscriber.unSelectorSubscribe(selector);
                UpstreamCacheManager.getInstance().removeByKey(selector.getId());
            });
            subscriber.unSubscribe(PluginData.builder().name("divide").build());
        }
        if (Objects.nonNull(context)) {
            context.close();
            SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        }
    }
}
