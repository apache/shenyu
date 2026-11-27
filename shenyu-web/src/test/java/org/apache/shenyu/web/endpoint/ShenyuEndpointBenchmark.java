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

package org.apache.shenyu.web.endpoint;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.MapPropertySource;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import org.springframework.web.server.handler.ResponseStatusExceptionHandler;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Endpoint-level microbenchmark for the raw-data gate (issue #8219 review, item 3).
 *
 * <p>Runs the real {@code DispatcherHandler} mapping for the three states the review asks
 * to be measured — the pre-change raw dump (the raw paths with
 * {@code shenyu.endpoint.raw-data.enabled=true}, byte-identical to the pre-change behavior),
 * the always-on digest view, and the shipped-default 404 rejection — and reports throughput,
 * latency percentiles and response size. It is not a CI test: set
 * {@code -Dshenyu.benchmark=true} to execute it; results go to
 * {@code docs/benchmarks/actuator-raw-data-gate/}. Ordinary forwarding is untouched by this
 * change — the gate lives only in these mappings — so there is no per-request forwarding
 * code to measure.</p>
 */
public final class ShenyuEndpointBenchmark {

    private static final String FAKE_CONFIG = "apiKey=bench-secret-key-12345,password=db-pass";

    private static final int WARMUP = 2000;

    private static final int REQUESTS = 20000;

    private static final int ROUNDS = 5;

    /**
     * The benchmark entry point; skipped unless -Dshenyu.benchmark=true.
     */
    @Test
    public void benchmark() {
        Assumptions.assumeTrue(Boolean.getBoolean("shenyu.benchmark"), "run with -Dshenyu.benchmark=true");
        BaseDataCache.getInstance().cachePluginData(PluginData.builder()
                .id("p1").name("divide").config(FAKE_CONFIG).role("1").enabled(true).build());
        MetaDataCache.getInstance().cache(MetaData.builder().id("m1").path("/rpc/demo").rpcType("dubbo")
                .rpcExt("{\"timeout\":3000,\"method\":\"demo\"}").build());
        try (AnnotationConfigApplicationContext raw = buildContext(Map.of("shenyu.endpoint.raw-data.enabled", "true"));
             AnnotationConfigApplicationContext digest = buildContext(Map.of())) {
            run("raw-dump (pre-change behavior, raw-data.enabled=true)", handler(raw), "/actuator/pluginData");
            run("digest (always-on view)", handler(digest), "/actuator/pluginDigest");
            run("404 default (shipped settings)", handler(digest), "/actuator/pluginData");
        } finally {
            BaseDataCache.getInstance().cleanPluginData();
            MetaDataCache.getInstance().clean();
        }
    }

    private HttpHandler handler(final AnnotationConfigApplicationContext context) {
        return WebHttpHandlerBuilder.webHandler(new DispatcherHandler(context))
                .exceptionHandler(new ResponseStatusExceptionHandler())
                .build();
    }

    private void run(final String label, final HttpHandler handler, final String path) {
        for (int i = 0; i < WARMUP; i++) {
            exchange(handler, path);
        }
        List<Long> latencies = new ArrayList<>(REQUESTS * ROUNDS);
        long bytes = 0;
        List<Double> throughputs = new ArrayList<>(ROUNDS);
        for (int round = 0; round < ROUNDS; round++) {
            long roundStart = System.nanoTime();
            for (int i = 0; i < REQUESTS; i++) {
                long start = System.nanoTime();
                MockServerWebExchange exchange = exchange(handler, path);
                latencies.add(System.nanoTime() - start);
                if (round == ROUNDS - 1) {
                    bytes += responseBody(exchange);
                }
            }
            throughputs.add(REQUESTS / ((System.nanoTime() - roundStart) / 1_000_000_000.0));
        }
        latencies.sort(Long::compareTo);
        long mean = 0;
        for (long latency : latencies) {
            mean += latency;
        }
        mean /= latencies.size();
        System.out.printf("%-52s throughput=%.0f req/s (min-max %.0f-%.0f) mean=%.1fus p95=%.1fus p99=%.1fus size=%d bytes%n",
                label, median(throughputs), throughputs.stream().mapToDouble(d -> d).min().orElse(0),
                throughputs.stream().mapToDouble(d -> d).max().orElse(0),
                mean / 1000.0,
                latencies.get((int) (latencies.size() * 0.95)) / 1000.0,
                latencies.get((int) (latencies.size() * 0.99)) / 1000.0,
                bytes / REQUESTS);
    }

    private double median(final List<Double> values) {
        List<Double> sorted = new ArrayList<>(values);
        sorted.sort(Double::compareTo);
        return sorted.get(sorted.size() / 2);
    }

    private MockServerWebExchange exchange(final HttpHandler handler, final String path) {
        MockServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost:8080" + path).build());
        handler.handle(exchange.getRequest(), exchange.getResponse()).block(Duration.ofSeconds(10));
        return exchange;
    }

    private int responseBody(final MockServerWebExchange exchange) {
        return exchange.getResponse().getBodyAsString().block(Duration.ofSeconds(10)).length();
    }

    private AnnotationConfigApplicationContext buildContext(final Map<String, Object> properties) {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        context.getEnvironment().getPropertySources().addFirst(new MapPropertySource("bench", properties));
        context.register(BenchConfig.class, ShenyuControllerEndpoint.class, ShenyuRawDataEndpoint.class);
        context.refresh();
        return context;
    }

    @Configuration
    @EnableWebFlux
    static class BenchConfig {

        @Bean
        ShenyuWebHandler shenyuWebHandler() {
            return Mockito.mock(ShenyuWebHandler.class);
        }
    }

}
