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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.context.CanaryContext;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.UpstreamRoutingContext;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.apache.shenyu.plugin.httpclient.AbstractHttpClientPlugin;
import org.apache.shenyu.plugin.httpclient.DefaultRetryStrategy;
import org.apache.shenyu.plugin.httpclient.ExponentialRetryBackoffStrategy;
import org.apache.shenyu.plugin.httpclient.FixedRetryStrategy;
import org.apache.shenyu.plugin.httpclient.RetryStrategy;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs Divide partition resolution and HTTP retries together against real upstream snapshots.
 */
class DivideCanaryFailoverTest {

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private final RecordingHttpClient http = new RecordingHttpClient();

    private final List<String> registered = new ArrayList<>();

    private DivideRuleHandle handle;

    private CanaryConfig config;

    @BeforeEach
    void setUp() {
        selector.setId("canary-failover-selector");
        rule.setId("canary-failover-rule");
        rule.setSelectorId(selector.getId());
        rule.setHandle(GsonUtils.getGson().toJson(new DivideRuleHandle()));
        new DividePluginDataHandler().handlerRule(rule);
        handle = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        handle.setRetry(1);
        handle.setRetryStrategy("failover");
        config = new CanaryConfig();
        config.setEnabled(true);
        config.setPercentage(100);
        config.setCanaryLabels(Map.of("release", "canary"));
        config.setStableLabels(Map.of("release", "stable"));
        handle.setCanary(config);
    }

    @AfterEach
    void tearDown() {
        new DividePluginDataHandler().removeRule(rule);
        UpstreamCacheManager.getInstance().removeByKey(selector.getId());
        registered.forEach(url -> UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(selector.getId(),
                Upstream.builder().url(url).protocol("http://").build())));
    }

    @ParameterizedTest
    @CsvSource({"stable,default", "canary,default", "fallback,default", "stable,fixed", "canary,fixed", "fallback,fixed"})
    void failoverRetainsResolvedPartitionAfterRefresh(final String mode, final String strategy) {
        boolean stable = !"canary".equals(mode);
        config.setEnabled(!"stable".equals(mode));
        DiscoveryUpstreamData first = instance("first:8080", stable ? "stable" : "canary", false);
        // A fallback starts with no canary nodes. In stable mode a legacy gray canary must not win a retry.
        refresh("fallback".equals(mode) ? List.of(first) : List.of(first, instance("opposite:8080", stable ? "canary" : "stable", stable)));
        ServerWebExchange exchange = exchange();
        AtomicInteger observations = new AtomicInteger();
        exchange.getAttributes().put(Constants.METRICS_CANARY, (Consumer<CanaryContext>) observation -> {
            assertEquals(stable ? "stable" : "canary", observation.getActualPartition());
            observations.incrementAndGet();
        });
        StepVerifier.create(new DividePlugin().doExecute(exchange, current -> {
            assertEquals("http://first:8080", current.getAttribute(Constants.HTTP_DOMAIN));
            current.getAttributes().put(Constants.HTTP_URI, RequestUrlUtils.buildRequestUri(current, current.getAttribute(Constants.HTTP_DOMAIN)));
            refresh(List.of(instance("second:8080", stable ? "stable" : "canary", false),
                    instance("opposite:8080", stable ? "canary" : "stable", stable)));
            // A shared rule update must not change this request's already resolved labels/fallback.
            config.setStableLabels(Map.of("release", "changed"));
            config.setCanaryLabels(Map.of("release", "changed"));
            return retry(strategy).execute(Mono.error(new TimeoutException("first failed")), current, Duration.ofSeconds(5), 1).then();
        }, selector, rule)).verifyComplete();
        assertEquals(List.of(URI.create("http://second:8080/test")), http.attempts);
        assertEquals(1, observations.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void exhaustedPartitionCannotFallBackAfterBackendFailure(final boolean canary) {
        config.setEnabled(canary);
        refresh(List.of(instance("first:8080", canary ? "canary" : "stable", false)));
        StepVerifier.create(new DividePlugin().doExecute(exchange(), current -> {
            current.getAttributes().put(Constants.HTTP_URI, URI.create("http://first:8080/test"));
            refresh(List.of(instance("opposite:8080", canary ? "stable" : "canary", true)));
            return new DefaultRetryStrategy<>(http).execute(Mono.error(new TimeoutException("failed")), current, Duration.ofSeconds(5), 1).then();
        }, selector, rule)).expectErrorSatisfies(error -> {
            assertTrue(error instanceof ResponseStatusException);
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, ((ResponseStatusException) error).getStatusCode());
        }).verify();
        assertTrue(http.attempts.isEmpty());
    }

    @Test
    void legacyFailoverKeepsGrayExclusivePool() {
        handle.setCanary(null);
        refresh(List.of(instance("first:8080", "stable", true), instance("normal:8080", "stable", false)));
        StepVerifier.create(new DividePlugin().doExecute(exchange(), current -> {
            assertNull(current.getAttribute(UpstreamRoutingContext.ATTRIBUTE));
            current.getAttributes().put(Constants.HTTP_URI, URI.create("http://first:8080/test"));
            refresh(List.of(instance("second:8080", "canary", true), instance("normal:8080", "stable", false)));
            return new DefaultRetryStrategy<>(http).execute(Mono.error(new TimeoutException("failed")), current, Duration.ofSeconds(5), 1).then();
        }, selector, rule)).verifyComplete();
        assertEquals(List.of(URI.create("http://second:8080/test")), http.attempts);
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "fixed", "exponential"})
    void currentRetriesRetainInitialUpstream(final String strategy) {
        handle.setRetryStrategy("current");
        refresh(List.of(instance("first:8080", "canary", false)));
        AtomicInteger attempts = new AtomicInteger();
        StepVerifier.create(new DividePlugin().doExecute(exchange(), current -> retry(strategy).execute(Mono.defer(() -> {
            assertEquals("http://first:8080", current.getAttribute(Constants.HTTP_DOMAIN));
            return attempts.incrementAndGet() == 1 ? Mono.error(new TimeoutException("transient failure")) : Mono.just("success");
        }), current, Duration.ofSeconds(5), 1).then(), selector, rule)).verifyComplete();
        assertEquals(2, attempts.get());
        assertTrue(http.attempts.isEmpty());
    }

    private RetryStrategy<String> retry(final String strategy) {
        if ("fixed".equals(strategy)) {
            return new FixedRetryStrategy<>(http);
        }
        return "exponential".equals(strategy) ? new ExponentialRetryBackoffStrategy<>(http) : new DefaultRetryStrategy<>(http);
    }

    private ServerWebExchange exchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl("/test");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        return exchange;
    }

    private DiscoveryUpstreamData instance(final String url, final String release, final boolean gray) {
        registered.add(url);
        return DiscoveryUpstreamData.builder().url(url).protocol("http://").status(0)
                .props(GsonUtils.getGson().toJson(Map.of("gray", gray, "healthCheckEnabled", false, "labels", Map.of("release", release)))).build();
    }

    private void refresh(final List<DiscoveryUpstreamData> upstreams) {
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId(selector.getId());
        data.setUpstreamDataList(upstreams);
        new DivideUpstreamDataHandler().handlerDiscoveryUpstreamData(data);
    }

    private static final class RecordingHttpClient extends AbstractHttpClientPlugin<String> {

        private final List<URI> attempts = new ArrayList<>();

        private RecordingHttpClient() {
            super(1024);
        }

        @Override
        protected Integer extractStatusCode(final String response) {
            return 200;
        }

        protected Mono<String> doRequest(final ServerWebExchange exchange, final String method, final URI uri, final Flux<DataBuffer> body) {
            attempts.add(uri);
            return Mono.just("success");
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "test-http";
        }
    }
}
