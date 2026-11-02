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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.web.filter.HealthFilter;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.env.MapPropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import org.springframework.web.server.handler.ResponseStatusExceptionHandler;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Reactive HTTP mapping tests for the {@code /actuator} plain controllers behind the
 * {@code HealthFilter} dispatch path (issue #8219).
 *
 * <p>These tests exercise the real {@code DispatcherHandler} mapping: the request-plugin
 * chain is represented by a deny {@link WebFilter} that answers 401 on business paths, which
 * the {@code /actuator/*} dispatch famously bypasses. With the shipped defaults the raw
 * cache-inspection mappings do not exist at all, so an anonymous caller is rejected before
 * any cache traversal; health-style mappings keep working.</p>
 */
public final class ShenyuActuatorDispatchIntegrationTest {

    private static final String FAKE_SECRET = "apiKey=my-super-secret-key-12345";

    @AfterEach
    public void cleanUp() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
    }

    @Test
    public void defaultShippedSettingsRejectRawInspection() {
        final AnnotationConfigApplicationContext context = buildContext(Map.of());
        final MockServerWebExchange exchange = perform(context, "/actuator/pluginData");
        assertEquals(HttpStatus.NOT_FOUND.value(), statusCode(exchange),
                "the raw cache mappings must not exist under shipped defaults");
    }

    @Test
    public void healthStyleMappingIsPreserved() {
        final AnnotationConfigApplicationContext context = buildContext(Map.of());
        final MockServerWebExchange exchange = perform(context, "/actuator/health");
        assertEquals(HttpStatus.OK.value(), statusCode(exchange),
                "health/readiness mappings must keep working");
    }

    @Test
    public void requestPluginIsBypassedButRawInspectionIsDigestOnlyWhenEnabled() {
        BaseDataCache.getInstance().cachePluginData(PluginData.builder()
                .id("p1").name("divide").config(FAKE_SECRET).role("1").enabled(true).build());
        final AnnotationConfigApplicationContext context = buildContext(Map.of("shenyu.endpoint.raw-data.enabled", "true"));
        final MockServerWebExchange business = perform(context, "/business/data");
        assertEquals(HttpStatus.UNAUTHORIZED.value(), statusCode(business),
                "business requests still traverse the request-plugin chain");

        final MockServerWebExchange endpoint = perform(context, "/actuator/pluginData");
        assertEquals(HttpStatus.OK.value(), statusCode(endpoint));
        final String body = endpoint.getResponse().getBodyAsString().block(Duration.ofSeconds(5));
        assertFalse(body.contains("my-super-secret-key-12345"), "raw config must never be published");
        assertTrue(body.matches("(?s).*[0-9a-f]{64}.*"), "expected a SHA-256 digest representation, got: " + body);
    }

    @Test
    public void enabledMappingServesDigestInsteadOfRawCacheDump() {
        BaseDataCache.getInstance().cachePluginData(PluginData.builder()
                .id("p1").name("divide").config(FAKE_SECRET).role("1").enabled(true).build());
        final AnnotationConfigApplicationContext context = buildContext(Map.of("shenyu.endpoint.raw-data.enabled", "true"));
        final MockServerWebExchange exchange = perform(context, "/actuator/pluginData");
        assertEquals(HttpStatus.OK.value(), statusCode(exchange));
        final String body = exchange.getResponse().getBodyAsString().block(Duration.ofSeconds(5));
        assertFalse(body.contains("my-super-secret-key-12345"), "raw config must never be published");
        assertTrue(body.contains("configDigest"), "expected the bounded digest representation, got: " + body);
    }

    private AnnotationConfigApplicationContext buildContext(final Map<String, Object> properties) {
        final AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        context.getEnvironment().getPropertySources().addFirst(new MapPropertySource("test", properties));
        context.register(GatewayConfig.class);
        context.refresh();
        return context;
    }

    private MockServerWebExchange perform(final AnnotationConfigApplicationContext context, final String path) {
        final DispatcherHandler dispatcher = new DispatcherHandler(context);
        final HttpHandler handler = WebHttpHandlerBuilder.webHandler(dispatcher)
                .exceptionHandler(new ResponseStatusExceptionHandler())
                .filter(new HealthFilter(dispatcher, List.of("/actuator", "/health_check")))
                .filter(new DenyBusinessPathFilter())
                .build();
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080" + path));
        handler.handle(exchange.getRequest(), exchange.getResponse()).block(Duration.ofSeconds(5));
        return exchange;
    }

    /**
     * Mock writes do not set an explicit 200 status; a null status with a written body
     * therefore corresponds to a real server's default 200.
     */
    private static int statusCode(final MockServerWebExchange exchange) {
        final HttpStatusCode status = exchange.getResponse().getStatusCode();
        return Objects.isNull(status) ? HttpStatus.OK.value() : status.value();
    }

    /**
     * Stands in for the request-plugin chain: deny everything under /business with 401,
     * pass everything else on.
     */
    private static final class DenyBusinessPathFilter implements WebFilter {

        @Override
        public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
            if (exchange.getRequest().getURI().getRawPath().startsWith("/business")) {
                exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                return exchange.getResponse().setComplete();
            }
            return chain.filter(exchange);
        }
    }

    @RestController
    static class HealthStubController {

        @GetMapping("/actuator/health")
        Mono<String> health() {
            return Mono.just("UP");
        }
    }

    @Configuration
    @EnableWebFlux
    @Import({HealthStubController.class, ShenyuControllerEndpoint.class})
    static class GatewayConfig {

        @Bean
        ShenyuWebHandler shenyuWebHandler() {
            return Mockito.mock(ShenyuWebHandler.class);
        }
    }
}
