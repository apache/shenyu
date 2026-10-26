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

package org.apache.shenyu.plugin.key.auth;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.key.auth.handler.KeyAuthPluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Real HTTP requests through the public plugin entry and the server-owned rule cache.
 */
class KeyAuthAuthenticationHttpTest {

    @Test
    void authenticatesHeaderAndQueryWithoutEchoingCredentials() {
        try (Fixture fixture = new Fixture()) {
            fixture.client.get().uri("/").header("X-Api-Key", "test-secret")
                    .header("X-Tenant-Id", "untrusted").exchange().expectStatus().isOk()
                    .expectBody(String.class).isEqualTo("http-namespace/http-selector/http-rule");
            fixture.client.get().uri("/?X-Api-Key=test-secret").exchange().expectStatus().isOk()
                    .expectBody(String.class).isEqualTo("http-namespace/http-selector/http-rule");
            assertEquals(2, fixture.calls.get());
        }
    }

    @Test
    void invalidCredentialNeverInvokesBusinessHandler() {
        try (Fixture fixture = new Fixture()) {
            fixture.client.get().uri("/").header("X-Api-Key", "wrong-secret").exchange()
                    .expectBody(String.class).value(body -> assertFalse(body.contains("wrong-secret")));
            fixture.client.get().uri("/").header("X-Tenant-Id", "untrusted").exchange().expectBody();
            assertEquals(0, fixture.calls.get());
        }
    }

    @Test
    void concurrentValidAndInvalidRequestsStayIsolated() {
        try (Fixture fixture = new Fixture()) {
            WebClient client = WebClient.builder().baseUrl(fixture.url()).build();
            StepVerifier.create(Flux.range(0, 32).flatMap(index -> client.get().uri("/")
                    .header("X-Api-Key", index % 2 == 0 ? "test-secret" : "invalid")
                    .retrieve().bodyToMono(String.class).doOnNext(body -> {
                        if (index % 2 == 0) {
                            assertEquals("http-namespace/http-selector/http-rule", body);
                        } else {
                            assertFalse(body.contains("http-rule"));
                        }
                    }), 8).then()).expectComplete().verify(Duration.ofSeconds(30));
            assertEquals(16, fixture.calls.get());
        }
    }

    @Test
    void disabledPluginDoesNotMakeDownstreamAuthenticated() {
        try (Fixture fixture = new Fixture()) {
            fixture.data.setEnabled(false);
            BaseDataCache.getInstance().cachePluginData(fixture.data);
            fixture.client.get().uri("/").header("X-Api-Key", "test-secret").exchange().expectStatus().isUnauthorized();
            assertEquals(0, fixture.calls.get());
        }
    }

    private static final class Fixture implements AutoCloseable {

        private final KeyAuthPlugin plugin = new KeyAuthPlugin();

        private final KeyAuthPluginDataHandler handler = new KeyAuthPluginDataHandler();

        private final AtomicInteger calls = new AtomicInteger();

        private final RuleData rule = new RuleData();

        private final PluginData data = new PluginData();

        private final LoopResources loops = LoopResources.create("key-auth-test", 2, true);

        private final DisposableServer server;

        private final WebTestClient client;

        private Fixture() {
            ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
            ShenyuConfig config = new ShenyuConfig();
            config.getSelectorMatchCache().getCache().setEnabled(false);
            config.getRuleMatchCache().getCache().setEnabled(false);
            when(context.getBean(ShenyuConfig.class)).thenReturn(config);
            when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
            SpringBeanUtils.getInstance().setApplicationContext(context);
            data.setName(plugin.named());
            data.setEnabled(true);
            BaseDataCache.getInstance().cachePluginData(data);
            SelectorData selector = new SelectorData();
            selector.setId("http-selector");
            selector.setPluginName(plugin.named());
            selector.setType(SelectorTypeEnum.FULL_FLOW.getCode());
            selector.setContinued(true);
            selector.setEnabled(true);
            selector.setLogged(false);
            selector.setSort(0);
            BaseDataCache.getInstance().cacheSelectData(selector);
            rule.setId("http-rule");
            rule.setSelectorId(selector.getId());
            rule.setNamespaceId("http-namespace");
            rule.setEnabled(true);
            rule.setLogged(false);
            rule.setSort(0);
            rule.setHandle("{\"keyName\":\"X-Api-Key\",\"key\":\"test-secret\"}");
            handler.handlerRule(rule);
            BaseDataCache.getInstance().cacheRuleData(rule);
            server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops)
                    .handle(new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange ->
                            plugin.execute(exchange, this::business)).build())).bindNow(Duration.ofSeconds(20));
            client = WebTestClient.bindToServer().baseUrl(url()).responseTimeout(Duration.ofSeconds(20)).build();
        }

        private Mono<Void> business(final ServerWebExchange exchange) {
            return KeyAuthAuthentication.from(exchange).map(result -> {
                calls.incrementAndGet();
                return result.getNamespaceId() + "/" + result.getSelectorId() + "/" + result.getRuleId();
            }).switchIfEmpty(Mono.defer(() -> {
                exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                return Mono.just("unauthenticated");
            })).flatMap(body -> exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory()
                    .wrap(body.getBytes(StandardCharsets.UTF_8))))).subscribeOn(Schedulers.parallel());
        }

        private String url() {
            return "http://127.0.0.1:" + server.port();
        }

        @Override
        public void close() {
            server.disposeNow(Duration.ofSeconds(10));
            loops.disposeLater().block(Duration.ofSeconds(10));
            handler.removeRule(rule);
            BaseDataCache.getInstance().removeRuleDataBySelectorId(rule.getSelectorId());
            BaseDataCache.getInstance().removeSelectDataByPluginName(plugin.named());
            BaseDataCache.getInstance().removePluginDataByPluginName(plugin.named());
        }
    }
}
