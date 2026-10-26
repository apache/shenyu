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
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.key.auth.handler.KeyAuthPluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.security.Principal;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Authentication publication and request-boundary regression tests.
 */
class KeyAuthAuthenticationTest {

    private final KeyAuthPlugin plugin = new KeyAuthPlugin();

    private final KeyAuthPluginDataHandler handler = new KeyAuthPluginDataHandler();

    private RuleData rule;

    @BeforeEach
    void setUp() {
        rule = new RuleData();
        rule.setId("verified-rule");
        rule.setSelectorId("verified-selector");
        rule.setNamespaceId("config-namespace");
        rule.setHandle("{\"keyName\":\"X-Api-Key\",\"key\":\"test-secret\"}");
        handler.handlerRule(rule);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        ShenyuConfig config = new ShenyuConfig();
        config.getSelectorMatchCache().getCache().setEnabled(false);
        config.getRuleMatchCache().getCache().setEnabled(false);
        when(context.getBean(ShenyuConfig.class)).thenReturn(config);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void cleanUp() {
        handler.removeRule(rule);
        BaseDataCache.getInstance().removePluginDataByPluginName(plugin.named());
        BaseDataCache.getInstance().removeSelectDataByPluginName(plugin.named());
        BaseDataCache.getInstance().removeRuleDataBySelectorId("verified-selector");
    }

    @Test
    void publishesVerifiedHeaderResultAfterSchedulerSwitch() {
        ServerWebExchange exchange = request("test-secret");
        ShenyuPluginChain chain = current -> KeyAuthAuthentication.from(current)
                .switchIfEmpty(Mono.error(new AssertionError("Missing verified key-auth result")))
                .doOnNext(this::assertRule).then().subscribeOn(Schedulers.parallel());
        StepVerifier.create(authenticate(exchange, chain)).expectComplete().verify(Duration.ofSeconds(10));
        StepVerifier.create(KeyAuthAuthentication.from(exchange)).verifyComplete();
    }

    @Test
    void publishesVerifiedQueryResult() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/mcp")
                .queryParam("X-Api-Key", "test-secret"));
        StepVerifier.create(authenticate(exchange, current -> KeyAuthAuthentication.from(current)
                .switchIfEmpty(Mono.error(new AssertionError("Missing query authentication result")))
                .doOnNext(this::assertRule).then())).verifyComplete();
    }

    @Test
    void rejectsWrongKeyBeforeDownstream() {
        AtomicInteger calls = new AtomicInteger();
        ServerWebExchange exchange = request("wrong-secret");
        StepVerifier.create(authenticate(exchange, current -> {
            calls.incrementAndGet();
            return Mono.empty();
        })).verifyComplete();
        assertEquals(0, calls.get());
        StepVerifier.create(KeyAuthAuthentication.from(exchange)).verifyComplete();
    }

    @Test
    void cannotReadAnotherRequestsAuthentication() {
        ServerWebExchange exchange = request("test-secret");
        ServerWebExchange other = request("test-secret");
        StepVerifier.create(authenticate(exchange, current -> KeyAuthAuthentication.from(current)
                .switchIfEmpty(Mono.error(new AssertionError("Missing current authentication")))
                .flatMap(result -> KeyAuthAuthentication.from(other).hasElement())
                .doOnNext(present -> assertFalse(present)).then())).verifyComplete();
    }

    @Test
    void ignoresIdentityHeadersAndAttributesWithoutAuthentication() {
        ServerWebExchange exchange = request("test-secret");
        exchange.getAttributes().put(KeyAuthAuthentication.class.getName(), "forged");
        StepVerifier.create(KeyAuthAuthentication.from(exchange.mutate()
                .request(builder -> builder.header("X-Tenant-Id", "admin")).build())).verifyComplete();
    }

    @Test
    void preservesPrincipalHeadersAndExchangeAttributes() {
        Principal principal = () -> "existing-principal";
        ServerWebExchange exchange = request("test-secret").mutate().principal(Mono.just(principal)).build();
        Map<String, Object> attributes = Map.copyOf(exchange.getAttributes());
        StepVerifier.create(authenticate(exchange, current -> current.getPrincipal()
                .doOnNext(value -> assertSame(principal, value))
                .then(required(current.mutate().request(builder -> builder.path("/rewritten")).build())
                        .doOnNext(this::assertRule)).then())).verifyComplete();
        assertEquals(attributes, exchange.getAttributes());
        assertEquals("test-secret", exchange.getRequest().getHeaders().getFirst("X-Api-Key"));
    }

    @Test
    void retainsSnapshotAfterRuleMutation() {
        StepVerifier.create(authenticate(request("test-secret"), current -> required(current).doOnNext(result -> {
            rule.setNamespaceId("replacement-namespace");
            rule.setSelectorId("replacement-selector");
            rule.setId("replacement-rule");
            assertRule(result);
            rule.setSelectorId("verified-selector");
            rule.setId("verified-rule");
        }).then())).verifyComplete();
    }

    @Test
    void missingNamespaceIsNotInventedAsTenant() {
        rule.setNamespaceId(null);
        StepVerifier.create(authenticate(request("test-secret"), current -> required(current)
                .doOnNext(result -> assertNull(result.getNamespaceId())).then())).verifyComplete();
    }

    @Test
    void missingKeyAndInvalidConfigurationNeverInvokeDownstream() {
        ShenyuPluginChain fail = current -> Mono.error(new AssertionError("Unauthenticated invocation"));
        StepVerifier.create(authenticate(MockServerWebExchange.from(MockServerHttpRequest.get("/mcp")), fail)).verifyComplete();
        rule.setHandle("{}");
        handler.handlerRule(rule);
        StepVerifier.create(authenticate(request("test-secret"), fail)).verifyComplete();
        handler.removeRule(rule);
        StepVerifier.create(authenticate(request("test-secret"), fail)).verifyComplete();
    }

    @Test
    void preservesExistingHeaderOrQueryAcceptance() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/mcp")
                .header("X-Api-Key", "wrong").queryParam("X-Api-Key", "test-secret"));
        StepVerifier.create(authenticate(exchange, current -> required(current).doOnNext(this::assertRule).then())).verifyComplete();
    }

    @Test
    void bypassedPluginDoesNotInheritAuthentication() {
        ServerWebExchange exchange = request("test-secret");
        StepVerifier.create(authenticate(exchange, current -> plugin.execute(current, nested ->
                KeyAuthAuthentication.from(nested).hasElement().doOnNext(present -> assertFalse(present)).then())
                .then(required(current).doOnNext(this::assertRule)).then())).verifyComplete();
    }

    @Test
    void disabledPluginDoesNotInheritAuthentication() {
        PluginData data = new PluginData();
        data.setName(plugin.named());
        data.setEnabled(false);
        BaseDataCache.getInstance().cachePluginData(data);
        StepVerifier.create(authenticate(request("test-secret"), current -> plugin.execute(current, nested ->
                KeyAuthAuthentication.from(nested).hasElement().doOnNext(present -> assertFalse(present)).then()))).verifyComplete();
    }

    @Test
    void unmatchedPluginDoesNotInheritAuthentication() {
        PluginData data = new PluginData();
        data.setName(plugin.named());
        data.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(data);
        StepVerifier.create(authenticate(request("test-secret"), current -> plugin.execute(current, nested ->
                KeyAuthAuthentication.from(nested).hasElement().doOnNext(present -> assertFalse(present)).then()))).verifyComplete();
    }

    @Test
    void subscriptionAuthenticatesAgainstCurrentConfiguration() {
        configureMatchingPlugin();
        AtomicInteger calls = new AtomicInteger();
        Mono<Void> execution = plugin.execute(request("test-secret"), current -> required(current)
                .doOnNext(result -> calls.incrementAndGet()).then());
        assertEquals(0, calls.get());
        StepVerifier.create(execution).verifyComplete();
        assertEquals(1, calls.get());
        rule.setHandle("{\"keyName\":\"X-Api-Key\",\"key\":\"rotated-secret\"}");
        handler.handlerRule(rule);
        StepVerifier.create(execution).verifyComplete();
        assertEquals(1, calls.get());
        StepVerifier.create(plugin.execute(request("rotated-secret"), current -> required(current)
                .doOnNext(result -> calls.incrementAndGet()).then())).verifyComplete();
        assertEquals(2, calls.get());
    }

    @Test
    void concurrentRequestsKeepIndependentRuleSnapshots() {
        AtomicInteger calls = new AtomicInteger();
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
            RuleData concurrent = new RuleData();
            concurrent.setId("concurrent-" + index);
            concurrent.setSelectorId("selector-" + index);
            concurrent.setNamespaceId("namespace-" + index);
            concurrent.setHandle(rule.getHandle());
            handler.handlerRule(concurrent);
            ServerWebExchange exchange = request("test-secret");
            return plugin.doExecute(exchange, current -> Mono.delay(Duration.ofMillis(2))
                    .then(required(current)).doOnNext(result -> {
                        assertEquals(concurrent.getId(), result.getRuleId());
                        assertEquals(concurrent.getSelectorId(), result.getSelectorId());
                        assertEquals(concurrent.getNamespaceId(), result.getNamespaceId());
                        calls.incrementAndGet();
                    }).then(), new SelectorData(), concurrent).subscribeOn(Schedulers.parallel())
                    .doFinally(signal -> handler.removeRule(concurrent));
        }, 16)).expectComplete().verify(Duration.ofSeconds(15));
        assertEquals(32, calls.get());
    }

    @Test
    void errorAndCancellationDoNotLeakAuthentication() {
        ServerWebExchange exchange = request("test-secret");
        StepVerifier.create(authenticate(exchange, current -> required(current)
                .flatMap(result -> Mono.error(new IllegalStateException("downstream failed")))))
                .expectErrorMessage("downstream failed").verify();
        StepVerifier.create(KeyAuthAuthentication.from(exchange)).verifyComplete();
        AtomicBoolean cancelled = new AtomicBoolean();
        StepVerifier.create(authenticate(exchange, current -> required(current)
                .then(Mono.<Void>never()).doOnCancel(() -> cancelled.set(true))))
                .thenCancel().verify();
        assertTrue(cancelled.get());
        StepVerifier.create(KeyAuthAuthentication.from(exchange)).verifyComplete();
        StepVerifier.create(authenticate(request("test-secret"), current -> required(current).then())).verifyComplete();
    }

    @Test
    void rotationDoesNotRewriteInflightSnapshot() {
        configureMatchingPlugin();
        Sinks.One<Void> release = Sinks.one();
        ServerWebExchange exchange = request("test-secret");
        StepVerifier.create(plugin.execute(exchange, current -> required(current).flatMap(result -> {
            rule.setHandle("{\"keyName\":\"X-Api-Key\",\"key\":\"rotated-secret\"}");
            handler.handlerRule(rule);
            release.tryEmitEmpty();
            return release.asMono().then(required(current)).doOnNext(value -> assertSame(result, value)).then();
        }))).verifyComplete();
        StepVerifier.create(plugin.execute(request("test-secret"), current ->
                Mono.error(new AssertionError("Revoked credential reached downstream")))).verifyComplete();
    }

    private Mono<KeyAuthAuthentication> required(final ServerWebExchange exchange) {
        return KeyAuthAuthentication.from(exchange).switchIfEmpty(Mono.error(new AssertionError("Missing authentication")));
    }

    private void configureMatchingPlugin() {
        PluginData data = new PluginData();
        data.setName(plugin.named());
        data.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(data);
        SelectorData selector = new SelectorData();
        selector.setId(rule.getSelectorId());
        selector.setPluginName(plugin.named());
        selector.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selector.setEnabled(true);
        selector.setContinued(true);
        selector.setLogged(false);
        selector.setSort(0);
        rule.setEnabled(true);
        rule.setLogged(false);
        rule.setSort(0);
        BaseDataCache.getInstance().cacheSelectData(selector);
        BaseDataCache.getInstance().cacheRuleData(rule);
    }

    private Mono<Void> authenticate(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return plugin.doExecute(exchange, chain, new SelectorData(), rule);
    }

    private ServerWebExchange request(final String key) {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/mcp").header("X-Api-Key", key));
    }

    private void assertRule(final KeyAuthAuthentication result) {
        assertEquals("config-namespace", result.getNamespaceId());
        assertEquals("verified-selector", result.getSelectorId());
        assertEquals("verified-rule", result.getRuleId());
        assertFalse(result.toString().contains("test-secret"));
    }
}
