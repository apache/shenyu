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

package org.apache.shenyu.plugin.wasm.base.gateway;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmPluginDataHandler;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Bootstraps the gateway starter on a real HTTP port with a registered WASM plugin.
 */
@SpringBootTest(classes = WasmGatewayTest.GatewayApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {"spring.main.web-application-type=reactive", "spring.main.allow-bean-definition-overriding=true",
                "shenyu.local.enabled=false", "shenyu.alert.enabled=false"})
public final class WasmGatewayTest {

    @Autowired
    private ConfigurableApplicationContext context;

    @Autowired
    private PluginDataSubscriber subscriber;

    @Autowired
    private ShenyuWebHandler webHandler;

    @Autowired
    private ReferenceWasmPlugin plugin;

    @Autowired
    private ReferenceWasmPluginDataHandler handler;

    @LocalServerPort
    private int port;

    private WebTestClient client;

    private PluginData pluginData;

    private SelectorData selector;

    private RuleData rule;

    @BeforeEach
    public void setUp() {
        // Other module tests install mock contexts in this static utility.
        SpringBeanUtils.getInstance().setApplicationContext(context);
        clearCaches();
        handler.events().clear();
        webHandler.putExtPlugins(List.of(plugin));
        client = WebTestClient.bindToServer().baseUrl("http://localhost:" + port).responseTimeout(Duration.ofSeconds(15)).build();
        pluginData = PluginData.builder().id("wasm-plugin").name(ReferenceWasmPlugin.NAME).enabled(true).sort(-100).config("v1").build();
        selector = SelectorData.builder().id("wasm-selector").pluginName(ReferenceWasmPlugin.NAME).enabled(true).sort(0)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).continued(true).matchMode(0).matchRestful(false).logged(false)
                .handle("/wasm/").conditionList(List.of(uriCondition())).build();
        rule = RuleData.builder().id("wasm-rule").selectorId(selector.getId()).pluginName(ReferenceWasmPlugin.NAME)
                .enabled(true).sort(0).matchMode(0).matchRestful(false).loged(false).handle("secret")
                .conditionDataList(List.of(uriCondition())).build();
        subscriber.onSubscribe(pluginData);
        subscriber.onSelectorSubscribe(selector);
        subscriber.onRuleSubscribe(rule);
    }

    @AfterEach
    public void tearDown() {
        assertEquals(0, ReferenceWasmPlugin.pendingArguments());
        assertEquals(0, ReferenceWasmPluginDataHandler.pendingArguments());
        clearCaches();
        webHandler.putExtPlugins(List.of(plugin));
    }

    @Test
    public void requestDataAndGuestDecisionControlTheChain() {
        assertTrue(webHandler.getPlugins().contains(plugin));
        request("secret", "request-1", "hello 世界").expectStatus().isOk()
                .expectHeader().valueEquals("X-Wasm-Decision", "allow")
                .expectHeader().valueEquals("X-Chain-Reached", "true")
                .expectBody().jsonPath("$.selectorId").isEqualTo(selector.getId())
                .jsonPath("$.selectorHandle").isEqualTo("/wasm/")
                .jsonPath("$.ruleId").isEqualTo(rule.getId()).jsonPath("$.ruleHandle").isEqualTo("secret")
                .jsonPath("$.method").isEqualTo("POST").jsonPath("$.path").isEqualTo("/wasm/check")
                .jsonPath("$.query").isEqualTo("source=gateway").jsonPath("$.token").isEqualTo("secret")
                .jsonPath("$.requestId").isEqualTo("request-1").jsonPath("$.body").isEqualTo("hello 世界");
        request("wrong", "request-2", "rejected").expectStatus().isForbidden()
                .expectHeader().valueEquals("X-Wasm-Decision", "deny").expectHeader().doesNotExist("X-Chain-Reached")
                .expectBody().jsonPath("$.decision").isEqualTo("deny").jsonPath("$.requestId").isEqualTo("request-2");
    }

    @Test
    public void configurationUpdatesAndDeletesReachTheGuestAndInvalidateMatching() {
        assertEvent(0, 0, pluginData);
        assertEvent(1, 1, selector);
        assertEvent(2, 2, rule);
        request("secret", "initial", "").expectStatus().isOk();
        rule.setHandle("updated");
        subscriber.onRuleSubscribe(rule);
        assertEvent(3, 2, rule);
        request("secret", "old-token", "").expectStatus().isForbidden();
        request("updated", "new-token", "").expectStatus().isOk();
        selector.setHandle("/different/");
        subscriber.onSelectorSubscribe(selector);
        assertEvent(4, 1, selector);
        request("updated", "new-selector", "").expectStatus().isForbidden();
        selector.setHandle("/wasm/");
        subscriber.onSelectorSubscribe(selector);
        subscriber.unRuleSubscribe(rule);
        assertEvent(6, 5, rule);
        expectBypass();
        subscriber.onRuleSubscribe(rule);
        request("updated", "rule-restored", "").expectStatus().isOk();
        subscriber.unSelectorSubscribe(selector);
        assertEvent(8, 4, selector);
        expectBypass();
        subscriber.onSelectorSubscribe(selector);
        subscriber.onRuleSubscribe(rule);
        request("updated", "selector-restored", "").expectStatus().isOk();
        pluginData.setConfig("v2");
        pluginData.setEnabled(false);
        subscriber.onSubscribe(pluginData);
        assertEvent(11, 0, pluginData);
        expectBypass();
        pluginData.setEnabled(true);
        subscriber.onSubscribe(pluginData);
        request("updated", "plugin-restored", "").expectStatus().isOk();
        subscriber.unSubscribe(pluginData);
        assertEvent(13, 3, pluginData);
        expectBypass();
    }

    @Test
    public void unmatchedRequestsContinueWithoutInvokingTheGuest() {
        client.get().uri("/outside").exchange().expectStatus().isOk()
                .expectHeader().doesNotExist("X-Wasm-Decision").expectHeader().valueEquals("X-Chain-Reached", "true");
    }

    @Test
    public void concurrentRequestsKeepArgumentsMemoryAndResultsIsolated() throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(12);
        try {
            List<Callable<Void>> calls = new ArrayList<>();
            for (int i = 0; i < 60; i++) {
                final String id = "parallel-" + i;
                final boolean allow = i % 2 == 0;
                calls.add(() -> {
                    WebTestClient.ResponseSpec response = request(allow ? "secret" : id, id, "body-" + id);
                    response.expectStatus().isEqualTo(allow ? HttpStatus.OK : HttpStatus.FORBIDDEN);
                    if (allow) {
                        response.expectHeader().valueEquals("X-Chain-Reached", "true");
                    } else {
                        response.expectHeader().doesNotExist("X-Chain-Reached");
                    }
                    response.expectBody().jsonPath("$.decision").isEqualTo(allow ? "allow" : "deny")
                            .jsonPath("$.requestId").isEqualTo(id).jsonPath("$.body").isEqualTo("body-" + id);
                    return null;
                });
            }
            for (Future<Void> future : executor.invokeAll(calls, 30, TimeUnit.SECONDS)) {
                future.get();
            }
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    public void missingRequiredExecutionReturnsTheGatewayError() {
        webHandler.putExtPlugins(List.of(new MissingExecutePlugin()));
        request("secret", "missing-export", "").expectStatus().is5xxServerError()
                .expectHeader().doesNotExist("X-Chain-Reached")
                .expectBody().jsonPath("$.code").isEqualTo(ShenyuResultEnum.WASM_FUNC_NOT_FOUND.getCode());
    }

    @Test
    public void missingConfigurationHooksAreOptional() {
        try (MissingHooksHandler missing = new MissingHooksHandler()) {
            missing.handlerPlugin(pluginData);
            missing.handlerSelector(selector);
            missing.handlerRule(rule);
            missing.removeRule(rule);
            missing.removeSelector(selector);
            missing.removePlugin(pluginData);
            assertFalse(missing.getWasmExtern("handlerRule").isPresent());
        }
    }

    @Test
    public void guestTrapFailsTheRequestAndReleasesArguments() {
        webHandler.putExtPlugins(List.of(new TrappingPlugin()));
        expectFailureAndRecovery();
    }

    @Test
    public void invalidGuestMemoryFailsTheRequestAndReleasesArguments() {
        webHandler.putExtPlugins(List.of(new InvalidMemoryPlugin()));
        expectFailureAndRecovery();
    }

    @Test
    public void unsupportedGuestResultVersionFailsTheRequest() {
        webHandler.putExtPlugins(List.of(new IncompatibleResultPlugin()));
        expectFailureAndRecovery();
    }

    private void expectFailureAndRecovery() {
        request("secret", "failure", "").expectStatus().is5xxServerError().expectHeader().doesNotExist("X-Chain-Reached");
        assertEquals(0, ReferenceWasmPlugin.pendingArguments());
        webHandler.putExtPlugins(List.of(plugin));
        request("secret", "recovery", "after failure").expectStatus().isOk()
                .expectBody().jsonPath("$.requestId").isEqualTo("recovery");
    }

    private WebTestClient.ResponseSpec request(final String token, final String id, final String body) {
        return client.post().uri("/wasm/check?source=gateway").header("X-Token", token).header("X-Request-Id", id)
                .contentType(MediaType.TEXT_PLAIN).bodyValue(body).exchange();
    }

    private void expectBypass() {
        request("updated", "bypass", "").expectStatus().isOk().expectHeader().doesNotExist("X-Wasm-Decision")
                .expectHeader().valueEquals("X-Chain-Reached", "true");
    }

    private void assertEvent(final int index, final int event, final Object data) {
        assertEquals(event, handler.events().get(index).get("event"));
        assertEquals(GsonUtils.getInstance().toJson(data), handler.events().get(index).get("json"));
    }

    private static ConditionData uriCondition() {
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setParamName("/");
        condition.setOperator("match");
        condition.setParamValue("/wasm/**");
        return condition;
    }

    private static void clearCaches() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    /**
     * Minimal gateway bootstrap: the same gateway starter used by shenyu-bootstrap.
     */
    @SpringBootConfiguration
    @EnableAutoConfiguration
    static class GatewayApplication {

        @Bean
        ReferenceWasmPlugin referenceWasmPlugin() {
            return new ReferenceWasmPlugin();
        }

        @Bean
        ReferenceWasmPluginDataHandler referenceWasmPluginDataHandler() {
            return new ReferenceWasmPluginDataHandler();
        }

        @Bean
        ShenyuPlugin terminalPlugin() {
            return new ShenyuPlugin() {
                @Override
                public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
                    exchange.getResponse().getHeaders().set("X-Chain-Reached", "true");
                    Object result = exchange.getAttribute(ReferenceWasmPlugin.RESULT);
                    return WebFluxResultUtils.result(exchange, Objects.nonNull(result) ? result : Map.of("bypass", true));
                }

                @Override
                public int getOrder() {
                    return -90;
                }

                @Override
                public String named() {
                    return "wasmTerminal";
                }
            };
        }
    }

    /** Loads the memory-only fixture without a doExecute export. */
    static final class MissingExecutePlugin extends ReferenceWasmPlugin {
    }

    /** Loads a guest that traps after reading its arguments. */
    static final class TrappingPlugin extends ReferenceWasmPlugin {
    }

    /** Loads a guest that passes an out-of-bounds result pointer. */
    static final class InvalidMemoryPlugin extends ReferenceWasmPlugin {
    }

    /** Loads a guest returning an unsupported reference profile version. */
    static final class IncompatibleResultPlugin extends ReferenceWasmPlugin {
    }

    /** Loads a handler with none of the optional configuration exports. */
    static final class MissingHooksHandler extends AbstractWasmPluginDataHandler {
        @Override
        public String pluginNamed() {
            return ReferenceWasmPlugin.NAME;
        }
    }
}
