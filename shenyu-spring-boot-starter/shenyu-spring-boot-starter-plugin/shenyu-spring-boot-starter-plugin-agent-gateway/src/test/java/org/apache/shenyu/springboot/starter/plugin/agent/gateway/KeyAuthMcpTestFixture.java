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

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayPlugin;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.key.auth.KeyAuthPlugin;
import org.apache.shenyu.plugin.key.auth.handler.KeyAuthPluginDataHandler;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Public plugin chain and loopback HTTP with a fixture-owned synchronized configuration cache.
 */
final class KeyAuthMcpTestFixture implements AutoCloseable {

    private final KeyAuthPlugin keyPlugin = new KeyAuthPlugin();

    private final KeyAuthPluginDataHandler keyHandler = new KeyAuthPluginDataHandler();

    private final AgentGatewayPluginDataHandler gatewayHandler = new AgentGatewayPluginDataHandler();

    private final KeyAuthMcpSecurityProperties properties = new KeyAuthMcpSecurityProperties();

    private final List<RuleData> keyRules = new ArrayList<>();

    private final AtomicInteger calls = new AtomicInteger();

    private final AtomicBoolean cancelled = new AtomicBoolean();

    private final AtomicReference<AgentMcpExecutionContext> captured = new AtomicReference<>();

    private volatile Sinks.One<JsonObject> pending;

    private final PluginData keyData;

    private final RuleData gatewayRule;

    private final AgentGatewayPlugin gateway;

    private final WebTestClient client;

    private final LoopResources loops = LoopResources.create("key-auth-mcp-test", 2, true);

    private final DisposableServer server;

    KeyAuthMcpTestFixture(final String mode, final boolean principal) {
        this(mode, principal, null, null);
    }

    KeyAuthMcpTestFixture(final String mode, final org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver resolver,
                         final org.springframework.web.server.WebFilter filter) {
        this(mode, false, resolver, filter);
    }

    private KeyAuthMcpTestFixture(final String mode, final boolean principal,
                                 final org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver resolver, final org.springframework.web.server.WebFilter filter) {
        ConfigurableApplicationContext application = mock(ConfigurableApplicationContext.class);
        ShenyuConfig config = new ShenyuConfig();
        config.getSelectorMatchCache().getCache().setEnabled(false);
        config.getRuleMatchCache().getCache().setEnabled(false);
        when(application.getBean(ShenyuConfig.class)).thenReturn(config);
        when(application.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(application);
        keyData = registerPlugin(keyPlugin.named());
        configureKeys();
        gateway = new AgentGatewayPlugin(new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(
                List.of(tool("read-a"), tool("read-b"), tool("blocked"), tool("outside"))), "test", "1"),
                java.util.Objects.isNull(resolver) ? new KeyAuthMcpSecurityResolver(properties) : resolver));
        registerPlugin(gateway.named());
        gatewayRule = rule(selector(gateway.named(), "gateway-selector", true), "gateway-rule",
                "{\"trafficType\":\"mcp\",\"mcp\":{\"responseMode\":\"" + mode + "\",\"allowedTools\":[\"read-a\",\"read-b\",\"blocked\"]}}");
        gatewayHandler.handlerRule(gatewayRule);
        BaseDataCache.getInstance().cacheRuleData(gatewayRule);
        WebHttpHandlerBuilder builder = WebHttpHandlerBuilder.webHandler(exchange -> execute(principal
                ? exchange.mutate().principal(Mono.just(() -> "other-authority")).build() : exchange));
        if (java.util.Objects.nonNull(filter)) {
            builder.filter(filter);
        }
        server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops)
                .handle(new ReactorHttpHandlerAdapter(builder.build()))
                .bindNow(Duration.ofSeconds(20));
        client = WebTestClient.bindToServer().baseUrl(url()).responseTimeout(Duration.ofSeconds(20)).build();
    }

    Mono<Void> execute(final ServerWebExchange exchange) {
        return keyPlugin.execute(exchange, authenticated -> gateway.execute(authenticated,
                ignored -> Mono.error(new AssertionError("MCP must terminate at Agent Gateway"))));
    }

    String url() {
        return "http://127.0.0.1:" + server.port();
    }

    int calls() {
        return calls.get();
    }

    boolean cancelled() {
        return cancelled.get();
    }

    AgentMcpExecutionContext captured() {
        return captured.get();
    }

    void pending(final Sinks.One<JsonObject> value) {
        pending = value;
    }

    void rotateKey() {
        keyRules.get(0).setHandle("{\"keyName\":\"X-Api-Key\",\"key\":\"rotated\"}");
        keyHandler.handlerRule(keyRules.get(0));
    }

    void disableAuthentication() {
        keyData.setEnabled(false);
        BaseDataCache.getInstance().cachePluginData(keyData);
    }

    void mutateBinding() {
        properties.getBindings().get(0).setTenant("mutated");
        properties.getBindings().get(0).setToolGrants(Set.of("read-b"));
    }

    void revokeTools() {
        gatewayRule.setHandle("{\"trafficType\":\"mcp\",\"mcp\":{\"allowedTools\":[]}}");
        gatewayHandler.handlerRule(gatewayRule);
    }

    void changeNamespace(final String namespace) {
        keyRules.get(0).setNamespaceId(namespace);
        keyHandler.handlerRule(keyRules.get(0));
    }

    WebTestClient.ResponseSpec request(final String path, final String key, final String method, final String tool) {
        return client.post().uri(path).headers(headers -> headers(headers, key, method, tool)).bodyValue(body(method, tool)).exchange();
    }

    JsonObject success(final String path, final String key, final String method, final String tool) {
        return parse(request(path, key, method, tool).expectStatus().isOk().expectBody(String.class)
                .returnResult().getResponseBody());
    }

    static JsonObject parse(final String body) {
        String json = body;
        if (body.startsWith("event: message\ndata: ")) {
            assertEquals(1, body.split("event: message", -1).length - 1);
            assertTrue(body.endsWith("\n\n"));
            json = body.substring("event: message\ndata: ".length()).trim();
        }
        return JsonParser.parseString(json).getAsJsonObject();
    }

    String body(final String method, final String tool) {
        JsonObject request = new JsonObject();
        request.addProperty("jsonrpc", "2.0");
        request.addProperty("id", 1);
        request.addProperty("method", method);
        JsonObject params = JsonParser.parseString("{\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\","
                + "\"io.modelcontextprotocol/clientCapabilities\":{}}}").getAsJsonObject();
        if (!tool.isEmpty()) {
            params.addProperty("name", tool);
            params.add("arguments", new JsonObject());
        }
        request.add("params", params);
        return request.toString();
    }

    static void headers(final HttpHeaders headers, final String key, final String method, final String tool) {
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Accept", "application/json, text/event-stream");
        headers.set("MCP-Protocol-Version", "2026-07-28");
        headers.set("Mcp-Method", method);
        if (!tool.isEmpty()) {
            headers.set("Mcp-Name", tool);
        }
        if (!key.isEmpty()) {
            headers.set("X-Api-Key", key);
        }
        headers.set("X-Tenant-Id", "forged");
        headers.set("X-Subject", "forged");
    }

    private void configureKeys() {
        SelectorData selector = selector(keyPlugin.named(), "auth-selector", false);
        List<KeyAuthMcpSecurityProperties.Binding> bindings = new ArrayList<>();
        for (String tenant : List.of("a", "b", "unbound")) {
            final RuleData rule = rule(selector, "rule-" + tenant, "{\"keyName\":\"X-Api-Key\",\"key\":\"key-" + tenant + "\"}");
            ConditionData condition = new ConditionData();
            condition.setParamType("uri");
            condition.setOperator("=");
            condition.setParamValue("/" + tenant);
            rule.setConditionDataList(List.of(condition));
            keyHandler.handlerRule(rule);
            BaseDataCache.getInstance().cacheRuleData(rule);
            keyRules.add(rule);
            if (!"unbound".equals(tenant)) {
                KeyAuthMcpSecurityProperties.Binding binding = new KeyAuthMcpSecurityProperties.Binding();
                binding.setNamespaceId("namespace");
                binding.setSelectorId(selector.getId());
                binding.setRuleId(rule.getId());
                binding.setTenant("tenant-" + tenant);
                binding.setSubject("service");
                binding.setToolGrants(Set.of("read-" + tenant, "blocked", "outside"));
                binding.setToolDenials(Set.of("blocked"));
                bindings.add(binding);
            }
        }
        properties.setBindings(bindings);
    }

    private AgentToolProvider tool(final String name) {
        AgentToolProvider provider = mock(AgentToolProvider.class);
        when(provider.getName()).thenReturn(name);
        when(provider.getDescription()).thenReturn("Test tool " + name);
        when(provider.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        when(provider.getRequiredClientCapabilities()).thenReturn(new JsonObject());
        when(provider.invoke(any())).thenAnswer(ignored -> Mono.deferContextual(context -> {
            AgentMcpExecutionContext identity = context.get(AgentMcpExecutionContext.class);
            captured.set(identity);
            calls.incrementAndGet();
            JsonObject result = new JsonObject();
            result.addProperty("tenant", identity.getTenant().orElseThrow());
            result.addProperty("subject", identity.getSubject());
            Sinks.One<JsonObject> current = pending;
            return java.util.Objects.isNull(current) ? Mono.just(result).publishOn(Schedulers.parallel())
                    : current.asMono().doOnCancel(() -> cancelled.set(true));
        }));
        return provider;
    }

    private PluginData registerPlugin(final String name) {
        PluginData data = new PluginData();
        data.setName(name);
        data.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(data);
        return data;
    }

    private SelectorData selector(final String pluginName, final String id, final boolean full) {
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setOperator("match");
        condition.setParamValue("/**");
        SelectorData data = SelectorData.builder().id(id).pluginName(pluginName).enabled(true).continued(true)
                .logged(false).sort(0).type(full ? 0 : 1).matchMode(0).conditionList(List.of(condition)).build();
        BaseDataCache.getInstance().cacheSelectData(data);
        return data;
    }

    private RuleData rule(final SelectorData route, final String id, final String handle) {
        return RuleData.builder().id(id).selectorId(route.getId()).pluginName(route.getPluginName())
                .namespaceId("namespace").enabled(true).logged(false).sort(0).matchMode(0).handle(handle).build();
    }

    @Override
    public void close() {
        server.disposeNow(Duration.ofSeconds(10));
        loops.disposeLater().block(Duration.ofSeconds(10));
        keyRules.forEach(keyHandler::removeRule);
        gatewayHandler.removeRule(gatewayRule);
        BaseDataCache cache = BaseDataCache.getInstance();
        cache.removeRuleDataBySelectorId("auth-selector");
        cache.removeRuleDataBySelectorId("gateway-selector");
        for (String name : List.of(keyPlugin.named(), gateway.named())) {
            cache.removeSelectDataByPluginName(name);
            cache.removePluginDataByPluginName(name);
        }
    }
}
