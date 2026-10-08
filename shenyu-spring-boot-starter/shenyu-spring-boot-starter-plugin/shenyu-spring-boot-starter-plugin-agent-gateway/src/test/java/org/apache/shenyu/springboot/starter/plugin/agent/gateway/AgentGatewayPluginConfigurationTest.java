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

import org.apache.shenyu.plugin.agent.gateway.AgentGatewayPlugin;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;

class AgentGatewayPluginConfigurationTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withUserConfiguration(AgentGatewayPluginConfiguration.class);

    @Test
    void shouldKeepPluginUnassembledByDefault() {
        contextRunner.run(context -> {
            assertFalse(context.containsBean("agentGatewayPlugin"));
            assertFalse(context.containsBean("agentGatewayPluginDataHandler"));
        });
    }

    @Test
    void shouldAssemblePluginWhenEnabled() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true")
                .run(context -> {
                    assertTrue(context.getBean("agentGatewayPlugin") instanceof AgentGatewayPlugin);
                    assertTrue(context.getBean(ShenyuPlugin.class) instanceof AgentGatewayPlugin);
                    assertTrue(context.getBean("agentGatewayPluginDataHandler") instanceof PluginDataHandler);
                });
    }

    @Test
    void shouldDenyMcpWhenNoSecurityAdapterIsRegistered() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true").run(context -> {
            AgentMcpHttpHandler handler = (AgentMcpHttpHandler) ReflectionTestUtils.getField(context.getBean(AgentGatewayPlugin.class), "mcpHandler");
            MockServerWebExchange exchange = exchange();
            StepVerifier.withVirtualTime(() -> handler.handle(exchange, AgentGatewayMcpConfig.parse(new JsonObject(), true),
                    new AgentTrafficContext("server-id", "mcp", "selector", "rule"), 1)).verifyComplete();
            assertEquals(401, exchange.getResponse().getStatusCode().value());
        });
    }

    @Test
    void shouldWireExplicitToolAndTrustedSecurityAdapter() {
        AgentToolProvider tool = mock(AgentToolProvider.class);
        when(tool.getName()).thenReturn("read");
        when(tool.getDescription()).thenReturn("Read a resource");
        when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        when(tool.getRequiredClientCapabilities()).thenReturn(JsonParser.parseString("{}").getAsJsonObject());
        when(tool.invoke(any())).thenReturn(Mono.just(new JsonObject()));
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true")
                .withBean(AgentToolProvider.class, () -> tool)
                .withBean(AgentMcpSecurityResolver.class, () -> next -> Mono.just(new AgentMcpIdentity("verified", Set.of("read"))))
                .run(context -> {
                    AgentMcpHttpHandler handler = (AgentMcpHttpHandler) ReflectionTestUtils.getField(context.getBean(AgentGatewayPlugin.class), "mcpHandler");
                    MockServerWebExchange exchange = exchange();
                    AgentGatewayMcpConfig config = AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedTools\":[\"read\"]}"), true);
                    StepVerifier.withVirtualTime(() -> handler.handle(exchange, config,
                            new AgentTrafficContext("server-id", "mcp", "selector", "rule"), 1)).verifyComplete();
                    assertEquals(200, exchange.getResponse().getStatusCode().value());
                    assertTrue(exchange.getResponse().getBodyAsString().block().contains("\"isError\":false"));
                });
    }

    @Test
    void shouldFailAssemblyForAmbiguousSecurityAdapters() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true")
                .withBean("firstSecurity", AgentMcpSecurityResolver.class, () -> next -> Mono.empty())
                .withBean("secondSecurity", AgentMcpSecurityResolver.class, () -> next -> Mono.empty())
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void shouldRequireDeploymentAllowlistForAggregation() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true", "shenyu.plugins.agent.gateway.aggregation.enabled=true")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void shouldAssembleOptInCatalogAndWireTrustedPluginSync() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true", "shenyu.plugins.agent.gateway.aggregation.enabled=true",
                        "shenyu.plugins.agent.gateway.aggregation.allowed-endpoints=https://192.0.2.10:443/mcp")
                .withBean(org.apache.shenyu.plugin.agent.gateway.remote.RemoteServiceCredentialResolver.class, () -> reference -> {
                    throw new SecurityException("Missing fixture credentials");
                }).run(context -> {
                    final var catalog = context.getBean(org.apache.shenyu.plugin.agent.gateway.remote.ManagedRemoteMcpCatalog.class);
                    var event = new org.apache.shenyu.common.dto.PluginData();
                    event.setEnabled(true);
                    event.setConfig("{}");
                    context.getBean(PluginDataHandler.class).handlerPlugin(event);
                    assertNotNull(catalog);
                    assertEquals(0, catalog.diagnostics().get("clients"));
                });
    }

    @Test
    void shouldRejectDomainAllowlistWithoutRelaxingTlsPolicy() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true", "shenyu.plugins.agent.gateway.aggregation.enabled=true",
                        "shenyu.plugins.agent.gateway.aggregation.allowed-endpoints=https://example.com:443/mcp")
                .withBean(org.apache.shenyu.plugin.agent.gateway.remote.RemoteServiceCredentialResolver.class, () -> reference -> {
                    throw new SecurityException("unused");
                }).run(context -> assertNotNull(context.getStartupFailure()));
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp")
                .header("Content-Type", "application/json").header("Accept", "application/json, text/event-stream")
                .header("MCP-Protocol-Version", "2026-07-28").header("Mcp-Method", "tools/call").header("Mcp-Name", "read")
                .body("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"read\",\"_meta\":{"
                        + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}"));
    }
}
