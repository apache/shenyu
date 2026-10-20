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

import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayPlugin;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.context.FilteredClassLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Instant;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class KeyAuthMcpSecurityConfigurationTest {

    private static final String PREFIX = "shenyu.plugins.agent.gateway.security.key-auth.";

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withUserConfiguration(AgentGatewayPluginConfiguration.class)
            .withPropertyValues("shenyu.plugins.agent.gateway.enabled=true");

    @Test
    void principalAdapterIsOptionalAndRequiresExplicitSource() {
        runner.withPropertyValues("shenyu.plugins.agent.gateway.security.principal.enabled=true")
                .run(context -> assertNotNull(context.getStartupFailure()));
        runner.withPropertyValues("shenyu.plugins.agent.gateway.security.principal.enabled=true",
                        "shenyu.plugins.agent.gateway.security.principal.source=unknown")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void principalIdentityCanOnlyBeConfiguredOnce() {
        runner.withPropertyValues("shenyu.plugins.agent.gateway.security.principal.enabled=true",
                        "shenyu.plugins.agent.gateway.security.principal.source=jwt",
                        "shenyu.plugins.agent.gateway.security.principal.issuer=https://issuer.example",
                        "shenyu.plugins.agent.gateway.security.principal.audience=mcp")
                .run(context -> {
                    assertNull(context.getStartupFailure());
                    assertEquals(1, context.getBeansOfType(AgentMcpSecurityResolver.class).size());
                });
    }

    @Test
    void principalDependenciesAreOptionalWhenDisabled() {
        runner.withClassLoader(new FilteredClassLoader("org.springframework.security"))
                .run(context -> assertNull(context.getStartupFailure()));
        principalConfigured().withClassLoader(new FilteredClassLoader("org.springframework.security"))
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"org.springframework.security.oauth2.jwt", "org.springframework.security.oauth2.server.resource"})
    void principalRequiresItsSelectedDependency(final String dependency) {
        principalConfigured().withClassLoader(new FilteredClassLoader(dependency))
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void loginRequiresOnlyItsOwnOptionalDependencies() {
        runner.withClassLoader(new FilteredClassLoader("org.springframework.security.oauth2.jwt", "org.springframework.security.oauth2.server.resource"))
                .withPropertyValues("shenyu.plugins.agent.gateway.security.principal.enabled=true",
                        "shenyu.plugins.agent.gateway.security.principal.source=oauth2-login",
                        "shenyu.plugins.agent.gateway.security.principal.registration-id=trusted")
                .run(context -> assertNull(context.getStartupFailure()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"issuer=", "audience=", "default-tenant=", "registration-id=unexpected",
        "scope-mappings[0].tool-grants=*", "scope-mappings[0].scope=", "scope-mappings[0].tool-denys=read", "unknown=true"})
    void principalRejectsInvalidPolicy(final String invalid) {
        principalConfigured().withPropertyValues("shenyu.plugins.agent.gateway.security.principal." + invalid)
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void principalRejectsDuplicateScopeBindings() {
        principalConfigured().withPropertyValues("shenyu.plugins.agent.gateway.security.principal.scope-mappings[1].scope=inventory:read")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void principalConflictsWithCustomResolverEvenIfPrimary() {
        principalConfigured().withBean("other", AgentMcpSecurityResolver.class, () -> exchange -> Mono.empty(), definition -> definition.setPrimary(true))
                .run(context -> assertNotNull(context.getStartupFailure()));
        configured().withPropertyValues("shenyu.plugins.agent.gateway.security.principal.enabled=true",
                        "shenyu.plugins.agent.gateway.security.principal.source=jwt",
                        "shenyu.plugins.agent.gateway.security.principal.issuer=https://issuer.example",
                        "shenyu.plugins.agent.gateway.security.principal.audience=mcp")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    private ApplicationContextRunner principalConfigured() {
        return runner.withPropertyValues("shenyu.plugins.agent.gateway.security.principal.enabled=true",
                "shenyu.plugins.agent.gateway.security.principal.source=jwt",
                "shenyu.plugins.agent.gateway.security.principal.issuer=https://issuer.example",
                "shenyu.plugins.agent.gateway.security.principal.audience=mcp",
                "shenyu.plugins.agent.gateway.security.principal.scope-mappings[0].scope=inventory:read",
                "shenyu.plugins.agent.gateway.security.principal.scope-mappings[0].tool-grants=read");
    }

    @Test
    void remainsOptionalAndOffByDefault() {
        runner.withClassLoader(new FilteredClassLoader("org.apache.shenyu.plugin.key.auth"))
                .run(context -> {
                    assertNull(context.getStartupFailure());
                    assertNotNull(context.getBean(AgentGatewayPlugin.class));
                    assertFalse(context.containsBean("agentGatewayKeyAuthSecurityResolver"));
                });
    }

    @Test
    void enabledAdapterRequiresDependency() {
        configured().withClassLoader(new FilteredClassLoader("org.apache.shenyu.plugin.key.auth"))
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void bindsExplicitPolicyButDoesNotAuthenticateHeaders() {
        configured().run(context -> {
            assertNull(context.getStartupFailure());
            AgentMcpSecurityResolver resolver = context.getBean(AgentMcpSecurityResolver.class);
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/mcp")
                    .header("X-Api-Key", "secret").header("X-Tenant-Id", "tenant").header("X-Subject", "service"));
            StepVerifier.create(resolver.resolve(exchange)).verifyComplete();
        });
    }

    @ParameterizedTest
    @ValueSource(strings = {"namespace-id", "selector-id", "rule-id", "tenant", "subject"})
    void rejectsMissingIdentityFields(final String field) {
        configured().withPropertyValues(PREFIX + "bindings[0]." + field + "=")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void rejectsEmptyPolicy() {
        runner.withPropertyValues(PREFIX + "enabled=true").run(context -> assertNotNull(context.getStartupFailure()));
    }

    @ParameterizedTest
    @ValueSource(strings = {"tool-grants", "tool-denials"})
    void rejectsWildcardPermissions(final String field) {
        configured().withPropertyValues(PREFIX + "bindings[0]." + field + "=*")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void rejectsUnknownConfigurationFields() {
        configured().withPropertyValues(PREFIX + "bindings[0].tool-denys=read")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void rejectsUnboundedOrControlCharacterIdentityBindings() {
        configured().withPropertyValues(PREFIX + "bindings[0].tenant=" + "x".repeat(257))
                .run(context -> assertNotNull(context.getStartupFailure()));
        configured().withPropertyValues(PREFIX + "bindings[0].subject=service" + (char) 9 + "other")
                .run(context -> assertNotNull(context.getStartupFailure()));
        KeyAuthMcpSecurityProperties properties = new KeyAuthMcpSecurityProperties();
        properties.setBindings(java.util.Collections.nCopies(1025, new KeyAuthMcpSecurityProperties.Binding()));
        assertThrows(IllegalArgumentException.class, () -> new KeyAuthMcpSecurityResolver(properties));
    }

    @Test
    void rejectsDuplicateRuleReferences() {
        configured().withPropertyValues(PREFIX + "bindings[1].namespace-id=namespace", PREFIX + "bindings[1].selector-id=selector",
                        PREFIX + "bindings[1].rule-id=rule", PREFIX + "bindings[1].tenant=another", PREFIX + "bindings[1].subject=service")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void primaryCannotHideConflictingAuthority() {
        configured().withBean("other", AgentMcpSecurityResolver.class, () -> exchange -> Mono.empty(), definition -> definition.setPrimary(true))
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void immutableIdentityDenialsPrecedeRuleIntersection() {
        Set<String> grants = new HashSet<>(Set.of("read", "denied", "outside"));
        Set<String> denials = new HashSet<>(Set.of("denied"));
        AgentMcpIdentity identity = new AgentMcpIdentity("tenant", "service", grants, denials);
        grants.clear();
        denials.clear();
        AgentMcpExecutionContext context = new AgentMcpExecutionContext("request", identity.getSubject(), "rule", 1,
                Set.of("read", "denied"), identity.getToolGrants(), Instant.MAX, AgentGatewayResultFilterConfig.empty(), identity.getTenant().orElseThrow());
        assertEquals(Set.of("read"), context.getAllowedTools());
        assertEquals(Set.of("denied"), identity.getToolDenials());
        assertEquals("tenant", context.getTenant().orElseThrow());
        assertThrows(UnsupportedOperationException.class, () -> identity.getToolGrants().add("denied"));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpIdentity(" ", "service", Set.of(), Set.of()));
        assertFalse(new AgentMcpIdentity("legacy", Set.of()).getTenant().isPresent());
    }

    private ApplicationContextRunner configured() {
        return runner.withPropertyValues(PREFIX + "enabled=true", PREFIX + "bindings[0].namespace-id=namespace",
                PREFIX + "bindings[0].selector-id=selector", PREFIX + "bindings[0].rule-id=rule", PREFIX + "bindings[0].tenant=tenant",
                PREFIX + "bindings[0].subject=service", PREFIX + "bindings[0].tool-grants=read,denied", PREFIX + "bindings[0].tool-denials=denied");
    }
}
