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

import io.opentelemetry.api.OpenTelemetry;
import org.apache.shenyu.common.audit.AgentInvocationAuditBridge;
import org.apache.shenyu.common.audit.AgentInvocationAuditObserver;
import org.apache.shenyu.common.metrics.AgentMcpTerminalObserver;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpCallTracer;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayPlugin;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayConstants;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpResourceAdmission;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceProvider;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceRegistry;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.core.env.Environment;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.apache.shenyu.plugin.agent.gateway.remote.ManagedRemoteMcpCatalog;
import org.apache.shenyu.plugin.agent.gateway.remote.RemoteServiceCredentialResolver;
import org.apache.shenyu.plugin.agent.gateway.remote.FileRemoteServiceCredentialResolver;
import java.net.URI;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import reactor.core.publisher.Mono;

/**
 * Agent gateway plugin auto configuration.
 */
@Configuration
@ConditionalOnProperty(value = "shenyu.plugins.agent.gateway.enabled", havingValue = "true", matchIfMissing = false)
public class AgentGatewayPluginConfiguration {

    private static final String JWT_SECURITY_RESOLVER_BEAN_NAME = "agentGatewayJwtSecurityResolver";

    /**
     * Create a plugin with an optional managed remote catalog.
     * @param providers local providers
     * @param resolvers trusted identity adapters
     * @param catalogs optional, unambiguous managed remote catalog
     * @param resources explicitly registered local resources
     * @param tracers optional tracing bridge
     * @param terminalObservers optional application-owned terminal event sink
     * @param aiObservers optional application-owned AI attempt and terminal sink
     * @param auditors optional unified audit sink
     * @param environment deployment-controlled tracing opt-in
     * @param beanFactory application bean factory used to verify the conditional JWT resolver
     * @return the plugin
     */
    @Bean
    public ShenyuPlugin agentGatewayPlugin(final ObjectProvider<AgentToolProvider> providers, final ObjectProvider<AgentMcpSecurityResolver> resolvers,
                                           final ObjectProvider<AgentMcpRemoteCatalog> catalogs, final ObjectProvider<AgentResourceProvider> resources,
                                           final ObjectProvider<AgentMcpCallTracer> tracers,
                                           final ObjectProvider<AgentMcpTerminalObserver> terminalObservers,
                                           final ObjectProvider<AiCallObserver> aiObservers,
                                           final ObjectProvider<AgentInvocationAuditObserver> auditors,
                                           final Environment environment, final BeanFactory beanFactory) {
        AgentMcpCallTracer tracer = tracers.getIfAvailable();
        if (environment.getProperty("shenyu.plugins.agent.gateway.tracing.enabled", Boolean.class, false) && java.util.Objects.isNull(tracer)) {
            throw new IllegalStateException("Enabled MCP tracing requires an application tracing bridge");
        }
        List<AgentMcpSecurityResolver> authorities = resolvers.stream().toList();
        if (authorities.size() > 1) {
            throw new IllegalStateException("MCP requires exactly one authoritative security resolver");
        }
        if (environment.getProperty("shenyu.plugins.agent.gateway.security.key-auth.enabled", Boolean.class, false)
                && authorities.stream().noneMatch(resolver -> resolver instanceof KeyAuthMcpSecurityResolver)) {
            throw new IllegalStateException("Enabled key-auth MCP authorization requires the key-auth plugin dependency");
        }
        if (environment.getProperty("shenyu.plugins.agent.gateway.security.principal.enabled", Boolean.class, false)
                && authorities.stream().noneMatch(resolver -> "org.apache.shenyu.springboot.starter.plugin.agent.gateway.PrincipalMcpSecurityResolver".equals(resolver.getClass().getName()))) {
            throw new IllegalStateException("Enabled MCP principal authorization requires an explicit source and its optional dependencies");
        }
        if (environment.getProperty("shenyu.plugins.agent.gateway.security.jwt.enabled", Boolean.class, false)
                && !beanFactory.containsBean(JWT_SECURITY_RESOLVER_BEAN_NAME)) {
            throw new IllegalStateException("Enabled MCP JWT identity requires the optional JWT adapter dependencies");
        }
        return createPlugin(providers, authorities, catalogs.getIfAvailable(), tracer, resources,
                AgentInvocationAuditBridge.mcp(terminalObservers.getIfAvailable(), auditors.getIfAvailable()),
                AgentInvocationAuditBridge.ai(aiObservers.getIfAvailable(), auditors.getIfAvailable()),
                environment.getProperty("shenyu.plugins.agent.gateway.security.llm.enabled", Boolean.class, false));
    }

    private ShenyuPlugin createPlugin(final ObjectProvider<AgentToolProvider> providers, final List<AgentMcpSecurityResolver> resolvers,
                                      final AgentMcpRemoteCatalog catalog, final AgentMcpCallTracer tracer,
                                      final ObjectProvider<AgentResourceProvider> resources,
                                      final AgentMcpTerminalObserver terminalObserver, final AiCallObserver aiObserver,
                                      final boolean llmIdentityEnabled) {
        AgentToolRegistry registry = new AgentToolRegistry(providers.orderedStream().toList());
        AgentMcpSecurityResolver resolver = resolvers.isEmpty() ? exchange -> Mono.empty() : resolvers.get(0);
        if (llmIdentityEnabled && resolvers.isEmpty()) {
            throw new IllegalStateException("Enabled LLM identity mapping requires an authoritative security resolver");
        }
        AgentResourceRegistry resourceRegistry = new AgentResourceRegistry(resources.orderedStream().toList());
        AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(registry, "shenyu-agent-gateway",
                AgentGatewayConstants.MCP_SERVER_VERSION, catalog, resourceRegistry,
                java.util.Objects.isNull(catalog) ? AgentMcpResourceAdmission.NOOP : catalog.admission());
        return new AgentGatewayPlugin(new AgentMcpHttpHandler(dispatcher, resolver, tracer, terminalObserver),
                llmIdentityEnabled ? resolver : null, aiObserver);
    }

    /**
     * Create the agent gateway plugin data handler.
     *
     * @param catalogs optional data-sync managed remote catalog
     * @return the data handler
     */
    @Bean
    public PluginDataHandler agentGatewayPluginDataHandler(final ObjectProvider<ManagedRemoteMcpCatalog> catalogs) {
        ManagedRemoteMcpCatalog catalog = catalogs.getIfAvailable();
        return new AgentGatewayPluginDataHandler(java.util.Objects.isNull(catalog) ? ignored -> { } : catalog::accept);
    }

    /**
     * Create the opt-in data-sync driven remote catalog.
     * @param environment deployment-controlled allowlist, never Admin-controlled
     * @param tools startup-local providers
     * @param credentials optional gateway credential source
     * @return owned managed catalog
     */
    @Bean(destroyMethod = "close")
    @ConditionalOnMissingBean(AgentMcpRemoteCatalog.class)
    @ConditionalOnProperty(value = "shenyu.plugins.agent.gateway.aggregation.enabled", havingValue = "true")
    public ManagedRemoteMcpCatalog agentGatewayRemoteCatalog(final Environment environment, final ObjectProvider<AgentToolProvider> tools,
                                                            final ObjectProvider<RemoteServiceCredentialResolver> credentials) {
        String endpoints = environment.getRequiredProperty("shenyu.plugins.agent.gateway.aggregation.allowed-endpoints");
        Set<URI> allowed = Arrays.stream(endpoints.split(",")).map(String::trim).map(URI::create).collect(Collectors.toUnmodifiableSet());
        RemoteServiceCredentialResolver resolver = credentials.getIfAvailable(() -> new FileRemoteServiceCredentialResolver(
                Path.of(environment.getRequiredProperty("shenyu.plugins.agent.gateway.aggregation.credentials-directory"))));
        return new ManagedRemoteMcpCatalog(allowed, tools.orderedStream().map(AgentToolProvider::getName).collect(Collectors.toUnmodifiableSet()), resolver,
                environment.getProperty("shenyu.plugins.agent.gateway.aggregation.resources-enabled", Boolean.class, false));
    }

    private static Mono<Boolean> keyAuthenticationPresent(final org.springframework.web.server.ServerWebExchange exchange) {
        if (org.springframework.util.ClassUtils.isPresent("org.apache.shenyu.plugin.key.auth.KeyAuthAuthentication", AgentGatewayPluginConfiguration.class.getClassLoader())) {
            return org.apache.shenyu.plugin.key.auth.KeyAuthAuthentication.from(exchange).hasElement();
        }
        return Mono.just(false);
    }

    private static java.util.function.Predicate<java.util.Map<String, Object>> tokenAuthority(final PrincipalMcpSecurityProperties properties) {
        String issuer = PrincipalMcpSecurityResolver.text(properties.getIssuer());
        String audience = PrincipalMcpSecurityResolver.text(properties.getAudience());
        if (java.util.Objects.nonNull(properties.getRegistrationId())) {
            throw new IllegalArgumentException("Token source does not accept a login registration");
        }
        return attributes -> issuer.equals(attributes.get("iss"))
                && PrincipalMcpSecurityResolver.names(attributes.get("aud"), false).contains(audience);
    }

    /**
     * Explicit opt-in; authentication remains owned by the earlier key-auth plugin.
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(name = "org.apache.shenyu.plugin.key.auth.KeyAuthAuthentication")
    @ConditionalOnProperty(value = "shenyu.plugins.agent.gateway.security.key-auth.enabled", havingValue = "true")
    @EnableConfigurationProperties(KeyAuthMcpSecurityProperties.class)
    static class KeyAuthConfiguration {

        /**
         * Snapshot deployment-owned bindings, rejecting duplicate or incomplete identities.
         * @param properties explicit authorization bindings
         * @return the sole key-auth identity adapter
         */
        @Bean
        KeyAuthMcpSecurityResolver agentGatewayKeyAuthSecurityResolver(final KeyAuthMcpSecurityProperties properties) {
            return new KeyAuthMcpSecurityResolver(properties);
        }
    }

    /**
     * Reuse an application-owned JWT Resource Server without building another authenticator.
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(name = {"org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken", "org.springframework.security.oauth2.jwt.Jwt"})
    @ConditionalOnProperty(prefix = "shenyu.plugins.agent.gateway.security.principal", name = "enabled", havingValue = "true")
    @EnableConfigurationProperties(PrincipalMcpSecurityProperties.class)
    static class JwtPrincipalConfiguration {

        @Bean
        @ConditionalOnProperty(prefix = "shenyu.plugins.agent.gateway.security.principal", name = "source", havingValue = "jwt")
        AgentMcpSecurityResolver agentGatewayPrincipalJwtResolver(final PrincipalMcpSecurityProperties properties) {
            var authority = tokenAuthority(properties);
            return new PrincipalMcpSecurityResolver<>(properties, org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken.class,
                    org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken::getTokenAttributes,
                    authentication -> authority.test(authentication.getTokenAttributes()), AgentGatewayPluginConfiguration::keyAuthenticationPresent);
        }
    }

    /**
     * Reuse the application's opaque-token introspection result, not the bearer credential.
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(name = "org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthentication")
    @ConditionalOnProperty(prefix = "shenyu.plugins.agent.gateway.security.principal", name = "enabled", havingValue = "true")
    @EnableConfigurationProperties(PrincipalMcpSecurityProperties.class)
    static class OpaquePrincipalConfiguration {

        @Bean
        @ConditionalOnProperty(prefix = "shenyu.plugins.agent.gateway.security.principal", name = "source", havingValue = "opaque-token")
        AgentMcpSecurityResolver agentGatewayPrincipalOpaqueResolver(final PrincipalMcpSecurityProperties properties) {
            var authority = tokenAuthority(properties);
            return new PrincipalMcpSecurityResolver<>(properties, org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthentication.class,
                    org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthentication::getTokenAttributes,
                    authentication -> authority.test(authentication.getTokenAttributes()), AgentGatewayPluginConfiguration::keyAuthenticationPresent);
        }
    }

    /**
     * Reuse the authenticated login principal for one explicitly trusted client registration.
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(name = "org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken")
    @ConditionalOnProperty(prefix = "shenyu.plugins.agent.gateway.security.principal", name = "enabled", havingValue = "true")
    @EnableConfigurationProperties(PrincipalMcpSecurityProperties.class)
    static class LoginPrincipalConfiguration {

        @Bean
        @ConditionalOnProperty(prefix = "shenyu.plugins.agent.gateway.security.principal", name = "source", havingValue = "oauth2-login")
        AgentMcpSecurityResolver agentGatewayPrincipalLoginResolver(final PrincipalMcpSecurityProperties properties) {
            String registration = PrincipalMcpSecurityResolver.text(properties.getRegistrationId());
            if (java.util.Objects.nonNull(properties.getIssuer()) || java.util.Objects.nonNull(properties.getAudience())) {
                throw new IllegalArgumentException("Login source is bound by client registration, not token issuer/audience settings");
            }
            return new PrincipalMcpSecurityResolver<>(properties, org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken.class,
                    authentication -> authentication.getPrincipal().getAttributes(),
                    authentication -> registration.equals(authentication.getAuthorizedClientRegistrationId()), AgentGatewayPluginConfiguration::keyAuthenticationPresent);
        }
    }

    /**
     * Optional bridge for an application-owned, authenticated Resource Server JWT principal.
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(name = {"org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken",
        "org.springframework.security.oauth2.jwt.Jwt"})
    @ConditionalOnProperty(value = "shenyu.plugins.agent.gateway.security.jwt.enabled", havingValue = "true")
    static class JwtIdentityConfiguration {

        /**
         * Adapt verified claims without creating a decoder or authentication filter chain.
         * @param environment deployment-owned identity policy
         * @return the sole identity authority
         */
        @Bean(name = JWT_SECURITY_RESOLVER_BEAN_NAME)
        AgentMcpSecurityResolver agentGatewayJwtSecurityResolver(final Environment environment) {
            String prefix = "shenyu.plugins.agent.gateway.security.jwt.";
            return new JwtAgentMcpSecurityResolver(environment.getRequiredProperty(prefix + "issuer"),
                    environment.getRequiredProperty(prefix + "audience"), environment.getProperty(prefix + "default-tenant"),
                    environment.getProperty(prefix + "tenant-claim-required", Boolean.class, false));
        }
    }

    /**
     * Optional API bridge; SDK, exporters and their lifecycle belong to the application.
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(OpenTelemetry.class)
    @ConditionalOnProperty(value = "shenyu.plugins.agent.gateway.tracing.enabled", havingValue = "true")
    static class TracingConfiguration {

        /**
         * Use the explicitly supplied application telemetry, never a second SDK or global singleton.
         * @param telemetry optional application-owned telemetry (takes precedence)
         * @param gatewayTracer optional tracer owned by the ShenYu tracing starter
         * @return request-local tracing bridge
         */
        @Bean
        @ConditionalOnMissingBean(AgentMcpCallTracer.class)
        AgentMcpCallTracer agentGatewayCallTracer(final ObjectProvider<OpenTelemetry> telemetry,
                                                @org.springframework.beans.factory.annotation.Qualifier("tracingTracer") final ObjectProvider<io.opentelemetry.api.trace.Tracer> gatewayTracer) {
            OpenTelemetry application = telemetry.getIfAvailable();
            if (java.util.Objects.nonNull(application)) {
                return new OpenTelemetryAgentMcpCallTracer(application);
            }
            io.opentelemetry.api.trace.Tracer shared = gatewayTracer.getIfAvailable();
            if (java.util.Objects.isNull(shared)) {
                throw new IllegalStateException("Enabled MCP tracing requires application OpenTelemetry, tracingTracer, or a custom AgentMcpCallTracer");
            }
            return new OpenTelemetryAgentMcpCallTracer(shared);
        }
    }
}
