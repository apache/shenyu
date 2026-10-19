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

package org.apache.shenyu.springboot.starter.plugin.ai.proxy;

import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiProviderOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.ConnectResponsesToChatConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransportProperties;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Auto-configuration for the migrated execution path, provider profiles and transport properties.
 */
class AiProxyPluginConfigurationTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(
                    ConfigurationPropertiesAutoConfiguration.class,
                    AiProxyPluginConfiguration.class));

    @Test
    void createsTheMigratedPluginAndAllExistingProviderIdentifiers() {
        new ApplicationContextRunner().withUserConfiguration(AiProxyPluginConfiguration.class).run(context -> {
            org.assertj.core.api.Assertions.assertThat(context).hasNotFailed().hasSingleBean(AiProxyPlugin.class).hasSingleBean(AiProxyEngine.class)
                    .hasSingleBean(AiProtocolConverterFactory.class).hasSingleBean(ConnectResponsesToChatConverter.class)
                    .hasSingleBean(AiProtocolRouteResolver.class)
                    .doesNotHaveBean(AiModelFactoryRegistry.class);
            AiProxyProviderFactory factory = context.getBean(AiProxyProviderFactory.class);
            org.junit.jupiter.api.Assertions.assertTrue(context.getBean(AiProtocolConverterFactory.class)
                    .supports(OpenAiResponses.NAME, OpenAiChat.NAME));
            assertSame(context.getBean(OpenAiResponses.class), context.getBean(AiProxyProtocolFactory.class).getProtocol(OpenAiResponses.NAME));
            for (String provider : List.of("OpenAI", "DeepSeek", "ALiYun", "OpenAPI", "Moonshot", "OpenAICompatible")) {
                assertEquals(provider.toLowerCase(java.util.Locale.ROOT), factory.getProvider(provider).name().toLowerCase(java.util.Locale.ROOT));
                org.junit.jupiter.api.Assertions.assertTrue(factory.getProvider(provider).supports(OpenAiChat.NAME));
                org.junit.jupiter.api.Assertions.assertFalse(factory.getProvider(provider).supports(OpenAiResponses.NAME));
            }
        });
    }

    @Test
    void discoversAnotherCompatibleProviderWithoutEngineProtocolOrTransportChanges() {
        OpenAiCompatible provider = new OpenAiCompatible("tenant-profile", OpenAiProviderOptions.generic(null));
        new ApplicationContextRunner().withUserConfiguration(AiProxyPluginConfiguration.class)
                .withBean("tenantProvider", AiProxyProvider.class, () -> provider).run(context -> {
                    org.assertj.core.api.Assertions.assertThat(context).hasNotFailed();
                    assertSame(provider, context.getBean(AiProxyProviderFactory.class).getProvider("TENANT_PROFILE"));
                    AiCommonConfig config = new AiCommonConfig();
                    config.setProvider("tenant-profile");
                    config.setBaseUrl("https://example.org");
                    config.setApiKey("key");
                    AiUpstreamRequest request = context.getBean(AiProxyEngine.class).buildRequest("{\"messages\":[{\"role\":\"user\"}]}", config);
                    assertEquals("https://example.org/v1/chat/completions", request.getUri().toString());
                });
    }

    @Test
    void allowsACustomTransportAndHonorsTheEnableFlag() {
        AiProxyTransport custom = request -> Mono.empty();
        new ApplicationContextRunner().withUserConfiguration(AiProxyPluginConfiguration.class).withBean(AiProxyTransport.class, () -> custom).run(context -> {
            org.assertj.core.api.Assertions.assertThat(context)
                    .hasNotFailed()
                    .hasSingleBean(AiProxyTransport.class)
                    .doesNotHaveBean(AiProxyHttpClientResources.class);
            assertSame(custom, context.getBean(AiProxyTransport.class));
        });
        new ApplicationContextRunner().withUserConfiguration(AiProxyPluginConfiguration.class).withPropertyValues("shenyu.plugins.ai.proxy.enabled=false")
                .run(context -> org.assertj.core.api.Assertions.assertThat(context).doesNotHaveBean(AiProxyPlugin.class).doesNotHaveBean(AiProxyEngine.class));
    }

    @Test
    void shouldUseDefaultConnectTimeout() {
        contextRunner.run(context -> {
            AiProxyTransportProperties properties = context.getBean(AiProxyTransportProperties.class);
            assertEquals(Duration.ofSeconds(30), properties.getConnectTimeout());
            assertEquals(Duration.ofSeconds(30), properties.getResponseHeaderTimeout());
            assertEquals(Duration.ofSeconds(60), properties.getStreamingIdleTimeout());
            assertEquals(Duration.ofMinutes(10), properties.getTotalStreamDuration());
            assertEquals(0, properties.getMaxResponseBytes());
            assertEquals(30, properties.getPool().getMaxConnections());
            assertEquals(60, properties.getPool().getPendingAcquireMaxCount());
            assertEquals(Duration.ofSeconds(3), properties.getPool().getPendingAcquireTimeout());
            assertEquals(Duration.ofSeconds(60), properties.getPool().getMaxIdleTime());
            assertEquals(Duration.ofMinutes(30), properties.getPool().getMaxLifeTime());
            assertEquals(Duration.ofSeconds(30), properties.getPool().getEvictionInterval());
            assertEquals(Duration.ofSeconds(30), properties.getPool().getGracefulDisposeTimeout());
        });
    }

    @Test
    void shouldBindConfiguredConnectTimeout() {
        contextRunner
                .withPropertyValues(
                        "shenyu.plugins.ai.proxy.transport.connect-timeout=5s",
                        "shenyu.plugins.ai.proxy.transport.response-header-timeout=7s",
                        "shenyu.plugins.ai.proxy.transport.streaming-idle-timeout=11s",
                        "shenyu.plugins.ai.proxy.transport.total-stream-duration=12m",
                        "shenyu.plugins.ai.proxy.transport.max-response-bytes=1024",
                        "shenyu.plugins.ai.proxy.transport.pool.enabled=false",
                        "shenyu.plugins.ai.proxy.transport.pool.name=test-ai-pool",
                        "shenyu.plugins.ai.proxy.transport.pool.max-connections=17",
                        "shenyu.plugins.ai.proxy.transport.pool.pending-acquire-max-count=23",
                        "shenyu.plugins.ai.proxy.transport.pool.pending-acquire-timeout=2s",
                        "shenyu.plugins.ai.proxy.transport.pool.max-idle-time=45s",
                        "shenyu.plugins.ai.proxy.transport.pool.max-life-time=20m",
                        "shenyu.plugins.ai.proxy.transport.pool.eviction-interval=15s",
                        "shenyu.plugins.ai.proxy.transport.pool.graceful-dispose-timeout=9s",
                        "shenyu.plugins.ai.proxy.transport.tls.verify=false",
                        "shenyu.plugins.ai.proxy.transport.proxy.enabled=true",
                        "shenyu.plugins.ai.proxy.transport.proxy.host=proxy.example.com",
                        "shenyu.plugins.ai.proxy.transport.proxy.port=8080",
                        "shenyu.plugins.ai.proxy.transport.proxy.username=user",
                        "shenyu.plugins.ai.proxy.transport.proxy.password=secret",
                        "shenyu.plugins.ai.proxy.transport.proxy.non-proxy-hosts-pattern=localhost|127\\..*")
                .run(context -> {
                    AiProxyTransportProperties properties = context.getBean(AiProxyTransportProperties.class);
                    assertEquals(Duration.ofSeconds(5), properties.getConnectTimeout());
                    assertEquals(Duration.ofSeconds(7), properties.getResponseHeaderTimeout());
                    assertEquals(Duration.ofSeconds(11), properties.getStreamingIdleTimeout());
                    assertEquals(Duration.ofMinutes(12), properties.getTotalStreamDuration());
                    assertEquals(1024, properties.getMaxResponseBytes());
                    assertFalse(properties.getPool().isEnabled());
                    assertEquals("test-ai-pool", properties.getPool().getName());
                    assertEquals(17, properties.getPool().getMaxConnections());
                    assertEquals(23, properties.getPool().getPendingAcquireMaxCount());
                    assertEquals(Duration.ofSeconds(2), properties.getPool().getPendingAcquireTimeout());
                    assertEquals(Duration.ofSeconds(45), properties.getPool().getMaxIdleTime());
                    assertEquals(Duration.ofMinutes(20), properties.getPool().getMaxLifeTime());
                    assertEquals(Duration.ofSeconds(15), properties.getPool().getEvictionInterval());
                    assertEquals(Duration.ofSeconds(9), properties.getPool().getGracefulDisposeTimeout());
                    assertFalse(properties.getTls().isVerify());
                    assertEquals("proxy.example.com", properties.getProxy().getHost());
                    assertEquals(8080, properties.getProxy().getPort());
                    assertEquals("user", properties.getProxy().getUsername());
                    assertEquals("secret", properties.getProxy().getPassword());
                    assertEquals("localhost|127\\..*", properties.getProxy().getNonProxyHostsPattern());
                });
    }

    @Test
    void shouldUseDefaultRetryProperties() {
        contextRunner.run(context -> {
            final AiProxyRetryProperties properties = context.getBean(AiProxyRetryProperties.class);
            assertEquals(3, properties.getMaxRetries());
            assertEquals(1, properties.getStreamingMaxRetries());
            assertEquals(Duration.ofSeconds(1), properties.getInitialBackoff());
        });
    }

    @Test
    void shouldBindConfiguredRetryProperties() {
        contextRunner
                .withPropertyValues(
                        "shenyu.plugins.ai.proxy.retry.max-retries=2",
                        "shenyu.plugins.ai.proxy.retry.streaming-max-retries=0",
                        "shenyu.plugins.ai.proxy.retry.initial-backoff=500ms")
                .run(context -> {
                    final AiProxyRetryProperties properties = context.getBean(AiProxyRetryProperties.class);
                    assertEquals(2, properties.getMaxRetries());
                    assertEquals(0, properties.getStreamingMaxRetries());
                    assertEquals(Duration.ofMillis(500), properties.getInitialBackoff());
                });
    }

    @Test
    void shouldRejectInvalidRetryProperties() {
        for (String property : List.of(
                "max-retries=-1",
                "max-retries=31",
                "streaming-max-retries=-1",
                "streaming-max-retries=31",
                "initial-backoff=-1s")) {
            contextRunner
                    .withPropertyValues("shenyu.plugins.ai.proxy.retry." + property)
                    .run(context -> org.assertj.core.api.Assertions.assertThat(context).hasFailed());
        }
    }

    @Test
    void shouldRejectNonPositiveTransportTimeouts() {
        for (String property : List.of(
                "connect-timeout",
                "response-header-timeout",
                "streaming-idle-timeout",
                "total-stream-duration")) {
            contextRunner
                    .withPropertyValues("shenyu.plugins.ai.proxy.transport." + property + "=0s")
                    .run(context -> org.assertj.core.api.Assertions.assertThat(context).hasFailed());
        }
    }

    @Test
    void shouldRejectNegativeMaximumResponseBytes() {
        contextRunner
                .withPropertyValues("shenyu.plugins.ai.proxy.transport.max-response-bytes=-1")
                .run(context -> org.assertj.core.api.Assertions.assertThat(context).hasFailed());
    }

    @Test
    void resolvesPluginUpdatesFromTheAutoConfiguredHandler() {
        contextRunner.run(context -> {
            PluginData plugin = new PluginData();
            plugin.setConfig("{\"provider\":\"OpenAI\",\"baseUrl\":\"https://example.org\",\"apiKey\":\"global-key\",\"model\":\"global-model\"}");
            AiProxyPluginHandler handler = context.getBean(AiProxyPluginHandler.class);
            handler.handlerPlugin(plugin);
            AiCommonConfig resolved = context.getBean(AiProxyConfigService.class).resolvePrimaryConfig(new AiProxyHandle());
            assertEquals("global-model", resolved.getModel());
            assertEquals("global-key", resolved.getApiKey());
            assertEquals("https://example.org", resolved.getBaseUrl());
            handler.removePlugin(plugin);
            org.junit.jupiter.api.Assertions.assertNull(context.getBean(AiProxyConfigService.class).resolvePrimaryConfig(null).getModel());
        });
    }

    @Test
    void shouldRejectInvalidConnectionResourceProperties() {
        for (String property : List.of(
                "pool.max-connections=0",
                "pool.pending-acquire-max-count=0",
                "pool.pending-acquire-timeout=0s",
                "pool.max-idle-time=0s",
                "pool.max-life-time=0s",
                "pool.eviction-interval=0s",
                "pool.graceful-dispose-timeout=0s")) {
            contextRunner
                    .withPropertyValues("shenyu.plugins.ai.proxy.transport." + property)
                    .run(context -> org.assertj.core.api.Assertions.assertThat(context).hasFailed());
        }
    }

    @Test
    void shouldRejectIncompleteProxyConfiguration() {
        contextRunner
                .withPropertyValues("shenyu.plugins.ai.proxy.transport.proxy.enabled=true")
                .run(context -> org.assertj.core.api.Assertions.assertThat(context).hasFailed());
    }
}
