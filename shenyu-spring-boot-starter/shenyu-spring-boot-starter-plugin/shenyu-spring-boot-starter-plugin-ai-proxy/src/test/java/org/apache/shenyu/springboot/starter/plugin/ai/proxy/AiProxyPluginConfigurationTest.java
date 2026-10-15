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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiProviderOptions;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
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
                    .doesNotHaveBean(AiModelFactoryRegistry.class);
            AiProxyProviderFactory factory = context.getBean(AiProxyProviderFactory.class);
            assertSame(context.getBean(OpenAiResponses.class), context.getBean(AiProxyProtocolFactory.class).getProtocol(OpenAiResponses.NAME));
            for (String provider : List.of("OpenAI", "DeepSeek", "ALiYun", "OpenAPI", "Moonshot", "OpenAICompatible")) {
                assertEquals(provider.toLowerCase(java.util.Locale.ROOT), factory.getProvider(provider).name().toLowerCase(java.util.Locale.ROOT));
                org.junit.jupiter.api.Assertions.assertTrue(factory.getProvider(provider).supports(OpenAiResponses.NAME));
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
            org.assertj.core.api.Assertions.assertThat(context).hasNotFailed().hasSingleBean(AiProxyTransport.class);
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
                        "shenyu.plugins.ai.proxy.transport.max-response-bytes=1024")
                .run(context -> {
                    AiProxyTransportProperties properties = context.getBean(AiProxyTransportProperties.class);
                    assertEquals(Duration.ofSeconds(5), properties.getConnectTimeout());
                    assertEquals(Duration.ofSeconds(7), properties.getResponseHeaderTimeout());
                    assertEquals(Duration.ofSeconds(11), properties.getStreamingIdleTimeout());
                    assertEquals(Duration.ofMinutes(12), properties.getTotalStreamDuration());
                    assertEquals(1024, properties.getMaxResponseBytes());
                });
    }

    @Test
    void shouldConvertConnectTimeoutToNettyMilliseconds() {
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setConnectTimeout(Duration.ofSeconds(5));

        final int timeout = new AiProxyPluginConfiguration()
                .connectTimeoutMillis(properties);

        assertEquals(5000, timeout);
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
}
