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
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Auto-configuration for the migrated execution path and provider profiles.
 */
class AiProxyPluginConfigurationTest {

    @Test
    void createsTheMigratedPluginAndAllExistingProviderIdentifiers() {
        new ApplicationContextRunner().withUserConfiguration(AiProxyPluginConfiguration.class).run(context -> {
            org.assertj.core.api.Assertions.assertThat(context).hasNotFailed().hasSingleBean(AiProxyPlugin.class).hasSingleBean(AiProxyEngine.class)
                    .doesNotHaveBean(AiModelFactoryRegistry.class);
            AiProxyProviderFactory factory = context.getBean(AiProxyProviderFactory.class);
            for (String provider : List.of("OpenAI", "DeepSeek", "ALiYun", "OpenAPI", "Moonshot")) {
                assertEquals(provider.toLowerCase(java.util.Locale.ROOT), factory.getProvider(provider).name().toLowerCase(java.util.Locale.ROOT));
            }
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
}
