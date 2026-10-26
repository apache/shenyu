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

package org.apache.shenyu.springboot.starter.sync.data.file;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.springboot.starter.gateway.ShenyuConfiguration;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.WebFilter;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Opt-in, single-source, startup-only declarative gateway configuration.
 */
@Configuration(proxyBeanMethods = false)
@AutoConfigureAfter(ShenyuConfiguration.class)
@ConditionalOnProperty(name = FileModeEnvironmentPostProcessor.ENABLED, havingValue = "true")
public class FileSyncDataConfiguration {

    /**
     * Create the loader; application completes before the web server lifecycle starts.
     * @param environment runtime properties and placeholder resolver
     * @param config gateway runtime configuration
     * @param plugins available plugins
     * @param handlers available configuration handlers
     * @param upstreamHandlers available upstream handlers
     * @param publisher existing plugin event publisher
     * @param upstreamSubscriber existing upstream subscriber
     * @return startup loader
     */
    @Bean
    public FileConfigurationLoader fileConfigurationLoader(final ConfigurableEnvironment environment, final ShenyuConfig config,
                                                           final List<ShenyuPlugin> plugins, final List<PluginDataHandler> handlers,
                                                           final List<DiscoveryUpstreamDataHandler> upstreamHandlers,
                                                           final ApplicationEventPublisher publisher, final DiscoveryUpstreamDataSubscriber upstreamSubscriber) {
        if (!environment.getPropertySources().contains("shenyuFileMode")) {
            throw new IllegalStateException("File mode requires the Boot environment post-processor before context creation");
        }
        // Undeclared plugins must not initialize remote clients while being disabled.
        List<PluginDataHandler> supportedHandlers = handlers.stream().filter(handler -> Set.of("divide", "contextPath").contains(handler.pluginNamed())).collect(Collectors.toList());
        CommonPluginDataSubscriber subscriber = new CommonPluginDataSubscriber(supportedHandlers, publisher, config.getSelectorMatchCache(), config.getRuleMatchCache());
        return new FileConfigurationLoader(environment, config.getNamespace(), plugins, handlers, upstreamHandlers, subscriber, upstreamSubscriber);
    }

    /**
     * Refuse traffic until the complete snapshot has been applied.
     * @param loader file loader
     * @return startup traffic gate
     */
    @Bean
    @Order(Ordered.HIGHEST_PRECEDENCE)
    public WebFilter fileConfigurationReadinessFilter(final FileConfigurationLoader loader) {
        return (exchange, chain) -> {
            if (!loader.isReady()) {
                exchange.getResponse().setStatusCode(HttpStatus.SERVICE_UNAVAILABLE);
                return exchange.getResponse().setComplete();
            }
            return chain.filter(exchange);
        };
    }
}
