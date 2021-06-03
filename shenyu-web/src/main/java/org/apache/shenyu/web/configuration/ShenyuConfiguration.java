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

package org.apache.shenyu.web.configuration;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.web.configuration.properties.ShenyuConfig;
import org.apache.shenyu.web.configuration.properties.ExcludePathProperties;
import org.apache.shenyu.web.filter.CrossFilter;
import org.apache.shenyu.web.filter.ExcludeFilter;
import org.apache.shenyu.web.filter.FileSizeFilter;
import org.apache.shenyu.web.filter.TimeWebFilter;
import org.apache.shenyu.web.filter.WebSocketParamFilter;
import org.apache.shenyu.web.forward.ForwardedRemoteAddressResolver;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.ParamTransformPlugin;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.Order;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.WebFilter;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * ShenyuConfiguration.
 */
@Configuration
@ComponentScan("org.apache.shenyu")
@Import(value = {ErrorHandlerConfiguration.class, ShenyuExtConfiguration.class, SpringExtConfiguration.class})
@Slf4j
public class ShenyuConfiguration {

    /**
     * Init ShenyuWebHandler.
     *
     * @param plugins this plugins is All impl ShenyuPlugin.
     * @return {@linkplain ShenyuWebHandler}
     */
    @Bean("webHandler")
    public ShenyuWebHandler shenyuWebHandler(final ObjectProvider<List<ShenyuPlugin>> plugins) {
        List<ShenyuPlugin> pluginList = plugins.getIfAvailable(Collections::emptyList);
        List<ShenyuPlugin> shenyuPlugins = pluginList.stream()
                .sorted(Comparator.comparingInt(ShenyuPlugin::getOrder)).collect(Collectors.toList());
        shenyuPlugins.forEach(shenyuPlugin -> log.info("load plugin:[{}] [{}]", shenyuPlugin.named(), shenyuPlugin.getClass().getName()));
        return new ShenyuWebHandler(shenyuPlugins);
    }

    /**
     * init dispatch handler.
     *
     * @return {@link DispatcherHandler}.
     */
    @Bean("dispatcherHandler")
    public DispatcherHandler dispatcherHandler() {
        return new DispatcherHandler();
    }

    /**
     * Param transform plugin shenyu plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin paramTransformPlugin() {
        return new ParamTransformPlugin();
    }

    /**
     * Plugin data subscriber plugin data subscriber.
     *
     * @param pluginDataHandlerList the plugin data handler list
     * @return the plugin data subscriber
     */
    @Bean
    public PluginDataSubscriber pluginDataSubscriber(final ObjectProvider<List<PluginDataHandler>> pluginDataHandlerList) {
        return new CommonPluginDataSubscriber(pluginDataHandlerList.getIfAvailable(Collections::emptyList));
    }

    /**
     * Remote address resolver remote address resolver.
     *
     * @return the remote address resolver
     */
    @Bean
    @ConditionalOnMissingBean(RemoteAddressResolver.class)
    public RemoteAddressResolver remoteAddressResolver() {
        return new ForwardedRemoteAddressResolver(1);
    }

    /**
     * Cross filter web filter.
     * if you application has cross-domain.
     * this is demo.
     * 1. Customize webflux's cross-domain requests.
     * 2. Spring bean Sort is greater than -1.
     *
     * @return the web filter
     */
    @Bean
    @Order(-100)
    @ConditionalOnProperty(name = "shenyu.cross.enabled", havingValue = "true")
    public WebFilter crossFilter() {
        return new CrossFilter();
    }

    /**
     * Body web filter web filter.
     *
     * @param shenyuConfig the shenyu config
     * @return the web filter
     */
    @Bean
    @Order(-10)
    @ConditionalOnProperty(name = "shenyu.file.enabled", havingValue = "true")
    public WebFilter fileSizeFilter(final ShenyuConfig shenyuConfig) {
        return new FileSizeFilter(shenyuConfig.getFileMaxSize());
    }

    /**
     * Rule out the url Filter.
     *
     * @param excludePathProperties the exclude path
     * @return the web filter
     */
    @Bean
    @Order(-5)
    @ConditionalOnProperty(name = "shenyu.exclude.enabled", havingValue = "true")
    public WebFilter excludeFilter(final ExcludePathProperties excludePathProperties) {
        return new ExcludeFilter(excludePathProperties);
    }

    /**
     * shenyu config.
     *
     * @return the shenyu config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu")
    public ShenyuConfig shenyuConfig() {
        return new ShenyuConfig();
    }

    /**
     * Init time web filter.
     *
     * @param shenyuConfig the shenyu config
     * @return {@linkplain TimeWebFilter}
     */
    @Bean
    @Order(30)
    @ConditionalOnProperty(name = "shenyu.filterTimeEnable")
    public WebFilter timeWebFilter(final ShenyuConfig shenyuConfig) {
        return new TimeWebFilter(shenyuConfig);
    }

    /**
     * Web socket web filter web filter.
     *
     * @return the web filter
     */
    @Bean
    @Order(4)
    public WebFilter webSocketWebFilter() {
        return new WebSocketParamFilter();
    }
}
