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
import org.apache.shenyu.web.config.SoulConfig;
import org.apache.shenyu.web.filter.CrossFilter;
import org.apache.shenyu.web.filter.ExcludeFilter;
import org.apache.shenyu.web.filter.FileSizeFilter;
import org.apache.shenyu.web.filter.TimeWebFilter;
import org.apache.shenyu.web.filter.WebSocketParamFilter;
import org.apache.shenyu.web.forward.ForwardedRemoteAddressResolver;
import org.apache.shenyu.web.handler.SoulWebHandler;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.SoulPlugin;
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
 * SoulConfiguration.
 *
 * @author xiaoyu(Myth)
 * @author dengliming
 */
@Configuration
@ComponentScan("org.apache.shenyu")
@Import(value = {ErrorHandlerConfiguration.class, SoulExtConfiguration.class, SpringExtConfiguration.class})
@Slf4j
public class SoulConfiguration {
    
    /**
     * Init SoulWebHandler.
     *
     * @param plugins this plugins is All impl SoulPlugin.
     * @return {@linkplain SoulWebHandler}
     */
    @Bean("webHandler")
    public SoulWebHandler soulWebHandler(final ObjectProvider<List<SoulPlugin>> plugins) {
        List<SoulPlugin> pluginList = plugins.getIfAvailable(Collections::emptyList);
        List<SoulPlugin> soulPlugins = pluginList.stream()
                .sorted(Comparator.comparingInt(SoulPlugin::getOrder)).collect(Collectors.toList());
        soulPlugins.forEach(soulPlugin -> log.info("load plugin:[{}] [{}]", soulPlugin.named(), soulPlugin.getClass().getName()));
        return new SoulWebHandler(soulPlugins);
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
     * Param transform plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin paramTransformPlugin() {
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
    @ConditionalOnProperty(name = "soul.cross.enabled", havingValue = "true")
    public WebFilter crossFilter() {
        return new CrossFilter();
    }
    
    /**
     * Body web filter web filter.
     *
     * @param soulConfig the soul config
     * @return the web filter
     */
    @Bean
    @Order(-10)
    @ConditionalOnProperty(name = "soul.file.enabled", havingValue = "true")
    public WebFilter fileSizeFilter(final SoulConfig soulConfig) {
        return new FileSizeFilter(soulConfig.getFileMaxSize());
    }
    
    /**
     * Rule out the url Filter.
     *
     * @param excludePathProperties the exclude path
     * @return the web filter
     */
    @Bean
    @Order(-5)
    @ConditionalOnProperty(name = "soul.exclude.enabled", havingValue = "true")
    public WebFilter excludeFilter(final ExcludePathProperties excludePathProperties) {
        return new ExcludeFilter(excludePathProperties);
    }
    
    /**
     * Soul config soul config.
     *
     * @return the soul config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul")
    public SoulConfig soulConfig() {
        return new SoulConfig();
    }
    
    /**
     * Init time web filter.
     *
     * @param soulConfig the soul config
     * @return {@linkplain TimeWebFilter}
     */
    @Bean
    @Order(30)
    @ConditionalOnProperty(name = "soul.filterTimeEnable")
    public WebFilter timeWebFilter(final SoulConfig soulConfig) {
        return new TimeWebFilter(soulConfig);
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
