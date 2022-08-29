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

package org.apache.shenyu.springboot.starter.gateway;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.RpcParamTransformPlugin;
import org.apache.shenyu.plugin.base.cache.CommonMetaDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.web.configuration.ErrorHandlerConfiguration;
import org.apache.shenyu.web.configuration.ShenyuExtConfiguration;
import org.apache.shenyu.web.configuration.SpringExtConfiguration;
import org.apache.shenyu.web.filter.CrossFilter;
import org.apache.shenyu.web.filter.ExcludeFilter;
import org.apache.shenyu.web.filter.FallbackFilter;
import org.apache.shenyu.web.filter.FileSizeFilter;
import org.apache.shenyu.web.filter.HealthFilter;
import org.apache.shenyu.web.filter.LocalDispatcherFilter;
import org.apache.shenyu.web.forward.ForwardedRemoteAddressResolver;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.ApplicationEventPublisher;
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
@AutoConfigureBefore(value = SpringExtConfiguration.class)
@Import(value = ErrorHandlerConfiguration.class)
@AutoConfigureAfter(value = ShenyuExtConfiguration.class)
public class ShenyuConfiguration {
    
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuConfiguration.class);
    
    /**
     * Init ShenyuWebHandler.
     *
     * @param plugins this plugins is All impl ShenyuPlugin.
     * @param config the config
     * @return {@linkplain ShenyuWebHandler}
     */
    @Bean("webHandler")
    public ShenyuWebHandler shenyuWebHandler(final ObjectProvider<List<ShenyuPlugin>> plugins, final ShenyuConfig config) {
        List<ShenyuPlugin> pluginList = plugins.getIfAvailable(Collections::emptyList);
        List<ShenyuPlugin> shenyuPlugins = pluginList.stream()
                .sorted(Comparator.comparingInt(ShenyuPlugin::getOrder)).collect(Collectors.toList());
        shenyuPlugins.forEach(shenyuPlugin -> LOG.info("load plugin:[{}] [{}]", shenyuPlugin.named(), shenyuPlugin.getClass().getName()));
        return new ShenyuWebHandler(shenyuPlugins, config);
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
     * Param transform plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin paramTransformPlugin() {
        return new RpcParamTransformPlugin();
    }
    
    /**
     * common plugin data subscriber.
     *
     * @param pluginDataHandlerList the plugin data handler list
     * @param eventPublisher event publisher
     * @return the plugin data subscriber
     */
    @Bean
    public PluginDataSubscriber pluginDataSubscriber(final ObjectProvider<List<PluginDataHandler>> pluginDataHandlerList,
                                                     final ApplicationEventPublisher eventPublisher) {
        return new CommonPluginDataSubscriber(pluginDataHandlerList.getIfAvailable(Collections::emptyList), eventPublisher);
    }

    /**
     * common meta data subscriber.
     *
     * @param metaDataHandlerList the meta data handler list
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber commonMetaDataSubscriber(final ObjectProvider<List<MetaDataHandler>> metaDataHandlerList) {
        return new CommonMetaDataSubscriber(metaDataHandlerList.getIfAvailable(Collections::emptyList));
    }
    
    /**
     * Shenyu loader service.
     *
     * @param shenyuWebHandler the shenyu web handler
     * @param pluginDataSubscriber the plugin data subscriber
     * @param config the config
     * @return the shenyu loader service
     */
    @Bean
    public ShenyuLoaderService shenyuLoaderService(final ShenyuWebHandler shenyuWebHandler,
                                                   final PluginDataSubscriber pluginDataSubscriber,
                                                   final ShenyuConfig config) {
        return new ShenyuLoaderService(shenyuWebHandler, (CommonPluginDataSubscriber) pluginDataSubscriber, config);
    }
    
    /**
     * Remote address resolver.
     *
     * @return the remote address resolver
     */
    @Bean
    @ConditionalOnMissingBean(RemoteAddressResolver.class)
    public RemoteAddressResolver remoteAddressResolver() {
        return new ForwardedRemoteAddressResolver(1);
    }
    
    /**
     * Local dispatcher filter.
     *
     * @param dispatcherHandler the dispatcher handler
     * @param shenyuConfig the shenyuConfig
     * @return the web filter
     */
    @Bean
    @Order(-200)
    @ConditionalOnProperty(name = "shenyu.local.enabled", havingValue = "true", matchIfMissing = true)
    public WebFilter localDispatcherFilter(final DispatcherHandler dispatcherHandler, final ShenyuConfig shenyuConfig) {
        return new LocalDispatcherFilter(dispatcherHandler, shenyuConfig.getLocal().getSha512Key());
    }
    
    /**
     * Cross filter.
     * if you application has cross-domain.
     * this is demo.
     * 1. Customize webflux's cross-domain requests.
     * 2. Spring bean Sort is greater than -1.
     *
     * @param shenyuConfig the shenyu config
     * @return the web filter
     */
    @Bean
    @Order(-100)
    @ConditionalOnProperty(name = "shenyu.cross.enabled", havingValue = "true")
    public WebFilter crossFilter(final ShenyuConfig shenyuConfig) {
        return new CrossFilter(shenyuConfig.getCross());
    }
    
    /**
     * Body web filter.
     *
     * @param shenyuConfig the shenyu config
     * @return the web filter
     */
    @Bean
    @Order(-10)
    @ConditionalOnProperty(name = "shenyu.file.enabled", havingValue = "true")
    public WebFilter fileSizeFilter(final ShenyuConfig shenyuConfig) {
        return new FileSizeFilter(shenyuConfig.getFile().getMaxSize());
    }
    
    /**
     * Exclude filter.
     *
     * @param shenyuConfig the shenyu config
     * @return the web filter
     */
    @Bean
    @Order(-5)
    @ConditionalOnProperty(name = "shenyu.exclude.enabled", havingValue = "true")
    public WebFilter excludeFilter(final ShenyuConfig shenyuConfig) {
        return new ExcludeFilter(shenyuConfig.getExclude().getPaths());
    }
    
    /**
     * fallback filter.
     *
     * @param shenyuConfig the shenyu config
     * @param dispatcherHandler the dispatcher handler
     * @return the fallback web filter
     */
    @Bean
    @Order(-5)
    @ConditionalOnProperty(name = "shenyu.fallback.enabled", havingValue = "true")
    public WebFilter fallbackFilter(final ShenyuConfig shenyuConfig, final DispatcherHandler dispatcherHandler) {
        return new FallbackFilter(shenyuConfig.getFallback().getPaths(), dispatcherHandler);
    }
    
    /**
     * Health filter.
     *
     * @param shenyuConfig the shenyu config
     * @return the web filter
     */
    @Bean
    @Order(-99)
    @ConditionalOnProperty(name = "shenyu.health.enabled", havingValue = "true")
    public WebFilter healthFilter(final ShenyuConfig shenyuConfig) {
        return new HealthFilter(shenyuConfig.getHealth().getPaths());
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
}
