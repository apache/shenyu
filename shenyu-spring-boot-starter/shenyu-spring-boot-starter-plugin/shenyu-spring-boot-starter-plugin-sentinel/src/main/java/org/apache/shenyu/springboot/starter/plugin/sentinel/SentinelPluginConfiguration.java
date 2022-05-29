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

package org.apache.shenyu.springboot.starter.plugin.sentinel;

import com.alibaba.csp.sentinel.adapter.spring.webflux.exception.SentinelBlockExceptionHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.fallback.FallbackHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.sentinel.SentinelPlugin;
import org.apache.shenyu.plugin.sentinel.fallback.SentinelFallbackHandler;
import org.apache.shenyu.plugin.sentinel.handler.SentinelRuleHandle;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.web.reactive.result.view.ViewResolver;

import java.util.Collections;
import java.util.List;

/**
 * Sentinel plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.sentinel.enabled"}, havingValue = "true", matchIfMissing = true)
public class SentinelPluginConfiguration {

    /**
     * Sentinel plugin viewResolvers.
     */
    private final List<ViewResolver> viewResolvers;

    /**
     * Sentinel plugin serverCodecConfigurer.
     */
    private final ServerCodecConfigurer serverCodecConfigurer;

    /**
     * sentinelPluginConfiguration constructor.
     *
     * @param listObjectProvider    the list object provider
     * @param serverCodecConfigurer the server codec configurer
     */
    public SentinelPluginConfiguration(final ObjectProvider<List<ViewResolver>> listObjectProvider, final ServerCodecConfigurer serverCodecConfigurer) {
        this.viewResolvers = listObjectProvider.getIfAvailable(Collections::emptyList);
        this.serverCodecConfigurer = serverCodecConfigurer;
    }

    /**
     * Sentinel plugin.
     *
     * @param fallbackHandler the fallback handler
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin sentinelPlugin(final FallbackHandler fallbackHandler) {
        return new SentinelPlugin(fallbackHandler);
    }

    /**
     * Fallback handler.
     *
     * @return the default fallback handler
     */
    @Bean
    @ConditionalOnMissingBean(FallbackHandler.class)
    public FallbackHandler fallbackHandler() {
        return new SentinelFallbackHandler();
    }

    /**
     * Sentinel plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler sentinelRuleHandle() {
        return new SentinelRuleHandle();
    }

    /**
     * Sentinel exception handler.
     *
     * @return the shenyu plugin
     */
    @Bean
    @Order(Ordered.HIGHEST_PRECEDENCE)
    public SentinelBlockExceptionHandler sentinelBlockExceptionHandler() {
        return new SentinelBlockExceptionHandler(viewResolvers, serverCodecConfigurer);
    }
}
