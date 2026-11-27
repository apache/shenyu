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

package org.apache.shenyu.springboot.plugin.divide;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.canary.CanaryDecisionService;
import org.apache.shenyu.plugin.divide.canary.DefaultCanaryDecisionService;
import org.apache.shenyu.plugin.divide.context.DivideShenyuContextDecorator;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideMetaDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.context.properties.bind.Binder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.util.StringUtils;

/**
 * ShenyuConfiguration.
 */
@Configuration
@EnableConfigurationProperties(DivideCircuitBreakerProperties.class)
@ConditionalOnProperty(value = {"shenyu.plugins.divide.enabled"}, havingValue = "true", matchIfMissing = true)
public class DividePluginConfiguration {

    /**
     * init dividePlugin.
     *
     * @param canaryDecisionService request partition decision service
     * @param divideCircuitBreaker the gateway's upstream circuit breaker
     * @return {@linkplain DividePlugin}
     */
    @Bean
    public ShenyuPlugin dividePlugin(final CanaryDecisionService canaryDecisionService, final UpstreamCircuitBreaker divideCircuitBreaker) {
        return new DividePlugin(canaryDecisionService, divideCircuitBreaker);
    }

    /**
     * Create one breaker for the lifetime of this gateway context.
     *
     * @param properties the bound startup configuration
     * @param environment the gateway environment
     * @return the shared upstream circuit breaker
     */
    @Bean
    public UpstreamCircuitBreaker divideCircuitBreaker(final DivideCircuitBreakerProperties properties, final Environment environment) {
        // The duration binder treats an empty string as an absent value.
        Binder.get(environment).bind(DivideCircuitBreakerProperties.HALF_OPEN_WAIT_PROPERTY, String.class).ifBound(value -> {
            if (!StringUtils.hasText(value)) {
                throw new IllegalArgumentException(DivideCircuitBreakerProperties.HALF_OPEN_WAIT_PROPERTY + " must be a duration");
            }
        });
        return UpstreamCircuitBreaker.create(properties.getFailureThreshold(), properties.halfOpenWaitMillis());
    }

    /**
     * Default request partition decision service.
     *
     * @return Canary decision service
     */
    @Bean
    @ConditionalOnMissingBean(CanaryDecisionService.class)
    public CanaryDecisionService canaryDecisionService() {
        return new DefaultCanaryDecisionService();
    }

    /**
     * Divide plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler dividePluginDataHandler() {
        return new DividePluginDataHandler();
    }

    /**
     * divide meta data handler.
     *
     * @return the meta data handler
     */
    @Bean
    public MetaDataHandler divideMetaDataHandler() {
        return new DivideMetaDataHandler();
    }


    /**
     * divide discovery upstream data handler.
     *
     * @return the discovery upstream data handler
     */
    @Bean
    public DivideUpstreamDataHandler divideUpstreamDataHandler() {
        return new DivideUpstreamDataHandler();
    }

    /**
     * Divide shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator divideShenyuContextDecorator() {
        return new DivideShenyuContextDecorator();
    }
}
