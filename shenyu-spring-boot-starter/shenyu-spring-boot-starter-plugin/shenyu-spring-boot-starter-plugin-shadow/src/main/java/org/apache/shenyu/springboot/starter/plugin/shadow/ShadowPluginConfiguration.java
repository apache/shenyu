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

package org.apache.shenyu.springboot.starter.plugin.shadow;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.shadow.ShadowPlugin;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntimeRegistry;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowWebClientFactory;
import org.apache.shenyu.plugin.shadow.handler.ShadowPluginDataHandler;
import org.apache.shenyu.plugin.shadow.report.LoggingShadowReporter;
import org.apache.shenyu.plugin.shadow.report.ShadowReporter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type shadow plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.shadow.enabled"}, havingValue = "true", matchIfMissing = true)
public class ShadowPluginConfiguration {

    /**
     * Shadow reporter.
     *
     * @return the shadow reporter
     */
    @Bean
    public ShadowReporter shadowReporter() {
        return new LoggingShadowReporter();
    }

    /**
     * Shadow runtime registry.
     *
     * @param reporter the shadow reporter
     * @return the shadow runtime registry
     */
    @Bean(destroyMethod = "close")
    public ShadowRuntimeRegistry shadowRuntimeRegistry(final ShadowReporter reporter) {
        return new ShadowRuntimeRegistry(ShadowWebClientFactory.create(), reporter);
    }

    /**
     * Shadow plugin.
     *
     * @param runtimeRegistry the shadow runtime registry
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin shadowPlugin(final ShadowRuntimeRegistry runtimeRegistry) {
        return new ShadowPlugin(runtimeRegistry);
    }

    /**
     * Shadow plugin data handler.
     *
     * @param runtimeRegistry the shadow runtime registry
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler shadowPluginDataHandler(final ShadowRuntimeRegistry runtimeRegistry) {
        return new ShadowPluginDataHandler(runtimeRegistry);
    }
}
