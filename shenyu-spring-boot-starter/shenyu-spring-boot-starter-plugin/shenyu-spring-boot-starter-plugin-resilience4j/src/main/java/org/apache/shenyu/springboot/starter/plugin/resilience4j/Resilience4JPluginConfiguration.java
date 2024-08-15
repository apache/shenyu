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

package org.apache.shenyu.springboot.starter.plugin.resilience4j;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.resilience4j.Resilience4JPlugin;
import org.apache.shenyu.plugin.resilience4j.executor.CombinedExecutor;
import org.apache.shenyu.plugin.resilience4j.executor.RateLimiterExecutor;
import org.apache.shenyu.plugin.resilience4j.handler.Resilience4JHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Resilience4j plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.resilience4j.enabled"}, havingValue = "true", matchIfMissing = true)
public class Resilience4JPluginConfiguration {

    /**
     * Resilience4j plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin resilience4JPlugin() {
        return new Resilience4JPlugin(new CombinedExecutor(), new RateLimiterExecutor());
    }

    /**
     * Resilience4j handler.
     *
     * @return ResilienceHandler
     */
    @Bean
    public PluginDataHandler resilience4JHandler() {
        return new Resilience4JHandler();
    }
}
