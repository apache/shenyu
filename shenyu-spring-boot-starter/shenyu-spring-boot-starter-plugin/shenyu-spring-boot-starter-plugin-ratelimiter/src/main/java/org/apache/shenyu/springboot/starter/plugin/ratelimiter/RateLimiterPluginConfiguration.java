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

package org.apache.shenyu.springboot.starter.plugin.ratelimiter;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.ratelimiter.RateLimiterPlugin;
import org.apache.shenyu.plugin.ratelimiter.executor.RedisRateLimiter;
import org.apache.shenyu.plugin.ratelimiter.handler.RateLimiterPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type RateLimiter plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.rate-limiter.enabled"}, havingValue = "true", matchIfMissing = true)
public class RateLimiterPluginConfiguration {
    
    /**
     * RateLimiter plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin rateLimiterPlugin() {
        return new RateLimiterPlugin(new RedisRateLimiter());
    }
    
    /**
     * Rate limiter plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler rateLimiterPluginDataHandler() {
        return new RateLimiterPluginDataHandler();
    }
}
