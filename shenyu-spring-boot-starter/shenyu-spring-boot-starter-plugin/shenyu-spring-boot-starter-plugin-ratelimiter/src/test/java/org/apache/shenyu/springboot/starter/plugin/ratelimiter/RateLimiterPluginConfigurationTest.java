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

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link RateLimiterPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class RateLimiterPluginConfigurationTest {

    @Test
    public void testRateLimiterPlugin() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RateLimiterPluginConfiguration.class))
            .withBean(RateLimiterPluginConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                assertThat(context).hasSingleBean(PluginDataHandler.class);
                ShenyuPlugin plugin = context.getBean("rateLimiterPlugin", ShenyuPlugin.class);
                assertNotNull(plugin);
                assertThat(plugin.named()).isEqualTo(PluginEnum.RATE_LIMITER.getName());
            });
    }

    @Test
    public void testRateLimiterPluginDataHandler() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RateLimiterPluginConfiguration.class))
            .withBean(RateLimiterPluginConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                PluginDataHandler handler = context.getBean("rateLimiterPluginDataHandler", PluginDataHandler.class);
                assertNotNull(handler);
            });
    }
}
