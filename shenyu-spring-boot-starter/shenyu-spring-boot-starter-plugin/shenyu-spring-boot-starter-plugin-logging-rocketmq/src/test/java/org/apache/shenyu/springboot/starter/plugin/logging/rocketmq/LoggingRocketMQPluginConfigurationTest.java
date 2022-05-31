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

package org.apache.shenyu.springboot.starter.plugin.logging.rocketmq;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * logging RocketMQ plugin config test.
 */
@Configuration
@EnableConfigurationProperties
public class LoggingRocketMQPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(LoggingRocketMQPluginConfiguration.class))
                .withBean(LoggingRocketMQPluginConfigurationTest.class);
    }

    @Test
    public void testLoggingRocketMQPlugin() {
        applicationContextRunner
                .withPropertyValues(
                        "debug=true",
                        "shenyu.logging.rocketmq.enabled=true"
                )
                .run(context -> {
                    PluginDataHandler pluginDataHandler = context.getBean("loggingRocketMQPluginDataHandler", PluginDataHandler.class);
                    assertNotNull(pluginDataHandler);

                    ShenyuPlugin plugin = context.getBean("loggingRocketMQPlugin", ShenyuPlugin.class);
                    assertNotNull(plugin);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.LOGGING_ROCKETMQ.getName());
                });
    }
}
