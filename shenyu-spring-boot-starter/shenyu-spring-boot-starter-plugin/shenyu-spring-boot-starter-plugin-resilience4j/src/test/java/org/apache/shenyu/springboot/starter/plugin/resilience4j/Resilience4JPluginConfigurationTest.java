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

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.resilience4j.Resilience4JPlugin;
import org.apache.shenyu.plugin.resilience4j.handler.Resilience4JHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link Resilience4JPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class Resilience4JPluginConfigurationTest {

    @Test
    public void testResilience4JPlugin() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(Resilience4JPluginConfiguration.class))
            .withBean(Resilience4JPluginConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                Resilience4JPlugin plugin = context.getBean("resilience4JPlugin", Resilience4JPlugin.class);
                assertNotNull(plugin);
                assertThat(plugin.named()).isEqualTo(PluginEnum.RESILIENCE4J.getName());
            });
    }

    @Test
    public void testResilience4JHandler() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(Resilience4JPluginConfiguration.class))
            .withBean(Resilience4JPluginConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                Resilience4JHandler handler = context.getBean("resilience4JHandler", Resilience4JHandler.class);
                assertNotNull(handler);
            });
    }
}
