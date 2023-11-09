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

package org.apache.shenyu.springboot.starter.plugin.request;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.request.RequestPlugin;
import org.apache.shenyu.plugin.request.handler.RequestPluginHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link RequestPluginConfiguration}.
 **/
@Configuration
@EnableConfigurationProperties
public class RequestPluginConfigurationTest {

    @Test
    public void testRequestPlugin() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RequestPluginConfiguration.class))
            .withBean(RequestPluginConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                assertThat(context).hasSingleBean(RequestPlugin.class);
                ShenyuPlugin plugin = context.getBean("requestPlugin", ShenyuPlugin.class);
                assertThat(plugin instanceof RequestPlugin).isEqualTo(true);
                assertThat(plugin.named()).isEqualTo(PluginEnum.REQUEST.getName());
                assertNotNull(plugin);
            });
    }

    @Test
    public void testRequestPluginHandler() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RequestPluginConfiguration.class))
            .withBean(RequestPluginConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                assertThat(context).hasSingleBean(RequestPluginHandler.class);
                PluginDataHandler handler = context.getBean("requestPluginDataHandler", PluginDataHandler.class);
                assertNotNull(handler);
            });
    }
}
