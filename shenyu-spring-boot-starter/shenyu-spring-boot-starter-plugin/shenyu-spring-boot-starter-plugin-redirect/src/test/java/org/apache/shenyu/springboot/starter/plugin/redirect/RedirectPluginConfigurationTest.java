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

package org.apache.shenyu.springboot.starter.plugin.redirect;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.DispatcherHandler;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * test case for {@link RedirectPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class RedirectPluginConfigurationTest {

    @Test
    public void testRedirectPlugin() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RedirectPluginConfiguration.class))
            .withBean(RedirectPluginConfigurationTest.class)
            .withBean(DispatcherHandler.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                ShenyuPlugin shenyuPlugin = context.getBean("redirectPlugin", ShenyuPlugin.class);
                assertNotNull(shenyuPlugin);
                assertThat(shenyuPlugin.named()).isEqualTo(PluginEnum.REDIRECT.getName());
            });
    }

    @Test
    public void testRedirectPluginDataHandler() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(RedirectPluginConfiguration.class))
            .withBean(RedirectPluginConfigurationTest.class)
            .withBean(DispatcherHandler.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                PluginDataHandler handler = context.getBean("redirectPluginDataHandler", PluginDataHandler.class);
                assertNotNull(handler);
            });
    }
}
