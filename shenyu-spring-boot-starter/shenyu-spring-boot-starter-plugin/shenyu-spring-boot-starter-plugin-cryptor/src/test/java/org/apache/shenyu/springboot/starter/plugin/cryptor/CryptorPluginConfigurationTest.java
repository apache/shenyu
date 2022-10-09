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

package org.apache.shenyu.springboot.starter.plugin.cryptor;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.support.DefaultServerCodecConfigurer;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link CryptorPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class CryptorPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(CryptorPluginConfiguration.class, DefaultServerCodecConfigurer.class))
            .withBean(CryptorPluginConfigurationTest.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://localhost:9095",
                "shenyu.client.http.props[contextPath]=/http",
                "shenyu.client.http.props[appName]=http",
                "shenyu.client.http.props[port]=8189",
                "shenyu.cross.enabled=true",
                "shenyu.file.enabled=true",
                "shenyu.exclude.enabled=true"
            );
    }

    @Test
    public void testShenyuPlugin() {
        applicationContextRunner.run(context -> {
                ShenyuPlugin cryptorRequestPlugin = context.getBean("cryptorRequestPlugin", ShenyuPlugin.class);
                assertNotNull(cryptorRequestPlugin);
                ShenyuPlugin cryptorResponsePlugin = context.getBean("cryptorResponsePlugin", ShenyuPlugin.class);
                assertNotNull(cryptorResponsePlugin);
            }
        );
    }

    @Test
    public void testPluginDataHandler() {
        applicationContextRunner.run(context -> {
                PluginDataHandler cryptorRequestPluginDataHandler = context.getBean("cryptorRequestPluginDataHandler", PluginDataHandler.class);
                assertNotNull(cryptorRequestPluginDataHandler);
                PluginDataHandler cryptorResponsePluginDataHandler = context.getBean("cryptorResponsePluginDataHandler", PluginDataHandler.class);
                assertNotNull(cryptorResponsePluginDataHandler);
            }
        );
    }
}
