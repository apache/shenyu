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

package org.apache.shenyu.springboot.starter.plugin.motan;

import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.motan.MotanPlugin;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link MotanPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class MotanPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(MotanPluginConfiguration.class))
            .withBean(MotanPluginConfigurationTest.class)
            .withPropertyValues("debug=true");
    }

    @Test
    public void testMotanPlugin() {
        applicationContextRunner.run(context -> {
                MotanPlugin plugin = context.getBean("motanPlugin", MotanPlugin.class);
                assertNotNull(plugin);
            }
        );
    }

    @Test
    public void testMotanPluginDataHandler() {
        applicationContextRunner.run(context -> {
                PluginDataHandler handler = context.getBean("motanPluginDataHandler", PluginDataHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testMotanMetaDataHandler() {
        applicationContextRunner.run(context -> {
                MetaDataHandler handler = context.getBean("motanMetaDataHandler", MetaDataHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testMotanShenyuContextDecorator() {
        applicationContextRunner.run(context -> {
                ShenyuContextDecorator subscriber = context.getBean("motanShenyuContextDecorator", ShenyuContextDecorator.class);
                assertNotNull(subscriber);
            }
        );
    }
}
