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

package org.apache.shenyu.springboot.starter.plugin.key.auth;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for {@link KeyAuthPluginConfiguration}.
 */
public class KeyAuthPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void setUp() {
        applicationContextRunner = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(KeyAuthPluginConfiguration.class))
                .withBean(KeyAuthPluginConfigurationTest.class)
                .withPropertyValues("debug=true");
    }

    @Test
    public void testKeyAuthPlugin() {
        applicationContextRunner.run(context -> assertNotNull(context.getBean("keyAuthPlugin")));
    }

    @Test
    public void testKeyAuthPluginDataHandler() {
        applicationContextRunner.run(context -> assertNotNull(context.getBean("keyAuthPluginDataHandler")));
    }

}
