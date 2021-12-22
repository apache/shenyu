/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.apache.shenyu.springboot.starter.client.tars;

import org.apache.shenyu.client.tars.TarsContextRefreshedEventListener;
import org.apache.shenyu.client.tars.TarsServiceBeanPostProcessor;
import org.junit.Before;
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.junit.Assert.assertNotNull;

/**
 * Test case for {@link ShenyuTarsClientConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuTarsClientConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @Before
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuTarsClientConfiguration.class))
            .withBean(ShenyuTarsClientConfigurationTest.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://localhost:9095",
                "shenyu.client.tars.props[contextPath]=/tars",
                "shenyu.client.tars.props[appName]=tars",
                "shenyu.client.tars.props[host]=127.0.0.1",
                "shenyu.client.tars.props[port]=21715"
            );
    }

    @Test
    public void testTarsServiceBeanPostProcessor() {
        applicationContextRunner.run(context -> {
            TarsServiceBeanPostProcessor processor = context.getBean("tarsServiceBeanPostProcessor", TarsServiceBeanPostProcessor.class);
            assertNotNull(processor);
        });
    }

    @Test
    public void testTarsContextRefreshedEventListener() {
        applicationContextRunner.run(context -> {
            TarsContextRefreshedEventListener listener = context.getBean("tarsContextRefreshedEventListener", TarsContextRefreshedEventListener.class);
            assertNotNull(listener);
        });
    }
}
