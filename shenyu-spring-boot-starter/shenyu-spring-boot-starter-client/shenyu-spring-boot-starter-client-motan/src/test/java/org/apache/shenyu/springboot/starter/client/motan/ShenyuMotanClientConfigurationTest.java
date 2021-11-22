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

package org.apache.shenyu.springboot.starter.client.motan;

import org.apache.shenyu.client.motan.MotanServiceBeanPostProcessor;
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.junit.Assert.assertNotNull;

/**
 * Test case for {@link ShenyuMotanClientConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuMotanClientConfigurationTest {

    @Test
    public void testMotanServiceBeanPostProcessor() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuMotanClientConfiguration.class))
            .withBean(ShenyuMotanClientConfigurationTest.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://localhost:9095",
                "shenyu.client.motan.props[contextPath]=/motan",
                "shenyu.client.motan.props[appName]=motan",
                "shenyu.client.motan.props[host]=127.0.0.1",
                "shenyu.client.motan.props[port]=8081"
            )
            .run(context -> {
                MotanServiceBeanPostProcessor processor = context.getBean("tarsServiceBeanPostProcessor", MotanServiceBeanPostProcessor.class);
                assertNotNull(processor);
            });
    }
}
