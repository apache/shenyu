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

package org.apache.shenyu.springboot.plugin.dubbo.common;

import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.dubbo.common.param.DubboParamResolveService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link DubboCommonConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class DubboCommonConfigurationTest {

    @Test
    public void testDubboShenyuContextDecorator() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(DubboCommonConfiguration.class))
            .withBean(DubboCommonConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                ShenyuContextDecorator decorator = context.getBean("dubboShenyuContextDecorator", ShenyuContextDecorator.class);
                assertNotNull(decorator);
            });
    }

    @Test
    public void testDubboParamResolveServiceImpl() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(DubboCommonConfiguration.class))
            .withBean(DubboCommonConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(context -> {
                DubboParamResolveService service = context.getBean("dubboParamResolveService", DubboParamResolveService.class);
                assertNotNull(service);
            });
    }
}
