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

package org.apache.shenyu.springboot.starter.client.alibaba.dubbo;

import org.apache.shenyu.client.alibaba.dubbo.AlibabaDubboServiceBeanListener;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Test case for {@link ShenyuAlibabaDubboClientConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
@PropertySource(value = "classpath:application.properties")
public class ShenyuAlibabaDubboClientConfigurationTest {

    @Test
    public void testShenyuAlibabaDubboClientConfiguration() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuAlibabaDubboClientConfiguration.class))
            .withBean(ShenyuAlibabaDubboClientConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(
                context -> {
                    AlibabaDubboServiceBeanListener listener = context.getBean("alibabaDubboServiceBeanListener", AlibabaDubboServiceBeanListener.class);
                    assertNotNull(listener);
                }
            );
        registerUtilsMockedStatic.close();
    }
}
