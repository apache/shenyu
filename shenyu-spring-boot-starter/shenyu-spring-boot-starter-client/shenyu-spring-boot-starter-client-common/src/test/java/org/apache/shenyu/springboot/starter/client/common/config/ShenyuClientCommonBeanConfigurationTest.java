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

package org.apache.shenyu.springboot.starter.client.common.config;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.enums.RegisterTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Test case for {@link ShenyuClientCommonBeanConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuClientCommonBeanConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(ShenyuClientCommonBeanConfiguration.class))
                .withBean(ShenyuClientCommonBeanConfigurationTest.class)
                .withPropertyValues(
                        "debug=true",
                        "shenyu.register.registerType=http",
                        "shenyu.register.serverLists=http://localhost:9095",
                        "shenyu.register.props.username=admin",
                        "shenyu.register.props.password=123456",
                        "shenyu.client.dubbo.props[contextPath]=/common",
                        "shenyu.client.dubbo.props[appName]=common",
                        "shenyu.client.dubbo.props[host]=localhost",
                        "shenyu.client.dubbo.props[port]=20888"
                );
    }

    @Test
    public void testShenyuClientRegisterRepository() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            ShenyuClientRegisterRepository repository = context.getBean("shenyuClientRegisterRepository", ShenyuClientRegisterRepository.class);
            assertNotNull(repository);
        });
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testShenyuRegisterCenterConfig() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            ShenyuRegisterCenterConfig config = context.getBean("shenyuRegisterCenterConfig", ShenyuRegisterCenterConfig.class);
            assertNotNull(config);
            assertThat(config.getRegisterType()).isEqualTo(RegisterTypeEnum.HTTP.getName());
        });
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testShenyuClientConfig() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            ShenyuClientConfig config = context.getBean("shenyuClientConfig", ShenyuClientConfig.class);
            assertNotNull(config);
            assertThat(config.getClient()).containsKey("dubbo");
        });
        registerUtilsMockedStatic.close();
    }
}
