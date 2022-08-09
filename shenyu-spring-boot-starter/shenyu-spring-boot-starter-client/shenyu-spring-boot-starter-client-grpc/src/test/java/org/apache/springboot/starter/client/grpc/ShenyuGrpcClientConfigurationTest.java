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

package org.apache.springboot.starter.client.grpc;

import org.apache.shenyu.client.grpc.GrpcClientEventListener;
import org.apache.shenyu.client.grpc.GrpcContextRefreshedEventListener;
import org.apache.shenyu.client.grpc.server.GrpcServerRunner;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Test case for {@link ShenyuGrpcClientConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
@ComponentScan(value = "org.apache.springboot.starter.client.grpc.server")
public class ShenyuGrpcClientConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuGrpcClientConfiguration.class))
            .withBean(ShenyuGrpcClientConfigurationTest.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://localhost:9095",
                "shenyu.register.props.username=admin",
                "shenyu.register.props.password=123456",
                "shenyu.client.grpc.props[contextPath]=/grpc",
                "shenyu.client.grpc.props[appName]=grpc",
                "shenyu.client.grpc.props[ipAndPort]=127.0.0.1:8080",
                "shenyu.client.grpc.props[port]=8080"
            );
    }

    @Test
    public void testGrpcClientBeanPostProcessor() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            GrpcClientEventListener listener = context.getBean("grpcClientEventListener", GrpcClientEventListener.class);
            assertNotNull(listener);
        });
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testGrpcContextRefreshedEventListener() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            GrpcContextRefreshedEventListener listener = context.getBean("grpcContextRefreshedEventListener", GrpcContextRefreshedEventListener.class);
            assertNotNull(listener);
        });
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testGrpcServerRunner() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            GrpcServerRunner runner = context.getBean("grpcServer", GrpcServerRunner.class);
            assertNotNull(runner);
        });
        registerUtilsMockedStatic.close();
    }
}
