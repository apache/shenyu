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

package org.apache.shenyu.springboot.starter.client.spring.websocket;

import java.util.Optional;
import org.apache.shenyu.client.spring.websocket.init.SpringContextRegisterListener;
import org.apache.shenyu.client.spring.websocket.init.SpringWebSocketClientEventListener;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Test case for {@link ShenyuSpringWebSocketClientConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuSpringWebSocketClientConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuSpringWebSocketClientConfiguration.class))
            .withBean(ShenyuSpringWebSocketClientConfigurationTest.class)
            .withPropertyValues(
                "debug=true",
                "shenyu.register.registerType=http",
                "shenyu.register.serverLists=http://localhost:9095",
                "shenyu.register.props.username=admin",
                "shenyu.register.props.password=123456",
                "shenyu.client.websocket.props[contextPath]=/websocket",
                "shenyu.client.websocket.props[appName]=websocket",
                "shenyu.client.websocket.props[port]=8001"
            );
    }

    @Test
    public void testSpringWebSocketClientEventListener() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            SpringWebSocketClientEventListener eventListener = context.getBean("springWebSocketClientEventListener", SpringWebSocketClientEventListener.class);
            assertNotNull(eventListener);
        });
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testContextRegisterListener() {
        MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.ofNullable("token"));
        applicationContextRunner.run(context -> {
            SpringContextRegisterListener listener = context.getBean("webSocketContextRegisterListener", SpringContextRegisterListener.class);
            assertNotNull(listener);
        });
        registerUtilsMockedStatic.close();
    }
}
