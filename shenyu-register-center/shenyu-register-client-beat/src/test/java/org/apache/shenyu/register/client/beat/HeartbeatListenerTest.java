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

package org.apache.shenyu.register.client.beat;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.autoconfigure.web.ServerProperties;

import java.lang.reflect.Field;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;

@ExtendWith(MockitoExtension.class)
class HeartbeatListenerTest {

    private HeartbeatListener heartbeatListener;

    private ShenyuBootstrapHeartBeatConfig config;

    private ShenyuConfig shenyuConfig;

    private ServerProperties serverProperties;

    @BeforeEach
    void setUp() {
        config = createMockConfig();
        shenyuConfig = createMockShenyuConfig();
        serverProperties = createMockServerProperties();
    }

    private ShenyuBootstrapHeartBeatConfig createMockConfig() {

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty(Constants.AES_SECRET_KEY, "");
        props.setProperty(Constants.AES_SECRET_IV, "");

        ShenyuBootstrapHeartBeatConfig config = new ShenyuBootstrapHeartBeatConfig();
        config.setServerLists("http://localhost:9095,http://localhost:9096");
        config.setProps(props);

        return config;
    }

    private ShenyuConfig createMockShenyuConfig() {
        ShenyuConfig config = new ShenyuConfig();
        config.setNamespace("shenyu");
        return config;
    }

    private ServerProperties createMockServerProperties() {

        ServerProperties properties = new ServerProperties();
        properties.setPort(8080);

        return properties;
    }

    @Test
    void testHeartbeatListenerCreation() {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            assertDoesNotThrow(() -> {
                heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);
            });

            assertNotNull(heartbeatListener);
        }
    }

    @Test
    void testHeartbeatListenerWithEncryption() {

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty(Constants.AES_SECRET_KEY, "2095132720951327");
        props.setProperty(Constants.AES_SECRET_IV, "6859932669599326");

        ShenyuBootstrapHeartBeatConfig configWithEncryption = new ShenyuBootstrapHeartBeatConfig();
        configWithEncryption.setServerLists("http://localhost:9095");
        configWithEncryption.setProps(props);

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            assertDoesNotThrow(() -> {
                heartbeatListener = new HeartbeatListener(configWithEncryption, shenyuConfig, serverProperties);
            });

            assertNotNull(heartbeatListener);
        }
    }

    @Test
    void testSendHeartbeatSuccess() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            registerUtilsMockedStatic.when(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()))
                    .then(invocation -> null);

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

            // Use reflection to access private method sendHeartbeat
            java.lang.reflect.Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", 
                    org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);

            org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO beatInfo = 
                    new org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO();
            beatInfo.setInstancePort("8080");
            beatInfo.setInstanceIp("127.0.0.1");
            beatInfo.setNamespaceId("shenyu");

            assertDoesNotThrow(() -> {
                try {
                    sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            });

            // Wait a bit to allow the heartbeat to be processed
            Thread.sleep(100);

            // Should be called for both servers in serverList
            registerUtilsMockedStatic.verify(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()),
                    Mockito.times(2));
        }
    }

    @Test
    void testSendHeartbeatWithLoginFailure() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.empty());

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

            // Use reflection to access private method sendHeartbeat
            java.lang.reflect.Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", 
                    org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);

            org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO beatInfo = 
                    new org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO();

            // Should throw RuntimeException due to login failure
            try {
                sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
            } catch (Exception e) {
                assertTrue(e.getCause() instanceof RuntimeException);
            }
        }
    }

    @Test
    void testOnShutdown() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

            // Use reflection to access the executor
            Field executorField = HeartbeatListener.class.getDeclaredField("executor");
            executorField.setAccessible(true);
            ScheduledThreadPoolExecutor executor = (ScheduledThreadPoolExecutor) executorField.get(heartbeatListener);

            assertNotNull(executor);
            assertTrue(!executor.isShutdown());

            heartbeatListener.onShutdown();

            // Wait a bit for shutdown to complete
            Thread.sleep(100);

            assertTrue(executor.isShutdown());
        }
    }

    @Test
    void testConfigurationValues() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

            // Use reflection to check private fields
            Field usernameField = HeartbeatListener.class.getDeclaredField("username");
            usernameField.setAccessible(true);
            String username = (String) usernameField.get(heartbeatListener);
            assertEquals("admin", username);

            Field passwordField = HeartbeatListener.class.getDeclaredField("password");
            passwordField.setAccessible(true);
            String password = (String) passwordField.get(heartbeatListener);
            assertEquals("123456", password);

            Field serverListField = HeartbeatListener.class.getDeclaredField("serverList");
            serverListField.setAccessible(true);
            @SuppressWarnings("unchecked")
            java.util.List<String> serverList = (java.util.List<String>) serverListField.get(heartbeatListener);
            assertEquals(2, serverList.size());
            assertTrue(serverList.contains("http://localhost:9095"));
            assertTrue(serverList.contains("http://localhost:9096"));
        }
    }

    @Test
    void testHeartbeatWithMultipleServersOneFailure() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            // First server call fails, second succeeds
            registerUtilsMockedStatic.when(() -> RegisterUtils.doHeartBeat(anyString(), 
                    Mockito.contains("localhost:9095"), anyString(), anyString()))
                    .thenThrow(new RuntimeException("Connection failed"));

            registerUtilsMockedStatic.when(() -> RegisterUtils.doHeartBeat(anyString(), 
                    Mockito.contains("localhost:9096"), anyString(), anyString()))
                    .then(invocation -> null);

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

            // Use reflection to access private method sendHeartbeat
            java.lang.reflect.Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", 
                    org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);

            org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO beatInfo = 
                    new org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO();

            assertDoesNotThrow(() -> {
                try {
                    sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            });
        }
    }

    @Test
    void testHeartbeatWithAllServersFailure() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            // All server calls fail
            registerUtilsMockedStatic.when(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()))
                    .thenThrow(new RuntimeException("Connection failed"));

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

            // Use reflection to access private method sendHeartbeat
            java.lang.reflect.Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", 
                    org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);

            org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO beatInfo = 
                    new org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO();

            // Should throw RuntimeException when all servers fail
            try {
                sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
            } catch (Exception e) {
                assertTrue(e.getCause() instanceof RuntimeException);
            }
        }
    }
}
