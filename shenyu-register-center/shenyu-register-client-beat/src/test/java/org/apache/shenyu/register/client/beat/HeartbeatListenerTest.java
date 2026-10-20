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
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.MockedConstruction;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.lang.reflect.Field;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;

@ExtendWith(MockitoExtension.class)
class HeartbeatListenerTest {

    private HeartbeatListener heartbeatListener;

    private ShenyuBootstrapHeartBeatConfig config;

    private ShenyuConfig shenyuConfig;

    @BeforeEach
    void setUp() {
        config = createMockConfig();
        shenyuConfig = createMockShenyuConfig();
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

    @Test
    void testPeriodicHeartbeatSurvivesFailuresOnEveryServer() throws IOException {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = Mockito.mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            register.when(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()))
                    .thenThrow(new IOException("admin unavailable"));
            heartbeatListener = new HeartbeatListener(config, shenyuConfig);
            bind(heartbeatListener, null, 8080);
            ArgumentCaptor<Runnable> periodic = ArgumentCaptor.forClass(Runnable.class);
            Mockito.verify(executors.constructed().get(0)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            assertDoesNotThrow(periodic.getValue()::run);
            assertDoesNotThrow(periodic.getValue()::run);
            register.verify(() -> RegisterUtils.doHeartBeat(anyString(), Mockito.eq("http://localhost:9095" + Constants.BEAT_URI_PATH),
                    Mockito.eq(Constants.HEARTBEAT), Mockito.eq("mock-token")), Mockito.times(2));
            register.verify(() -> RegisterUtils.doHeartBeat(anyString(), Mockito.eq("http://localhost:9096" + Constants.BEAT_URI_PATH),
                    Mockito.eq(Constants.HEARTBEAT), Mockito.eq("mock-token")), Mockito.times(2));
        }
    }

    @Test
    void testPeriodicHeartbeatReportsBootstrapInstanceType() {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = Mockito.mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            heartbeatListener = new HeartbeatListener(config, shenyuConfig);
            bind(heartbeatListener, null, 8080);
            ArgumentCaptor<Runnable> periodic = ArgumentCaptor.forClass(Runnable.class);
            Mockito.verify(executors.constructed().get(0)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            periodic.getValue().run();
            ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
            register.verify(() -> RegisterUtils.doHeartBeat(payload.capture(), anyString(), anyString(), anyString()), Mockito.atLeastOnce());
            InstanceBeatInfoDTO reported = GsonUtils.getInstance().fromJson(payload.getAllValues().get(0), InstanceBeatInfoDTO.class);
            assertEquals(InstanceTypeConstants.BOOTSTRAP_INSTANCE_TYPE, reported.getInstanceType());
            assertEquals("8080", reported.getInstancePort());
        }
    }

    @Test
    void testDoesNotReportBeforeTheServerBinds() {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = Mockito.mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            heartbeatListener = new HeartbeatListener(config, shenyuConfig);
            ArgumentCaptor<Runnable> periodic = ArgumentCaptor.forClass(Runnable.class);
            Mockito.verify(executors.constructed().get(0)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            periodic.getValue().run();
            register.verify(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()), never());
            assertEquals(1, executors.constructed().size());
        }
    }

    @Test
    void testReportsBoundPortInsteadOfConfiguredZeroAndIgnoresManagement() {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = Mockito.mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            final HeartbeatListener first = new HeartbeatListener(config, shenyuConfig);
            final HeartbeatListener second = new HeartbeatListener(config, shenyuConfig);
            bind(first, "management", 19090);
            bind(first, null, 0);
            bind(first, null, 41234);
            bind(second, null, 41235);
            ArgumentCaptor<Runnable> periodic = ArgumentCaptor.forClass(Runnable.class);
            Mockito.verify(executors.constructed().get(0)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            Mockito.verify(executors.constructed().get(1)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            periodic.getAllValues().get(0).run();
            periodic.getAllValues().get(1).run();
            ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
            register.verify(() -> RegisterUtils.doHeartBeat(payload.capture(), anyString(), anyString(), anyString()), Mockito.atLeast(2));
            InstanceBeatInfoDTO reportedFirst = GsonUtils.getInstance().fromJson(payload.getAllValues().get(0), InstanceBeatInfoDTO.class);
            InstanceBeatInfoDTO reportedSecond = GsonUtils.getInstance().fromJson(payload.getAllValues().get(2), InstanceBeatInfoDTO.class);
            assertEquals("41234", reportedFirst.getInstancePort());
            assertEquals("41235", reportedSecond.getInstancePort());
            assertNotEquals(reportedFirst.getInstanceIp() + ":" + reportedFirst.getInstancePort(),
                    reportedSecond.getInstanceIp() + ":" + reportedSecond.getInstancePort());
            assertEquals(2, executors.constructed().size());
        }
    }

    @Test
    void testShutdownBeforeInitializationDoesNotReport() {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            heartbeatListener = new HeartbeatListener(config, shenyuConfig);
            ArgumentCaptor<Runnable> periodic = ArgumentCaptor.forClass(Runnable.class);
            Mockito.verify(executors.constructed().get(0)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            heartbeatListener.onShutdown();
            bind(heartbeatListener, null, 9195, true);
            periodic.getValue().run();
            register.verify(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()), never());
            Mockito.verify(executors.constructed().get(0), Mockito.times(1))
                    .scheduleAtFixedRate(Mockito.any(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            assertEquals(1, executors.constructed().size());
        }
    }

    private void bind(final HeartbeatListener listener, final String namespace, final int port) {
        bind(listener, namespace, port, false);
    }

    private void bind(final HeartbeatListener listener, final String namespace, final int port, final boolean lenient) {
        WebServer webServer = Mockito.mock(WebServer.class);
        WebServerApplicationContext applicationContext = Mockito.mock(WebServerApplicationContext.class);
        WebServerInitializedEvent event = Mockito.mock(WebServerInitializedEvent.class);
        if (lenient) {
            Mockito.lenient().when(webServer.getPort()).thenReturn(port);
            Mockito.lenient().when(applicationContext.getServerNamespace()).thenReturn(namespace);
            Mockito.lenient().when(event.getWebServer()).thenReturn(webServer);
            Mockito.lenient().when(event.getApplicationContext()).thenReturn(applicationContext);
        } else {
            Mockito.when(webServer.getPort()).thenReturn(port);
            Mockito.when(applicationContext.getServerNamespace()).thenReturn(namespace);
            Mockito.when(event.getWebServer()).thenReturn(webServer);
            Mockito.when(event.getApplicationContext()).thenReturn(applicationContext);
        }
        listener.onWebServerInitialized(event);
    }

    @Test
    void testHeartbeatListenerCreation() {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            assertDoesNotThrow(() -> {
                heartbeatListener = new HeartbeatListener(config, shenyuConfig);
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
                heartbeatListener = new HeartbeatListener(configWithEncryption, shenyuConfig);
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

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

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

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

            // Use reflection to access private method sendHeartbeat
            java.lang.reflect.Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", 
                    org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);

            org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO beatInfo = 
                    new org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO();

            assertDoesNotThrow(() -> sendHeartbeatMethod.invoke(heartbeatListener, beatInfo));
            assertDoesNotThrow(() -> sendHeartbeatMethod.invoke(heartbeatListener, beatInfo));
            registerUtilsMockedStatic.verify(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()),
                    Mockito.never());
        }
    }

    @Test
    void testOnShutdown() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

            // Use reflection to access the executor
            Field executorField = HeartbeatListener.class.getDeclaredField("executor");
            executorField.setAccessible(true);
            ScheduledThreadPoolExecutor executor = (ScheduledThreadPoolExecutor) executorField.get(heartbeatListener);

            assertNotNull(executor);
            assertTrue(!executor.isShutdown());

            heartbeatListener.onShutdown();

            assertTrue(executor.isShutdown());
        }
    }

    @Test
    void testConfigurationValues() throws Exception {

        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

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

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

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

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

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
