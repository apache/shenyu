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
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.autoconfigure.web.ServerProperties;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
        props.setProperty("heartbeatDirectExecution", "true");

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
    void testPeriodicHeartbeatSurvivesFailuresOnEveryServer() throws IOException {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = Mockito.mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            register.when(() -> RegisterUtils.doHeartBeat(anyString(), anyString(), anyString(), anyString()))
                    .thenThrow(new IOException("admin unavailable"));
            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);
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
        props.setProperty("heartbeatDirectExecution", "true");

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

            heartbeatListener = new HeartbeatListener(config, shenyuConfig, serverProperties);

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

    @Test
    void testSlowAdminDoesNotDelayHealthyAdmin() throws Exception {
        CountDownLatch slowAdminStarted = new CountDownLatch(1);
        CountDownLatch slowAdminRelease = new CountDownLatch(1);
        CountDownLatch healthyAdminCompleted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        Mockito.doAnswer(invocation -> {
            String url = invocation.getArgument(1);
            if (url.contains("9095")) {
                slowAdminStarted.countDown();
                slowAdminRelease.await(2, TimeUnit.SECONDS);
            } else if (url.contains("9096")) {
                healthyAdminCompleted.countDown();
            }
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        ShenyuBootstrapHeartBeatConfig asyncConfig = new ShenyuBootstrapHeartBeatConfig();
        asyncConfig.setServerLists("http://localhost:9095,http://localhost:9096");
        asyncConfig.setProps(props);

        ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            heartbeatListener = new HeartbeatListener(asyncConfig, shenyuConfig, serverProperties, pool, beatSender);
            Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);
            InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

            sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

            assertTrue(slowAdminStarted.await(1, TimeUnit.SECONDS));
            assertTrue(healthyAdminCompleted.await(1, TimeUnit.SECONDS),
                    "Healthy admin heartbeat must complete promptly without being delayed by slow admin");
        } finally {
            slowAdminRelease.countDown();
            pool.shutdownNow();
        }
    }

    @Test
    void testTokenLoadingStallDoesNotDelayHealthyAdmin() throws Exception {
        CountDownLatch slowLoginStarted = new CountDownLatch(1);
        CountDownLatch slowLoginRelease = new CountDownLatch(1);
        CountDownLatch healthyAdminCompleted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenAnswer(invocation -> {
            String url = invocation.getArgument(2);
            if (url.contains("9095")) {
                slowLoginStarted.countDown();
                slowLoginRelease.await(2, TimeUnit.SECONDS);
                return Optional.of("token-9095");
            }
            return Optional.of("token-9096");
        });
        Mockito.doAnswer(invocation -> {
            String url = invocation.getArgument(1);
            if (url.contains("9096")) {
                healthyAdminCompleted.countDown();
            }
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        ShenyuBootstrapHeartBeatConfig asyncConfig = new ShenyuBootstrapHeartBeatConfig();
        asyncConfig.setServerLists("http://localhost:9095,http://localhost:9096");
        asyncConfig.setProps(props);

        ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            heartbeatListener = new HeartbeatListener(asyncConfig, shenyuConfig, serverProperties, pool, beatSender);
            Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);
            InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

            sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

            assertTrue(slowLoginStarted.await(1, TimeUnit.SECONDS));
            assertTrue(healthyAdminCompleted.await(1, TimeUnit.SECONDS),
                    "Healthy admin must complete promptly even if token loading stalls for another admin");
        } finally {
            slowLoginRelease.countDown();
            pool.shutdownNow();
        }
    }

    @Test
    void testHeartbeatTimeoutDeadlineCancelsStalledTask() throws Exception {
        CountDownLatch taskInterrupted = new CountDownLatch(1);
        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        Mockito.doAnswer(invocation -> {
            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                taskInterrupted.countDown();
                throw e;
            }
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "200");
        ShenyuBootstrapHeartBeatConfig timeoutConfig = new ShenyuBootstrapHeartBeatConfig();
        timeoutConfig.setServerLists("http://localhost:9095");
        timeoutConfig.setProps(props);

        ExecutorService pool = Executors.newSingleThreadExecutor();
        try {
            heartbeatListener = new HeartbeatListener(timeoutConfig, shenyuConfig, serverProperties, pool, beatSender);
            Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);
            InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

            sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

            assertTrue(taskInterrupted.await(2, TimeUnit.SECONDS),
                    "Stalled heartbeat task must be interrupted when deadline expires");
        } finally {
            pool.shutdownNow();
        }
    }

    @Test
    void testOverlappingHeartbeatSkippedWhenPreviousInFlight() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class)) {
            CountDownLatch firstBeatStarted = new CountDownLatch(1);
            CountDownLatch releaseFirstBeat = new CountDownLatch(1);
            AtomicInteger beatCount = new AtomicInteger(0);

            HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
            when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            Mockito.doAnswer(invocation -> {
                beatCount.incrementAndGet();
                firstBeatStarted.countDown();
                releaseFirstBeat.await(2, TimeUnit.SECONDS);
                return null;
            }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

            Properties props = new Properties();
            props.setProperty(Constants.USER_NAME, "admin");
            props.setProperty(Constants.PASS_WORD, "123456");
            ShenyuBootstrapHeartBeatConfig configSingle = new ShenyuBootstrapHeartBeatConfig();
            configSingle.setServerLists("http://localhost:9095");
            configSingle.setProps(props);

            ExecutorService pool = Executors.newFixedThreadPool(2);
            try {
                heartbeatListener = new HeartbeatListener(configSingle, shenyuConfig, serverProperties, pool, beatSender);
                Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
                sendHeartbeatMethod.setAccessible(true);
                InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

                sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
                assertTrue(firstBeatStarted.await(1, TimeUnit.SECONDS));

                // Second trigger while first is still in flight -> skipped
                sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
                assertEquals(1, beatCount.get());

                // Release first beat
                releaseFirstBeat.countDown();
                Thread.sleep(100);

                // Third trigger after completion -> proceeds
                sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
                Thread.sleep(100);
                assertEquals(2, beatCount.get());
            } finally {
                releaseFirstBeat.countDown();
                pool.shutdownNow();
            }
        }
    }

    @Test
    void testPerDestinationTokenOwnership() throws Exception {
        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), eq("http://localhost:9095" + Constants.LOGIN_PATH)))
                .thenReturn(Optional.of("token-for-9095"));
        when(beatSender.login(anyString(), anyString(), eq("http://localhost:9096" + Constants.LOGIN_PATH)))
                .thenReturn(Optional.of("token-for-9096"));

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatDirectExecution", "true");
        ShenyuBootstrapHeartBeatConfig configTwo = new ShenyuBootstrapHeartBeatConfig();
        configTwo.setServerLists("http://localhost:9095,http://localhost:9096");
        configTwo.setProps(props);

        heartbeatListener = new HeartbeatListener(configTwo, shenyuConfig, serverProperties, null, beatSender);
        Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeatMethod.setAccessible(true);
        InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

        verify(beatSender, Mockito.atLeastOnce()).doHeartBeat(anyString(), eq("http://localhost:9095" + Constants.BEAT_URI_PATH), anyString(), eq("token-for-9095"));
        verify(beatSender, Mockito.atLeastOnce()).doHeartBeat(anyString(), eq("http://localhost:9096" + Constants.BEAT_URI_PATH), anyString(), eq("token-for-9096"));
    }
}
