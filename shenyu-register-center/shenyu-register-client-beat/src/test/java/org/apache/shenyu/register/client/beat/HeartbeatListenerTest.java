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
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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
            register.verify(() -> RegisterUtils.doHeartBeat(anyString(), Mockito.eq("http://localhost:9095".concat(Constants.BEAT_URI_PATH)),
                    Mockito.eq(Constants.HEARTBEAT), Mockito.eq("mock-token")), Mockito.times(2));
            register.verify(() -> RegisterUtils.doHeartBeat(anyString(), Mockito.eq("http://localhost:9096".concat(Constants.BEAT_URI_PATH)),
                    Mockito.eq(Constants.HEARTBEAT), Mockito.eq("mock-token")), Mockito.times(2));
        }
    }

    @Test
    void testPeriodicHeartbeatPublishesBootstrapInstanceType() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> executors = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> system = Mockito.mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = Mockito.mockStatic(RegisterUtils.class)) {
            system.when(SystemInfoUtils::getSystemInfo).thenReturn("system-info");
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            heartbeatListener = new HeartbeatListener(config, shenyuConfig);
            bind(heartbeatListener, null, 8080);
            ArgumentCaptor<Runnable> periodic = ArgumentCaptor.forClass(Runnable.class);
            Mockito.verify(executors.constructed().get(0)).scheduleAtFixedRate(periodic.capture(), Mockito.eq(0L), Mockito.eq(5L), Mockito.eq(TimeUnit.SECONDS));
            periodic.getValue().run();
            ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
            register.verify(() -> RegisterUtils.doHeartBeat(payload.capture(), Mockito.eq("http://localhost:9095".concat(Constants.BEAT_URI_PATH)),
                    Mockito.eq(Constants.HEARTBEAT), Mockito.eq("mock-token")));
            InstanceBeatInfoDTO beatInfo = GsonUtils.getInstance().fromJson(payload.getValue(), InstanceBeatInfoDTO.class);
            assertEquals(InstanceTypeConstants.BOOTSTRAP_INSTANCE_TYPE, beatInfo.getInstanceType());
            assertEquals("8080", beatInfo.getInstancePort());
            assertEquals("shenyu", beatInfo.getNamespaceId());
            assertEquals("system-info", beatInfo.getInstanceInfo());
        }
    }

    @Test
    void testHeartbeatStop() throws Exception {
        try (MockedStatic<RegisterUtils> registerUtilsMockedStatic = Mockito.mockStatic(RegisterUtils.class)) {
            registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString()))
                    .thenReturn(Optional.of("mock-token"));

            heartbeatListener = new HeartbeatListener(config, shenyuConfig);

            Field executorField = HeartbeatListener.class.getDeclaredField("executor");
            executorField.setAccessible(true);
            ScheduledThreadPoolExecutor executor = (ScheduledThreadPoolExecutor) executorField.get(heartbeatListener);

            assertFalse(executor.isShutdown());

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

            Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", 
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
    void testHeartbeatRunsOnDedicatedWorkerPool() throws Exception {
        CountDownLatch latch = new CountDownLatch(2);
        AtomicInteger executedCount = new AtomicInteger(0);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        Mockito.doAnswer(invocation -> {
            executedCount.incrementAndGet();
            latch.countDown();
            assertNotEquals("main", Thread.currentThread().getName());
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        ShenyuBootstrapHeartBeatConfig poolConfig = new ShenyuBootstrapHeartBeatConfig();
        poolConfig.setServerLists("http://localhost:9095,http://localhost:9096");
        poolConfig.setProps(props);

        heartbeatListener = new HeartbeatListener(poolConfig, shenyuConfig, null, beatSender);

        Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeatMethod.setAccessible(true);
        InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

        assertTrue(latch.await(2, TimeUnit.SECONDS));
        assertEquals(2, executedCount.get());
        heartbeatListener.onShutdown();
    }

    @Test
    void testHeartbeatTimeoutDeadlineEnforced() throws Exception {
        CountDownLatch beatStarted = new CountDownLatch(1);
        CountDownLatch beatInterrupted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        Mockito.doAnswer(invocation -> {
            beatStarted.countDown();
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                beatInterrupted.countDown();
            }
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "100");
        ShenyuBootstrapHeartBeatConfig timeoutConfig = new ShenyuBootstrapHeartBeatConfig();
        timeoutConfig.setServerLists("http://localhost:9095");
        timeoutConfig.setProps(props);

        heartbeatListener = new HeartbeatListener(timeoutConfig, shenyuConfig, null, beatSender);

        Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeatMethod.setAccessible(true);
        InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

        assertTrue(beatStarted.await(1, TimeUnit.SECONDS));
        assertTrue(beatInterrupted.await(2, TimeUnit.SECONDS));
        heartbeatListener.onShutdown();
    }

    @Test
    void testSlowAdminDoesNotBlockHealthyAdmin() throws Exception {
        CountDownLatch slowStarted = new CountDownLatch(1);
        CountDownLatch healthyCompleted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));

        Mockito.doAnswer(invocation -> {
            slowStarted.countDown();
            Thread.sleep(5000);
            return null;
        }).when(beatSender).doHeartBeat(anyString(), eq("http://localhost:9095".concat(Constants.BEAT_URI_PATH)), anyString(), anyString());

        Mockito.doAnswer(invocation -> {
            healthyCompleted.countDown();
            return null;
        }).when(beatSender).doHeartBeat(anyString(), eq("http://localhost:9096".concat(Constants.BEAT_URI_PATH)), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "500");
        ShenyuBootstrapHeartBeatConfig isolateConfig = new ShenyuBootstrapHeartBeatConfig();
        isolateConfig.setServerLists("http://localhost:9095,http://localhost:9096");
        isolateConfig.setProps(props);

        heartbeatListener = new HeartbeatListener(isolateConfig, shenyuConfig, null, beatSender);

        Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeatMethod.setAccessible(true);
        InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

        assertTrue(slowStarted.await(1, TimeUnit.SECONDS));
        assertTrue(healthyCompleted.await(1, TimeUnit.SECONDS));
        heartbeatListener.onShutdown();
    }

    @Test
    void testConcurrentHeartbeatToSameAdminSuppressed() throws Exception {
        CountDownLatch holdingBeat = new CountDownLatch(1);
        CountDownLatch releaseBeat = new CountDownLatch(1);
        AtomicInteger totalDispatches = new AtomicInteger(0);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        Mockito.doAnswer(invocation -> {
            totalDispatches.incrementAndGet();
            holdingBeat.countDown();
            releaseBeat.await(2, TimeUnit.SECONDS);
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "5000");
        ShenyuBootstrapHeartBeatConfig singleServerConfig = new ShenyuBootstrapHeartBeatConfig();
        singleServerConfig.setServerLists("http://localhost:9095");
        singleServerConfig.setProps(props);

        heartbeatListener = new HeartbeatListener(singleServerConfig, shenyuConfig, null, beatSender);

        Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeatMethod.setAccessible(true);
        InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
        assertTrue(holdingBeat.await(1, TimeUnit.SECONDS));

        // Second tick while first is still in flight: must be skipped
        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

        assertEquals(1, totalDispatches.get());

        releaseBeat.countDown();
        heartbeatListener.onShutdown();
    }

    @Test
    void testTokenLoadingDoesNotBlockOtherServers() throws Exception {
        CountDownLatch slowLoginStarted = new CountDownLatch(1);
        CountDownLatch fastLoginCompleted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        Mockito.doAnswer(invocation -> {
            slowLoginStarted.countDown();
            Thread.sleep(2000);
            return Optional.of("slow-token");
        }).when(beatSender).login(anyString(), anyString(), eq("http://localhost:9095".concat(Constants.LOGIN_PATH)));

        Mockito.doAnswer(invocation -> {
            fastLoginCompleted.countDown();
            return Optional.of("fast-token");
        }).when(beatSender).login(anyString(), anyString(), eq("http://localhost:9096".concat(Constants.LOGIN_PATH)));

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        ShenyuBootstrapHeartBeatConfig loginConfig = new ShenyuBootstrapHeartBeatConfig();
        loginConfig.setServerLists("http://localhost:9095,http://localhost:9096");
        loginConfig.setProps(props);

        heartbeatListener = new HeartbeatListener(loginConfig, shenyuConfig, null, beatSender);

        Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeatMethod.setAccessible(true);
        InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

        sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

        assertTrue(slowLoginStarted.await(1, TimeUnit.SECONDS));
        assertTrue(fastLoginCompleted.await(1, TimeUnit.SECONDS));
        heartbeatListener.onShutdown();
    }

    @Test
    void testDirectExecutionConfigurationRespected() throws Exception {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatDirectExecution", "true");

        ShenyuBootstrapHeartBeatConfig directCfg = new ShenyuBootstrapHeartBeatConfig();
        directCfg.setServerLists("http://localhost:9095");
        directCfg.setProps(props);

        heartbeatListener = new HeartbeatListener(directCfg, shenyuConfig);

        Field directField = HeartbeatListener.class.getDeclaredField("directExecution");
        directField.setAccessible(true);
        boolean direct = (boolean) directField.get(heartbeatListener);
        assertTrue(direct);

        Field executorField = HeartbeatListener.class.getDeclaredField("beatExecutor");
        executorField.setAccessible(true);
        Object exec = executorField.get(heartbeatListener);
        assertNotNull(exec);
    }

    @Test
    void testNegativeDeadlineDisablesTimeout() throws Exception {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "-1");

        ShenyuBootstrapHeartBeatConfig noTimeoutCfg = new ShenyuBootstrapHeartBeatConfig();
        noTimeoutCfg.setServerLists("http://localhost:9095");
        noTimeoutCfg.setProps(props);

        heartbeatListener = new HeartbeatListener(noTimeoutCfg, shenyuConfig);

        Field deadlineField = HeartbeatListener.class.getDeclaredField("deadlineMs");
        deadlineField.setAccessible(true);
        long deadline = (long) deadlineField.get(heartbeatListener);
        assertEquals(-1L, deadline);
    }

    @Test
    void testMalformedHeartbeatTimeoutMsFallback() throws Exception {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "invalid");

        ShenyuBootstrapHeartBeatConfig cfg = new ShenyuBootstrapHeartBeatConfig();
        cfg.setServerLists("http://localhost:9095");
        cfg.setProps(props);

        heartbeatListener = new HeartbeatListener(cfg, shenyuConfig);

        Field deadlineField = HeartbeatListener.class.getDeclaredField("deadlineMs");
        deadlineField.setAccessible(true);
        long deadline = (long) deadlineField.get(heartbeatListener);
        assertEquals(4000L, deadline);
    }

    @Test
    void testCancelledQueuedTaskClearsInFlightFlag() throws Exception {
        CountDownLatch firstBeatStarted = new CountDownLatch(1);
        CountDownLatch releaseFirstBeat = new CountDownLatch(1);
        CountDownLatch secondBeatExecuted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));

        Mockito.doAnswer(invocation -> {
            firstBeatStarted.countDown();
            releaseFirstBeat.await(2, TimeUnit.SECONDS);
            return null;
        }).when(beatSender).doHeartBeat(anyString(), eq("http://localhost:9095".concat(Constants.BEAT_URI_PATH)), anyString(), anyString());

        Mockito.doAnswer(invocation -> {
            secondBeatExecuted.countDown();
            return null;
        }).when(beatSender).doHeartBeat(anyString(), eq("http://localhost:9096".concat(Constants.BEAT_URI_PATH)), anyString(), anyString());

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatTimeoutMs", "100");
        ShenyuBootstrapHeartBeatConfig queueConfig = new ShenyuBootstrapHeartBeatConfig();
        queueConfig.setServerLists("http://localhost:9095,http://localhost:9096");
        queueConfig.setProps(props);

        ExecutorService singleThreadExecutor = Executors.newSingleThreadExecutor();
        try {
            heartbeatListener = new HeartbeatListener(queueConfig, shenyuConfig, singleThreadExecutor, beatSender);
            Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);
            InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

            sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);

            assertTrue(firstBeatStarted.await(1, TimeUnit.SECONDS));

            // Wait until 9096's deadline (100ms) expires and cancels its queued task
            Thread.sleep(250);

            Field inFlightField = HeartbeatListener.class.getDeclaredField("inFlight");
            inFlightField.setAccessible(true);
            @SuppressWarnings("unchecked")
            Map<String, AtomicBoolean> inFlightMap = (Map<String, AtomicBoolean>) inFlightField.get(heartbeatListener);
            assertNotNull(inFlightMap.get("http://localhost:9096"));
            assertFalse(inFlightMap.get("http://localhost:9096").get(),
                    "Cancelled queued task must clear the in-flight flag for that server");

            // Release the worker thread
            releaseFirstBeat.countDown();
            Thread.sleep(100);

            // Subsequent heartbeat to 9096 must now proceed and execute
            sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
            assertTrue(secondBeatExecuted.await(1, TimeUnit.SECONDS),
                    "Subsequent heartbeat must execute after queued task was cancelled");
        } finally {
            releaseFirstBeat.countDown();
            singleThreadExecutor.shutdownNow();
        }
    }

    @Test
    void testRejectedTaskClearsInFlightFlag() throws Exception {
        CountDownLatch workerHolding = new CountDownLatch(1);
        CountDownLatch releaseWorker = new CountDownLatch(1);
        CountDownLatch heartbeatExecuted = new CountDownLatch(1);

        HeartbeatListener.BeatSender beatSender = mock(HeartbeatListener.BeatSender.class);
        when(beatSender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        Mockito.doAnswer(invocation -> {
            heartbeatExecuted.countDown();
            return null;
        }).when(beatSender).doHeartBeat(anyString(), anyString(), anyString(), anyString());

        ThreadPoolExecutor rejectingPool = new ThreadPoolExecutor(
                1, 1, 0, TimeUnit.SECONDS,
                new SynchronousQueue<>(),
                new ThreadPoolExecutor.AbortPolicy());

        // Fill the single worker thread so any new submit will be rejected
        rejectingPool.submit(() -> {
            workerHolding.countDown();
            releaseWorker.await(2, TimeUnit.SECONDS);
            return null;
        });
        assertTrue(workerHolding.await(1, TimeUnit.SECONDS));

        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        ShenyuBootstrapHeartBeatConfig configSingle = new ShenyuBootstrapHeartBeatConfig();
        configSingle.setServerLists("http://localhost:9095");
        configSingle.setProps(props);

        try {
            heartbeatListener = new HeartbeatListener(configSingle, shenyuConfig, rejectingPool, beatSender);
            Method sendHeartbeatMethod = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
            sendHeartbeatMethod.setAccessible(true);
            InstanceBeatInfoDTO beatInfo = new InstanceBeatInfoDTO();

            // Submit will be rejected, but must not throw or leave flag stuck
            assertDoesNotThrow(() -> sendHeartbeatMethod.invoke(heartbeatListener, beatInfo));

            Field inFlightField = HeartbeatListener.class.getDeclaredField("inFlight");
            inFlightField.setAccessible(true);
            @SuppressWarnings("unchecked")
            Map<String, AtomicBoolean> inFlightMap = (Map<String, AtomicBoolean>) inFlightField.get(heartbeatListener);
            assertNotNull(inFlightMap.get("http://localhost:9095"));
            assertFalse(inFlightMap.get("http://localhost:9095").get(),
                    "Rejected task must clear in-flight flag immediately");

            // Release worker so subsequent heartbeat can succeed
            releaseWorker.countDown();
            Thread.sleep(100);

            sendHeartbeatMethod.invoke(heartbeatListener, beatInfo);
            assertTrue(heartbeatExecuted.await(1, TimeUnit.SECONDS),
                    "Heartbeat must succeed once executor becomes available again");
        } finally {
            releaseWorker.countDown();
            rejectingPool.shutdownNow();
        }
    }

    private void bind(final HeartbeatListener listener, final String namespace, final int port) {
        WebServerInitializedEvent event = Mockito.mock(WebServerInitializedEvent.class);
        WebServer webServer = Mockito.mock(WebServer.class);
        WebServerApplicationContext applicationContext = Mockito.mock(WebServerApplicationContext.class);
        Mockito.when(event.getWebServer()).thenReturn(webServer);
        Mockito.when(event.getApplicationContext()).thenReturn(applicationContext);
        Mockito.when(webServer.getPort()).thenReturn(port);
        Mockito.when(applicationContext.getServerNamespace()).thenReturn(namespace);
        listener.onWebServerInitialized(event);
    }
}
