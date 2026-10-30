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

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.exception.HttpResponseException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.mockingDetails;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercises token reuse and renewal through real loopback HTTP requests, without waiting for scheduled ticks.
 */
@Timeout(30)
class HeartbeatTokenRefreshTest {

    private final AtomicLong elapsedNanos = new AtomicLong();

    @Test
    void testValidTokenRemainsCached() throws Exception {
        try (TestAdmin admin = new TestAdmin();
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(admin.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                tick(heartbeat);
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(1, admin.logins.get(), "A valid token should be reused"),
                        () -> assertEquals(3, admin.accepted.get()),
                        () -> assertEquals(0, admin.rejected.get()));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testRejectedTokenIsRenewedOnNextHeartbeat() throws Exception {
        try (TestAdmin admin = new TestAdmin();
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(admin.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                assertEquals(1, admin.accepted.get(), "The first login and heartbeat must succeed");
                admin.rejectOriginalToken.set(true);
                tick(heartbeat);
                assertEquals(1, admin.rejected.get(), "The Admin must reject the cached token with HTTP 401");
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(2, admin.logins.get(), "A rejected token should trigger a new login"),
                        () -> assertEquals(2, admin.accepted.get(), "Heartbeat reporting should recover"),
                        () -> assertEquals(1, admin.rejected.get(), "The rejected token should not be reused"),
                        () -> assertEquals(List.of("test-token-1", "test-token-1", "test-token-2"), admin.tokens));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {403, 500, 503})
    void testOtherHttpFailuresDoNotInvalidateToken(final int status) throws Exception {
        try (TestAdmin admin = new TestAdmin();
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(admin.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                admin.heartbeatStatus.set(status);
                tick(heartbeat);
                tick(heartbeat);
                admin.heartbeatStatus.set(200);
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(1, admin.logins.get()),
                        () -> assertEquals(2, admin.accepted.get()),
                        () -> assertEquals(List.of("test-token-1", "test-token-1", "test-token-1", "test-token-1"), admin.tokens));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testUnauthorizedStatusIsNotHiddenBySuccessBody() throws Exception {
        try (TestAdmin admin = new TestAdmin();
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(admin.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                admin.heartbeatStatus.set(401);
                tick(heartbeat);
                admin.heartbeatStatus.set(200);
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(2, admin.logins.get()),
                        () -> assertEquals(2, admin.accepted.get()),
                        () -> assertEquals(List.of("test-token-1", "test-token-1", "test-token-2"), admin.tokens));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testApplicationFailureDoesNotTriggerReauthentication() throws Exception {
        try (TestAdmin admin = new TestAdmin();
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(admin.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                admin.heartbeatBody = "{\"code\":600,\"message\":\"application failure\"}";
                tick(heartbeat);
                tick(heartbeat);
                admin.heartbeatBody = Constants.SUCCESS;
                tick(heartbeat);
                assertEquals(1, admin.logins.get(), "Only HTTP 401 establishes authentication rejection");
                assertEquals(List.of("test-token-1", "test-token-1", "test-token-1", "test-token-1"), admin.tokens);
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testTokenRenewalIsIndependentForEachAdmin() throws Exception {
        try (TestAdmin first = new TestAdmin();
                TestAdmin second = new TestAdmin("other-token-");
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(first.address() + "," + second.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                first.rejectOriginalToken.set(true);
                tick(heartbeat);
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(2, first.logins.get()),
                        () -> assertEquals(2, first.accepted.get()),
                        () -> assertEquals(1, second.logins.get()),
                        () -> assertEquals(3, second.accepted.get()),
                        () -> assertEquals(List.of("other-token-1", "other-token-1", "other-token-1"), second.tokens));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testNetworkFailureRetainsTokenAndDoesNotSuppressOtherAdmins() throws Exception {
        try (TestAdmin first = new TestAdmin();
                TestAdmin second = new TestAdmin("other-token-");
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(first.address() + "," + second.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                first.dropHeartbeats.set(true);
                tick(heartbeat);
                tick(heartbeat);
                first.dropHeartbeats.set(false);
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(1, first.logins.get(), "A transport error must not invalidate the cached token"),
                        () -> assertEquals(2, first.accepted.get()),
                        () -> assertEquals(1, second.logins.get()),
                        () -> assertEquals(4, second.accepted.get()));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testFailedLoginsUseBoundedBackoffAndResetAfterRecovery() throws Exception {
        try (TestAdmin admin = new TestAdmin();
                TestAdmin healthy = new TestAdmin("healthy-token-");
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            admin.loginAllowed.set(false);
            HeartbeatListener listener = listener(admin.address() + "," + healthy.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                long[] attemptsAt = {0, 5, 15, 35, 75, 135, 195};
                for (int i = 0; i < attemptsAt.length; i++) {
                    elapsedNanos.set(TimeUnit.SECONDS.toNanos(attemptsAt[i]));
                    heartbeat.run();
                    assertEquals(i + 1, admin.logins.get());
                    elapsedNanos.set(TimeUnit.SECONDS.toNanos(attemptsAt[i] + 4));
                    heartbeat.run();
                    assertEquals(i + 1, admin.logins.get(), "Login attempts must be suppressed during backoff");
                }
                assertEquals(1, healthy.logins.get());
                assertEquals(14, healthy.accepted.get());
                admin.loginAllowed.set(true);
                elapsedNanos.set(TimeUnit.SECONDS.toNanos(255));
                heartbeat.run();
                assertEquals(8, admin.logins.get());
                assertEquals(1, admin.accepted.get());
                admin.heartbeatStatus.set(401);
                tick(heartbeat);
                admin.heartbeatStatus.set(200);
                tick(heartbeat);
                assertEquals(9, admin.logins.get(), "A successful heartbeat must reset the backoff to five seconds");
                assertEquals(2, admin.accepted.get());
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testUnavailableLoginBacksOffWithoutSuppressingOtherAdmins() throws Exception {
        try (TestAdmin unavailable = new TestAdmin();
                TestAdmin healthy = new TestAdmin("healthy-token-");
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            unavailable.dropLogins.set(true);
            HeartbeatListener listener = listener(unavailable.address() + "," + healthy.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                tick(heartbeat);
                tick(heartbeat);
                tick(heartbeat);
                assertEquals(2, unavailable.logins.get());
                assertEquals(3, healthy.accepted.get());
                unavailable.dropLogins.set(false);
                tick(heartbeat);
                assertAll(
                        () -> assertEquals(3, unavailable.logins.get()),
                        () -> assertEquals(1, unavailable.accepted.get()),
                        () -> assertEquals(1, healthy.logins.get()),
                        () -> assertEquals(4, healthy.accepted.get()));
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testRepeatedTokenRejectionsAlsoBackOff() throws Exception {
        try (TestAdmin admin = new TestAdmin();
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            admin.rejectAllTokens.set(true);
            HeartbeatListener listener = listener(admin.address());
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                heartbeat.run();
                tick(heartbeat);
                tick(heartbeat);
                assertEquals(2, admin.logins.get(), "A successful login alone must not reset repeated token rejections");
                tick(heartbeat);
                assertEquals(3, admin.logins.get());
                assertEquals(3, admin.rejected.get());
            } finally {
                listener.onShutdown();
            }
        }
    }

    @Test
    void testWorkerPoolRecoversOneAdminWithoutReloggingHealthyAdmin() throws Exception {
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try (TestAdmin first = new TestAdmin();
                TestAdmin healthy = new TestAdmin("healthy-token-");
                MockedConstruction<ScheduledThreadPoolExecutor> executors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            HeartbeatListener listener = listener(first.address() + "," + healthy.address(), workers, null);
            try {
                Runnable heartbeat = heartbeat(listener, executors);
                heartbeat.run();
                awaitWorkers(workers);
                first.rejectOriginalToken.set(true);
                tick(heartbeat);
                awaitWorkers(workers);
                tick(heartbeat);
                awaitWorkers(workers);
                assertAll(
                        () -> assertEquals(2, first.logins.get()),
                        () -> assertEquals(2, first.accepted.get()),
                        () -> assertEquals(1, first.rejected.get()),
                        () -> assertEquals(1, healthy.logins.get()),
                        () -> assertEquals(3, healthy.accepted.get()));
            } finally {
                listener.onShutdown();
            }
        } finally {
            workers.shutdownNow();
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {200, 401})
    void testLateResponseDoesNotUndoNewerRecovery(final int lateStatus) throws Exception {
        ExecutorService workers = Executors.newSingleThreadExecutor();
        CountDownLatch started = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        AtomicInteger logins = new AtomicInteger();
        HeartbeatListener.BeatSender sender = mock(HeartbeatListener.BeatSender.class);
        when(sender.login(anyString(), anyString(), anyString()))
                .thenAnswer(invocation -> Optional.of("test-token-" + logins.incrementAndGet()));
        doAnswer(invocation -> {
            int call = calls.incrementAndGet();
            if (call == 1) {
                started.countDown();
                assertTrue(release.await(5, TimeUnit.SECONDS));
                if (lateStatus == 401) {
                    throw new HttpResponseException(401);
                }
            } else if (call == 2 || call == 3 && lateStatus == 200) {
                throw new HttpResponseException(401);
            }
            return null;
        }).when(sender).doHeartBeat(anyString(), anyString(), anyString(), anyString());
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = mockConstruction(ScheduledThreadPoolExecutor.class)) {
            HeartbeatListener listener = listener("http://test-admin", null, sender);
            try {
                Method send = HeartbeatListener.class.getDeclaredMethod("sendHeartbeatToServer", String.class, InstanceBeatInfoDTO.class);
                send.setAccessible(true);
                InstanceBeatInfoDTO beat = new InstanceBeatInfoDTO();
                // Simulate a request that remains active after best-effort timeout cancellation.
                final Future<?> late = workers.submit(() -> send.invoke(listener, "http://test-admin", beat));
                assertTrue(started.await(5, TimeUnit.SECONDS));
                send.invoke(listener, "http://test-admin", beat);
                elapsedNanos.set(TimeUnit.SECONDS.toNanos(5));
                send.invoke(listener, "http://test-admin", beat);
                release.countDown();
                late.get(5, TimeUnit.SECONDS);
                send.invoke(listener, "http://test-admin", beat);
                assertEquals(2, logins.get(), "A stale response must not discard a newer token or reset a newer backoff");
                if (lateStatus == 200) {
                    assertEquals(3, calls.get(), "The newer rejection must retain its ten-second backoff");
                    elapsedNanos.set(TimeUnit.SECONDS.toNanos(15));
                    send.invoke(listener, "http://test-admin", beat);
                    assertEquals(3, logins.get());
                } else {
                    assertEquals(4, calls.get(), "The refreshed token should still be available after the stale rejection");
                }
                verify(sender, times(4)).doHeartBeat(anyString(), anyString(), anyString(), anyString());
            } finally {
                release.countDown();
                listener.onShutdown();
            }
        } finally {
            release.countDown();
            workers.shutdownNow();
        }
    }

    private void awaitWorkers(final ExecutorService workers) throws Exception {
        CountDownLatch reached = new CountDownLatch(2);
        Runnable barrier = () -> {
            reached.countDown();
            try {
                assertTrue(reached.await(5, TimeUnit.SECONDS));
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new AssertionError(e);
            }
        };
        Future<?> first = workers.submit(barrier);
        Future<?> second = workers.submit(barrier);
        first.get(5, TimeUnit.SECONDS);
        second.get(5, TimeUnit.SECONDS);
    }

    private void tick(final Runnable heartbeat) {
        elapsedNanos.addAndGet(TimeUnit.SECONDS.toNanos(5));
        heartbeat.run();
    }

    private HeartbeatListener listener(final String address) {
        return listener(address, null, null);
    }

    private HeartbeatListener listener(final String address, final ExecutorService workers, final HeartbeatListener.BeatSender sender) {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "test-user");
        props.setProperty(Constants.PASS_WORD, "test-only-password");
        props.setProperty("heartbeatDirectExecution", String.valueOf(Objects.isNull(workers)));
        props.setProperty("heartbeatTimeoutMs", "-1");
        ShenyuBootstrapHeartBeatConfig config = new ShenyuBootstrapHeartBeatConfig();
        config.setServerLists(address);
        config.setProps(props);
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("test-namespace");
        return new HeartbeatListener(config, shenyuConfig, workers, sender, elapsedNanos::get);
    }

    private Runnable heartbeat(final HeartbeatListener listener, final MockedConstruction<ScheduledThreadPoolExecutor> executors) {
        WebServer webServer = mock(WebServer.class);
        WebServerApplicationContext context = mock(WebServerApplicationContext.class);
        WebServerInitializedEvent event = mock(WebServerInitializedEvent.class);
        when(webServer.getPort()).thenReturn(8080);
        when(event.getWebServer()).thenReturn(webServer);
        when(event.getApplicationContext()).thenReturn(context);
        listener.onWebServerInitialized(event);
        ArgumentCaptor<Runnable> task = ArgumentCaptor.forClass(Runnable.class);
        ScheduledThreadPoolExecutor scheduler = executors.constructed().stream()
                .filter(executor -> mockingDetails(executor).getInvocations().stream()
                        .anyMatch(invocation -> "scheduleAtFixedRate".equals(invocation.getMethod().getName())))
                .findFirst().orElseThrow();
        verify(scheduler).scheduleAtFixedRate(task.capture(), eq(0L), eq(5L), eq(TimeUnit.SECONDS));
        return task.getValue();
    }

    private static final class TestAdmin implements AutoCloseable {

        private final HttpServer server;

        private final AtomicInteger logins = new AtomicInteger();

        private final AtomicInteger accepted = new AtomicInteger();

        private final AtomicInteger rejected = new AtomicInteger();

        private final AtomicBoolean rejectOriginalToken = new AtomicBoolean();

        private final AtomicBoolean rejectAllTokens = new AtomicBoolean();

        private final AtomicBoolean loginAllowed = new AtomicBoolean(true);

        private final AtomicBoolean dropHeartbeats = new AtomicBoolean();

        private final AtomicBoolean dropLogins = new AtomicBoolean();

        private final AtomicInteger heartbeatStatus = new AtomicInteger(200);

        private volatile String heartbeatBody = Constants.SUCCESS;

        private final List<String> tokens = new CopyOnWriteArrayList<>();

        private TestAdmin() throws IOException {
            this("test-token-");
        }

        private TestAdmin(final String tokenPrefix) throws IOException {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext(Constants.LOGIN_PATH, exchange -> {
                String token = tokenPrefix + logins.incrementAndGet();
                if (dropLogins.get()) {
                    exchange.getRequestBody().readAllBytes();
                    exchange.close();
                    return;
                }
                if (!loginAllowed.get()) {
                    respond(exchange, 200, "{\"code\":400,\"message\":\"invalid test credentials\"}");
                    return;
                }
                Map<String, Object> result = Map.of(Constants.ADMIN_RESULT_CODE, CommonErrorCode.SUCCESSFUL,
                        Constants.ADMIN_RESULT_DATA, Map.of(Constants.ADMIN_RESULT_TOKEN, token));
                respond(exchange, 200, GsonUtils.getInstance().toJson(result));
            });
            server.createContext(Constants.BEAT_URI_PATH, exchange -> {
                String token = exchange.getRequestHeaders().getFirst(Constants.X_ACCESS_TOKEN);
                tokens.add(token);
                if (dropHeartbeats.get()) {
                    exchange.getRequestBody().readAllBytes();
                    exchange.close();
                    return;
                }
                if (heartbeatStatus.get() != 200) {
                    respond(exchange, heartbeatStatus.get(), heartbeatBody);
                    return;
                }
                if (rejectAllTokens.get() || !(tokenPrefix + logins.get()).equals(token) || rejectOriginalToken.get() && (tokenPrefix + "1").equals(token)) {
                    rejected.incrementAndGet();
                    respond(exchange, 401, GsonUtils.getInstance().toJson(Map.of(Constants.ADMIN_RESULT_CODE,
                            CommonErrorCode.TOKEN_ERROR, "message", "token is error")));
                } else {
                    accepted.incrementAndGet();
                    respond(exchange, 200, heartbeatBody);
                }
            });
            server.start();
        }

        private String address() {
            return "http://127.0.0.1:" + server.getAddress().getPort();
        }

        private void respond(final HttpExchange exchange, final int status, final String body) throws IOException {
            try {
                exchange.getRequestBody().readAllBytes();
                byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", "application/json;charset=utf-8");
                exchange.sendResponseHeaders(status, bytes.length);
                exchange.getResponseBody().write(bytes);
            } finally {
                exchange.close();
            }
        }

        @Override
        public void close() {
            server.stop(0);
        }
    }
}
