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
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.mockingDetails;
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

    private void tick(final Runnable heartbeat) {
        elapsedNanos.addAndGet(TimeUnit.SECONDS.toNanos(5));
        heartbeat.run();
    }

    private HeartbeatListener listener(final String address) {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "test-user");
        props.setProperty(Constants.PASS_WORD, "test-only-password");
        ShenyuBootstrapHeartBeatConfig config = new ShenyuBootstrapHeartBeatConfig();
        config.setServerLists(address);
        config.setProps(props);
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("test-namespace");
        return new HeartbeatListener(config, shenyuConfig, elapsedNanos::get);
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
