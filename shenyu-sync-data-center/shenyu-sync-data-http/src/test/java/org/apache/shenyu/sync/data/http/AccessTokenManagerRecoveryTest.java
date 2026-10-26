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

package org.apache.shenyu.sync.data.http;

import com.github.tomakehurst.wiremock.WireMockServer;
import okhttp3.OkHttpClient;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.postRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.verify;

class AccessTokenManagerRecoveryTest {

    private static final String PRIMARY = "/first";

    private static final String SECONDARY = "/second";

    private WireMockServer server;

    private OkHttpClient client;

    @BeforeEach
    void setUp() {
        server = new WireMockServer(options().dynamicPort());
        server.start();
        client = new OkHttpClient.Builder().connectTimeout(Duration.ofSeconds(3)).readTimeout(Duration.ofSeconds(3)).build();
    }

    @AfterEach
    void tearDown() {
        client.connectionPool().evictAll();
        client.dispatcher().executorService().shutdownNow();
        server.stop();
    }

    @ParameterizedTest
    @ValueSource(strings = {"<html>temporarily unavailable</html>", "{broken-json", "[]", "{\"code\":200,\"data\":[]}"})
    void malformedFirstAdminDoesNotPreventInitialFailover(final String malformed) {
        stub(PRIMARY, malformed);
        stub(SECONDARY, success("secondary-token"));
        try (MockedConstruction<ScheduledThreadPoolExecutor> schedules = mockConstruction(ScheduledThreadPoolExecutor.class)) {
            AccessTokenManager manager = new AccessTokenManager(client, config());

            assertEquals("secondary-token", manager.getAccessToken());
            refreshTask(schedules);
            server.verify(1, postRequestedFor(urlPathEqualTo(PRIMARY + Constants.LOGIN_PATH)));
            server.verify(1, postRequestedFor(urlPathEqualTo(SECONDARY + Constants.LOGIN_PATH)));
        }
    }

    @Test
    void allMalformedInitialResponsesStillScheduleLaterRecovery() {
        stub(PRIMARY, "{broken-json");
        stub(SECONDARY, "[]");
        try (MockedConstruction<ScheduledThreadPoolExecutor> schedules = mockConstruction(ScheduledThreadPoolExecutor.class)) {
            AccessTokenManager manager = new AccessTokenManager(client, config());
            assertNull(manager.getAccessToken());
            Runnable refresh = refreshTask(schedules);

            stub(PRIMARY, success("recovered-token"));
            refresh.run();

            assertEquals("recovered-token", manager.getAccessToken());
            server.verify(2, postRequestedFor(urlPathEqualTo(PRIMARY + Constants.LOGIN_PATH)));
            server.verify(1, postRequestedFor(urlPathEqualTo(SECONDARY + Constants.LOGIN_PATH)));
        }
    }

    @Test
    void periodicRefreshContinuesToTheHealthyAdminAfterMalformedResponse() {
        stub(PRIMARY, success("initial-token"));
        stub(SECONDARY, success("replacement-token"));
        try (MockedConstruction<ScheduledThreadPoolExecutor> schedules = mockConstruction(ScheduledThreadPoolExecutor.class)) {
            AccessTokenManager manager = new AccessTokenManager(client, config());
            assertEquals("initial-token", manager.getAccessToken());
            Runnable refresh = refreshTask(schedules);
            ReflectionTestUtils.setField(manager, "lastRefreshTime", 0L);

            stub(PRIMARY, "{broken-json");
            refresh.run();

            assertEquals("replacement-token", manager.getAccessToken());
            server.verify(2, postRequestedFor(urlPathEqualTo(PRIMARY + Constants.LOGIN_PATH)));
            server.verify(1, postRequestedFor(urlPathEqualTo(SECONDARY + Constants.LOGIN_PATH)));
        }
    }

    @Test
    void failedRefreshPreservesTheLastTokenAndTheNextIntervalCanRecover() {
        stub(PRIMARY, success("last-good-token"));
        stub(SECONDARY, "[]");
        try (MockedConstruction<ScheduledThreadPoolExecutor> schedules = mockConstruction(ScheduledThreadPoolExecutor.class)) {
            AccessTokenManager manager = new AccessTokenManager(client, config());
            Runnable refresh = refreshTask(schedules);
            ReflectionTestUtils.setField(manager, "lastRefreshTime", 0L);

            stub(PRIMARY, "{broken-json");
            refresh.run();
            assertEquals("last-good-token", manager.getAccessToken());

            stub(PRIMARY, success("recovered-token"));
            refresh.run();
            assertEquals("recovered-token", manager.getAccessToken());
        }
    }

    private HttpConfig config() {
        HttpConfig config = new HttpConfig();
        config.setUrl(server.baseUrl() + PRIMARY + "," + server.baseUrl() + SECONDARY);
        config.setUsername("test-admin");
        config.setPassword("test-password");
        return config;
    }

    private void stub(final String path, final String response) {
        server.stubFor(post(urlPathEqualTo(path + Constants.LOGIN_PATH))
                .willReturn(aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(response)));
    }

    private String success(final String token) {
        return GsonUtils.getInstance().toJson(Map.of(Constants.ADMIN_RESULT_CODE, CommonErrorCode.SUCCESSFUL,
                Constants.ADMIN_RESULT_DATA, Map.of(Constants.ADMIN_RESULT_TOKEN, token, Constants.ADMIN_RESULT_EXPIRED_TIME, 3600000L)));
    }

    private Runnable refreshTask(final MockedConstruction<ScheduledThreadPoolExecutor> schedules) {
        assertEquals(1, schedules.constructed().size());
        ArgumentCaptor<Runnable> task = ArgumentCaptor.forClass(Runnable.class);
        verify(schedules.constructed().get(0)).scheduleWithFixedDelay(task.capture(), eq(5000L), eq(5000L), eq(TimeUnit.MILLISECONDS));
        return task.getValue();
    }
}
