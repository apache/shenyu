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

package org.apache.shenyu.register.client.http;

import okhttp3.Headers;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.http.utils.RuntimeUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.utils.OkHttpTools;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Boot-session behavior tests for {@link HttpClientRegisterRepository}.
 */
public final class HttpClientRegisterRepositoryFencingTest {

    private static final String SERVER_A = "http://localhost:9095";

    private static final String SERVER_B = "http://localhost:9096";

    private static final String DOMAIN_A = "domain-a";

    private static final String DOMAIN_B = "domain-b";

    private static final String SESSION = "b7c0a1d2-3e4f-4a5b-8c9d-0e1f2a3b4c5d";

    private static final String SESSION_B = "c8d1a2e3-4f5a-4b6c-9d0e-1f2a3b4c5d6e";

    private static final String TOKEN = "test-token";

    @Test
    public void registrationHeartbeatReadinessAndOfflineUseOneSessionAndOrderedSequences() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + Constants.URI_PATH), anyString(), any(Headers.class))).thenReturn(Constants.SUCCESS);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class))).thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo"));

            repository.doPersistURI(uri());
            repository.sendHeartbeat(uri());
            repository.notReady(uri());
            repository.ready(uri());
            repository.offline(uri());

            ArgumentCaptor<String> bodies = ArgumentCaptor.forClass(String.class);
            verify(http, times(5)).post(eq(SERVER_A + mutatePath()), bodies.capture(), any(Headers.class));
            List<String> values = bodies.getAllValues();
            assertMutation(values.get(0), RegistrationFencingProtocol.REGISTER, "1");
            assertMutation(values.get(1), RegistrationFencingProtocol.HEARTBEAT, "2");
            assertReadinessMutation(values.get(2), EventType.NOT_READY, "3");
            assertReadinessMutation(values.get(3), EventType.READY, "4");
            assertMutation(values.get(4), RegistrationFencingProtocol.OFFLINE, "5");
            verify(http, times(1)).post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH),
                    anyString(), any(Headers.class));
            verify(http, never()).post(eq(SERVER_A + Constants.URI_PATH), anyString(), any(Headers.class));
        }
    }

    @Test
    public void readinessRetryKeepsThePendingPayloadAndSequenceBeforeTheNextState() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + Constants.URI_PATH), anyString(), any(Headers.class))).thenReturn(Constants.SUCCESS);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class)))
                .thenReturn(okJson())
                .thenThrow(new IOException("down"))
                .thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo"));
            URIRegisterDTO dto = uri();

            repository.doPersistURI(dto);
            assertThrows(RuntimeException.class, () -> repository.notReady(dto));
            repository.ready(dto);

            ArgumentCaptor<String> bodies = ArgumentCaptor.forClass(String.class);
            verify(http, times(4)).post(eq(SERVER_A + mutatePath()), bodies.capture(), any(Headers.class));
            List<String> values = bodies.getAllValues();
            assertMutation(values.get(0), RegistrationFencingProtocol.REGISTER, "1");
            assertReadinessMutation(values.get(1), EventType.NOT_READY, "2");
            assertEquals(values.get(1), values.get(2));
            assertReadinessMutation(values.get(3), EventType.READY, "3");
            verify(http, times(1)).post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH),
                    anyString(), any(Headers.class));
            verify(http, never()).post(eq(SERVER_A + Constants.URI_PATH), anyString(), any(Headers.class));
        }
    }

    @Test
    public void staleReadinessSessionNeverFallsBackToLegacyRegistration() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + Constants.URI_PATH), anyString(), any(Headers.class))).thenReturn(Constants.SUCCESS);
        String stale = GsonUtils.getInstance().toJson(Map.of("code", RegistrationFencingProtocol.STALE_SESSION,
                "message", RegistrationFencingProtocol.STALE_SESSION));
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson()).thenReturn(stale);
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class)))
                .thenReturn(okJson()).thenReturn(stale);

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo"));

            repository.doPersistURI(uri());
            RuntimeException notReadyFailure = assertThrows(RuntimeException.class, () -> repository.notReady(uri()));
            assertTrue(notReadyFailure.getMessage().contains(RegistrationFencingProtocol.STALE_SESSION));
            RuntimeException readyFailure = assertThrows(RuntimeException.class, () -> repository.ready(uri()));
            assertTrue(readyFailure.getMessage().contains(RegistrationFencingProtocol.STALE_SESSION));

            ArgumentCaptor<String> mutations = ArgumentCaptor.forClass(String.class);
            verify(http, times(2)).post(eq(SERVER_A + mutatePath()), mutations.capture(), any(Headers.class));
            assertReadinessMutation(mutations.getAllValues().get(1), EventType.NOT_READY, "2");
            ArgumentCaptor<String> acquisitions = ArgumentCaptor.forClass(String.class);
            verify(http, times(2)).post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH),
                    acquisitions.capture(), any(Headers.class));
            Map<String, Object> initial = GsonUtils.getInstance().convertToMap(acquisitions.getAllValues().get(0));
            Map<String, Object> retry = GsonUtils.getInstance().convertToMap(acquisitions.getAllValues().get(1));
            assertEquals(initial.get("startupId"), retry.get("startupId"));
            assertEquals("1", retry.get("expectedGeneration"));
            verify(http, never()).post(eq(SERVER_A + Constants.URI_PATH), anyString(), any(Headers.class));
        }
    }

    @Test
    public void transportFailureRetriesTheSameSequenceWithoutReacquiring() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class)))
                .thenThrow(new IOException("down"))
                .thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo"));

            assertThrows(RuntimeException.class, () -> repository.doPersistURI(uri()));
            repository.doPersistURI(uri());

            ArgumentCaptor<String> bodies = ArgumentCaptor.forClass(String.class);
            verify(http, times(2)).post(eq(SERVER_A + mutatePath()), bodies.capture(), any(Headers.class));
            assertMutation(bodies.getAllValues().get(0), RegistrationFencingProtocol.REGISTER, "1");
            assertMutation(bodies.getAllValues().get(1), RegistrationFencingProtocol.REGISTER, "1");
        }
    }

    @Test
    public void partialDomainFailureKeepsTheCommittedDomainAndRetriesOnlyTheOther() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_B + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class))).thenReturn(okJson());
        when(http.post(eq(SERVER_B + mutatePath()), anyString(), any(Headers.class)))
                .thenThrow(new IOException("down"))
                .thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo;"
                            + DOMAIN_B + "|target-b|" + SERVER_B + "|||127.0.0.1|18080|http|ns-1|demo"));

            assertThrows(RuntimeException.class, () -> repository.doPersistURI(uri()));
            repository.doPersistURI(uri());

            verify(http, times(1)).post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class));
            ArgumentCaptor<String> bodies = ArgumentCaptor.forClass(String.class);
            verify(http, times(2)).post(eq(SERVER_B + mutatePath()), bodies.capture(), any(Headers.class));
            assertMutation(bodies.getAllValues().get(0), RegistrationFencingProtocol.REGISTER, "1");
            assertMutation(bodies.getAllValues().get(1), RegistrationFencingProtocol.REGISTER, "1");
        }
    }

    @Test
    public void operationUsesOnlyTheTargetWhoseAddressMatchesTheUri() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(anyString(), anyString(), any(Headers.class))).thenAnswer(invocation -> {
            String url = invocation.getArgument(0);
            String body = invocation.getArgument(1);
            if (url.endsWith(RegistrationFencingProtocol.ACQUIRE_PATH)) {
                Map<String, Object> request = GsonUtils.getInstance().convertToMap(body);
                return "target-a".equals(request.get("targetId"))
                        ? acquireJson(SESSION, "127.0.0.1", 18080, "http")
                        : acquireJson(SESSION_B, "127.0.0.2", 18081, "http");
            }
            return okJson();
        });

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    "authority|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo;"
                            + "authority|target-b|" + SERVER_A + "|||127.0.0.2|18081|http|ns-1|demo"));

            repository.doPersistURI(uri());

            verify(http, times(1)).post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class));
            verify(http, times(1)).post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class));
            ArgumentCaptor<String> mutationBody = ArgumentCaptor.forClass(String.class);
            verify(http).post(eq(SERVER_A + mutatePath()), mutationBody.capture(), any(Headers.class));
            assertMutation(mutationBody.getValue(), RegistrationFencingProtocol.REGISTER, "1");
        }
    }

    @Test
    public void operationDoesNotMatchAnotherServiceOnTheSameAddress() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class))).thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    "authority|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo;"
                            + "authority|target-b|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|another-service"));

            repository.doPersistURI(uri());

            verify(http, times(1)).post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class));
            verify(http, times(1)).post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class));
            verify(http, never()).post(eq(SERVER_A + RegistrationFencingProtocol.MUTATE_PATH.replace("{sessionId}", SESSION_B)),
                    anyString(), any(Headers.class));
        }
    }

    @Test
    public void pendingOperationKeepsAnImmutablePayloadAcrossRetry() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class)))
                .thenThrow(new IOException("down"))
                .thenReturn(okJson())
                .thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo"));
            URIRegisterDTO dto = uri();

            assertThrows(RuntimeException.class, () -> repository.doPersistURI(dto));
            dto.setEventType(org.apache.shenyu.register.common.enums.EventType.HEARTBEAT);
            repository.sendHeartbeat(dto);

            ArgumentCaptor<String> bodies = ArgumentCaptor.forClass(String.class);
            verify(http, times(3)).post(eq(SERVER_A + mutatePath()), bodies.capture(), any(Headers.class));
            assertMutation(bodies.getAllValues().get(0), RegistrationFencingProtocol.REGISTER, "1");
            assertMutation(bodies.getAllValues().get(1), RegistrationFencingProtocol.REGISTER, "1");
            assertMutation(bodies.getAllValues().get(2), RegistrationFencingProtocol.HEARTBEAT, "2");
            Map<String, Object> retry = GsonUtils.getInstance().convertToMap(bodies.getAllValues().get(1));
            Map<?, ?> retryPayload = (Map<?, ?>) retry.get("payload");
            org.junit.jupiter.api.Assertions.assertFalse(retryPayload.containsKey("eventType"));
        }
    }

    @Test
    public void fencedClientDoesNotSendSharedMetadata() throws IOException {
        final Timer timer = mock(Timer.class);
        final OkHttpTools http = mock(OkHttpTools.class);
        setupLogin(http);
        when(http.post(eq(SERVER_A + RegistrationFencingProtocol.ACQUIRE_PATH), anyString(), any(Headers.class)))
                .thenReturn(acquireJson());
        when(http.post(eq(SERVER_A + mutatePath()), anyString(), any(Headers.class))).thenReturn(okJson());

        try (MockedStatic<WheelTimerFactory> timers = mockStatic(WheelTimerFactory.class);
             MockedStatic<OkHttpTools> clients = mockStatic(OkHttpTools.class);
             MockedStatic<RuntimeUtils> runtime = mockStatic(RuntimeUtils.class)) {
            timers.when(WheelTimerFactory::getSharedTimer).thenReturn(timer);
            clients.when(OkHttpTools::getInstance).thenReturn(http);
            runtime.when(() -> RuntimeUtils.listenByOther(anyInt())).thenReturn(false);
            final HttpClientRegisterRepository repository = new HttpClientRegisterRepository(config(
                    DOMAIN_A + "|target-a|" + SERVER_A + "|||127.0.0.1|18080|http|ns-1|demo"));

            repository.persistInterface(MetaDataRegisterDTO.builder().path("/demo").build());

            verify(http, never()).post(eq(SERVER_A + Constants.META_PATH), anyString(), any(Headers.class));
        }
    }

    private void setupLogin(final OkHttpTools http) throws IOException {
        Map<String, Object> login = new HashMap<>();
        login.put(Constants.ADMIN_RESULT_CODE, 200);
        Map<String, Object> data = new HashMap<>();
        data.put(Constants.ADMIN_RESULT_TOKEN, TOKEN);
        login.put(Constants.ADMIN_RESULT_DATA, data);
        when(http.post(anyString(), anyString())).thenReturn(GsonUtils.getInstance().toJson(login));
    }

    private String acquireJson() {
        return acquireJson(SESSION, "127.0.0.1", 18080, "http");
    }

    private String acquireJson(final String sessionId, final String host, final int port, final String protocol) {
        Map<String, Object> data = new HashMap<>();
        data.put("sessionId", sessionId);
        data.put("namespaceId", "ns-1");
        data.put("selectorId", "selector-1");
        data.put("generation", "1");
        data.put("lastSequence", "0");
        data.put("host", host);
        data.put("port", port);
        data.put("protocol", protocol);
        Map<String, Object> response = new HashMap<>();
        response.put("code", RegistrationFencingProtocol.OK);
        response.put("message", RegistrationFencingProtocol.OK);
        response.put("data", data);
        return GsonUtils.getInstance().toJson(response);
    }

    private String okJson() {
        Map<String, Object> response = new HashMap<>();
        response.put("code", RegistrationFencingProtocol.OK);
        response.put("message", RegistrationFencingProtocol.OK);
        response.put("data", RegistrationFencingProtocol.OK);
        return GsonUtils.getInstance().toJson(response);
    }

    private String mutatePath() {
        return RegistrationFencingProtocol.MUTATE_PATH.replace("{sessionId}", SESSION);
    }

    private void assertMutation(final String body, final String operation, final String sequence) {
        Map<String, Object> request = GsonUtils.getInstance().convertToMap(body);
        assertEquals(operation, request.get("operation"));
        assertEquals(sequence, request.get("clientSequence"));
        assertEquals(SESSION, request.get("sessionId"));
    }

    private void assertReadinessMutation(final String body, final EventType eventType, final String sequence) {
        assertMutation(body, RegistrationFencingProtocol.UPDATE, sequence);
        Map<String, Object> request = GsonUtils.getInstance().convertToMap(body);
        Map<?, ?> payload = (Map<?, ?>) request.get("payload");
        assertEquals(eventType.name(), payload.get("eventType"));
    }

    private ShenyuRegisterCenterConfig config(final String domains) {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("registrationFencingDomains", domains);
        return new ShenyuRegisterCenterConfig("http", SERVER_A, props);
    }

    private URIRegisterDTO uri() {
        return URIRegisterDTO.builder()
                .appName("demo")
                .rpcType("http")
                .protocol("http")
                .host("127.0.0.1")
                .port(18080)
                .namespaceId("ns-1")
                .build();
    }
}
