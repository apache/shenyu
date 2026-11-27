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
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.protocol.RegistrationLeaseProtocol;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.web.context.WebServerApplicationContext;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.lang.reflect.Method;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The lease-capable reporter: negotiates the server-issued interval, sends compact
 * renewals carrying the lease id, re-registers on a terminal lease response and falls
 * back to the legacy cadence for a server that answers the v1 marker. Time is a
 * controlled supplier, so no test sleeps.
 */
@ExtendWith(MockitoExtension.class)
class HeartbeatListenerLeaseTest {

    private static final String SERVER = "http://localhost:9095";

    private final AtomicLong nowNanos = new AtomicLong(0);

    private HeartbeatListener listener;

    private HeartbeatListener.BeatSender beatSender;

    private Method sendHeartbeat;

    @BeforeEach
    void setUp() throws Exception {
        sendHeartbeat = HeartbeatListener.class.getDeclaredMethod("sendHeartbeat", InstanceBeatInfoDTO.class);
        sendHeartbeat.setAccessible(true);
    }

    @AfterEach
    void tearDown() {
        if (Objects.nonNull(listener)) {
            listener.onShutdown();
        }
    }

    private HeartbeatListener.BeatSender newSender(final Function<String, String> responder) throws Exception {
        HeartbeatListener.BeatSender sender = mock(HeartbeatListener.BeatSender.class);
        when(sender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
        when(sender.doLeaseHeartBeat(anyString(), anyString(), anyString(), anyString()))
                .thenAnswer(invocation -> responder.apply(invocation.getArgument(0)));
        this.beatSender = sender;
        return sender;
    }

    private HeartbeatListener newListener(final HeartbeatListener.BeatSender sender) {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatDirectExecution", "true");
        props.setProperty("registrationLease", "true");
        ShenyuBootstrapHeartBeatConfig config = new ShenyuBootstrapHeartBeatConfig();
        config.setServerLists(SERVER);
        config.setProps(props);
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("shenyu");
        HeartbeatListener created = new HeartbeatListener(config, shenyuConfig, null, sender, nowNanos::get);
        bind(created, null, 8080);
        return created;
    }

    private void bind(final HeartbeatListener target, final String namespace, final int port) {
        WebServerInitializedEvent event = Mockito.mock(WebServerInitializedEvent.class);
        WebServer webServer = Mockito.mock(WebServer.class);
        WebServerApplicationContext applicationContext = Mockito.mock(WebServerApplicationContext.class);
        Mockito.when(event.getWebServer()).thenReturn(webServer);
        Mockito.when(event.getApplicationContext()).thenReturn(applicationContext);
        Mockito.when(webServer.getPort()).thenReturn(port);
        Mockito.when(applicationContext.getServerNamespace()).thenReturn(namespace);
        target.onWebServerInitialized(event);
    }

    private void tick() throws Exception {
        InstanceBeatInfoDTO beat = new InstanceBeatInfoDTO();
        beat.setInstanceInfo("{\"cpu\":8}");
        sendHeartbeat.invoke(listener, beat);
    }

    @Test
    void leaseDisabledKeepsTheExactLegacyWire() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class)) {
            HeartbeatListener.BeatSender sender = mock(HeartbeatListener.BeatSender.class);
            when(sender.login(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            listener = newListenerWithoutLease(sender);
            tick();

            verify(sender).doHeartBeat(anyString(), anyString(), anyString(), anyString());
            verify(sender, never()).doLeaseHeartBeat(anyString(), anyString(), anyString(), anyString());
        }
    }

    @Test
    void firstBeatCarriesFullObservationAndStoresTheGrantedLease() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class)) {
            ConcurrentHashMap<String, String> state = new ConcurrentHashMap<>();
            state.put("phase", "first");
            listener = newListener(newSender(json -> {
                if ("first".equals(state.get("phase"))) {
                    state.put("phase", "renewal");
                    return GsonUtils.getInstance().toJson(RegistrationLeaseProtocol.LeaseResponse.ok("L1", 10, 30_000L));
                }
                return GsonUtils.getInstance().toJson(RegistrationLeaseProtocol.LeaseResponse.ok("L1", 10, 40_000L));
            }));

            tick();

            ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
            verify(beatSenderCapture()).doLeaseHeartBeat(payload.capture(), anyString(), anyString(), anyString());
            assertFalse(payload.getValue().contains("leaseId"), "the first beat presents no lease id");
            assertTrue(payload.getValue().contains("instanceInfo"), "the first beat carries the full observation");

            // the server-issued interval of 10s gates the next tick at 5s
            nowNanos.addAndGet(5_000_000_000L);
            tick();
            verify(beatSenderCapture(), times(1)).doLeaseHeartBeat(anyString(), anyString(), anyString(), anyString());

            nowNanos.addAndGet(5_000_000_000L);
            tick();
            ArgumentCaptor<String> renewal = ArgumentCaptor.forClass(String.class);
            verify(beatSenderCapture(), times(2)).doLeaseHeartBeat(renewal.capture(), anyString(), anyString(), anyString());
            assertTrue(renewal.getValue().contains("\"leaseId\":\"L1\""), "renewals carry the lease id");
            assertFalse(renewal.getValue().contains("instanceInfo"), "renewals are compact");
        }
    }

    @Test
    void expiredLeaseDropsTheLeaseAndReregistersWithFullObservation() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class)) {
            listener = newListener(newSender(json -> {
                if (Objects.isNull(json) || !json.contains("L1")) {
                    return GsonUtils.getInstance().toJson(RegistrationLeaseProtocol.LeaseResponse.ok("L1", 5, 1L));
                }
                return GsonUtils.getInstance().toJson(RegistrationLeaseProtocol.LeaseResponse.error(
                        RegistrationLeaseProtocol.EXPIRED, "expired"));
            }));

            tick();
            nowNanos.addAndGet(5_000_000_000L);
            tick();
            nowNanos.addAndGet(1L);
            tick();

            ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
            verify(beatSenderCapture(), times(3)).doLeaseHeartBeat(payload.capture(), anyString(), anyString(), anyString());
            assertFalse(payload.getAllValues().get(2).contains("\"leaseId\":\"L1\""),
                    "after EXPIRED the client re-registers with a blank lease id");
            assertTrue(payload.getAllValues().get(2).contains("instanceInfo"),
                    "re-registration carries the full observation");
        }
    }

    @Test
    void legacyMarkerKeepsTheLegacyCadence() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class)) {
            listener = newListener(newSender(json -> Constants.SUCCESS));

            tick();
            // legacy cadence stays on the fixed 5 second period
            nowNanos.addAndGet(4_999_999_999L);
            tick();
            verify(beatSenderCapture(), times(1)).doLeaseHeartBeat(anyString(), anyString(), anyString(), anyString());
            nowNanos.addAndGet(1L);
            tick();
            verify(beatSenderCapture(), times(2)).doLeaseHeartBeat(anyString(), anyString(), anyString(), anyString());
        }
    }

    @Test
    void transportFailureDefersTheNextAttemptPastTheInterval() throws Exception {
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignored = Mockito.mockConstruction(ScheduledThreadPoolExecutor.class)) {
            listener = newListener(newSender(json -> {
                throw new IllegalStateException(new java.io.IOException("admin unavailable"));
            }));

            tick();
            nowNanos.addAndGet(1_000_000_000L);
            tick();
            verify(beatSenderCapture(), times(1)).doLeaseHeartBeat(anyString(), anyString(), anyString(), anyString());
        }
    }

    /**
     * The sender mock is recreated per test; this helper keeps the verifications short.
     *
     * @return the beat sender of the current listener under test
     */
    private HeartbeatListener.BeatSender beatSenderCapture() {
        return beatSender;
    }

    private HeartbeatListener newListenerWithoutLease(final HeartbeatListener.BeatSender sender) {
        this.beatSender = sender;
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        props.setProperty("heartbeatDirectExecution", "true");
        ShenyuBootstrapHeartBeatConfig config = new ShenyuBootstrapHeartBeatConfig();
        config.setServerLists(SERVER);
        config.setProps(props);
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("shenyu");
        HeartbeatListener created = new HeartbeatListener(config, shenyuConfig, null, sender, nowNanos::get);
        bind(created, null, 8080);
        return created;
    }
}
