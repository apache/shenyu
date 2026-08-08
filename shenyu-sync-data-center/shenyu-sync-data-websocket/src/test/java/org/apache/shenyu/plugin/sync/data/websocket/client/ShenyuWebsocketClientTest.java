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

package org.apache.shenyu.plugin.sync.data.websocket.client;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.mockito.Answers;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.withSettings;

/**
 * add test case for {@link ShenyuWebsocketClient}.
 */
@ExtendWith(MockitoExtension.class)
public class ShenyuWebsocketClientTest {

    @InjectMocks
    private ShenyuWebsocketClient shenyuWebsocketClient;

    @Mock
    private URI serverUri;

    @Mock
    private PluginDataSubscriber pluginDataSubscriber;

    @Mock
    private List<MetaDataSubscriber> metaDataSubscribers;

    @Mock
    private List<AuthDataSubscriber> authDataSubscribers;

    @Mock
    private ScheduledThreadPoolExecutor executor;

    private WebsocketData<PluginData> websocketData;

    @BeforeEach
    public void setUp() {
        websocketData = new WebsocketData<>();
        websocketData.setEventType(DataEventTypeEnum.MYSELF.name());
        websocketData.setGroupType(ConfigGroupEnum.PLUGIN.name());
        List<PluginData> list = new ArrayList<>(1);
        PluginData pluginData = PluginData.builder().enabled(true).name("shenyu-plugin-grpc")
                .id("shenyu-plugin-grpc").role("admin").build();
        list.add(pluginData);
        websocketData.setData(list);
    }

    @Test
    public void testOnOpen() {
        shenyuWebsocketClient = spy(shenyuWebsocketClient);
        ServerHandshake serverHandshake = mock(ServerHandshake.class);
        doNothing().when(shenyuWebsocketClient).send(DataEventTypeEnum.RUNNING_MODE.name());
        doNothing().when(shenyuWebsocketClient).send(DataEventTypeEnum.MYSELF.name());
        shenyuWebsocketClient.onOpen(serverHandshake);
        verify(shenyuWebsocketClient).send(DataEventTypeEnum.RUNNING_MODE.name());
        verify(shenyuWebsocketClient).send(DataEventTypeEnum.MYSELF.name());
    }

    @Test
    public void testOnMessage() {
        doNothing().when(pluginDataSubscriber).onSubscribe(any());
        String json = GsonUtils.getInstance().toJson(websocketData);
        shenyuWebsocketClient.onMessage(json);
        verify(pluginDataSubscriber).onSubscribe(any());
    }

    @Test
    public void testOnClose() {
        shenyuWebsocketClient = spy(shenyuWebsocketClient);
        doNothing().when(shenyuWebsocketClient).close();
        shenyuWebsocketClient.onClose(1, "shenyu-plugin-grpc", true);
        verify(shenyuWebsocketClient).close();
    }

    @Test
    public void testOnError() {
        shenyuWebsocketClient = spy(shenyuWebsocketClient);
        Assertions.assertDoesNotThrow(() -> shenyuWebsocketClient.onError(new ShenyuException("test")));
    }

    // ========== reconnect/backoff tests ==========

    private ShenyuWebsocketClient createMockClient() {
        ShenyuWebsocketClient client = mock(ShenyuWebsocketClient.class,
                withSettings().defaultAnswer(Answers.CALLS_REAL_METHODS));
        setField(client, "reconnecting", new AtomicBoolean(false));
        setField(client, "reconnectBackoff", new AtomicInteger(0));
        setField(client, "lastReconnectAttemptTime", 0L);
        return client;
    }

    private void setField(final Object target, final String name, final Object value) {
        try {
            Field field = ShenyuWebsocketClient.class.getDeclaredField(name);
            field.setAccessible(true);
            field.set(target, value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private Object getField(final Object target, final String name) {
        try {
            Field field = ShenyuWebsocketClient.class.getDeclaredField(name);
            field.setAccessible(true);
            return field.get(target);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private Object invokePrivate(final Object target, final String methodName) {
        try {
            Method method = ShenyuWebsocketClient.class.getDeclaredMethod(methodName);
            method.setAccessible(true);
            return method.invoke(target);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof RuntimeException) {
                throw (RuntimeException) cause;
            }
            throw new RuntimeException(cause);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    // ---------- calculateBackoff tests ----------

    @Test
    void testCalculateBackoffReturnsZeroForNoFailures() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(0));

        long backoff = (long) invokePrivate(client, "calculateBackoff");

        assertEquals(0, backoff);
    }

    @Test
    void testCalculateBackoffExponentialGrowth() {
        ShenyuWebsocketClient client = createMockClient();

        setField(client, "reconnectBackoff", new AtomicInteger(1));
        long backoff1 = (long) invokePrivate(client, "calculateBackoff");
        assertTrue(backoff1 >= 1000 && backoff1 <= 1500,
                () -> "Expected [1000, 1500] but got " + backoff1);

        setField(client, "reconnectBackoff", new AtomicInteger(2));
        long backoff2 = (long) invokePrivate(client, "calculateBackoff");
        assertTrue(backoff2 >= 2000 && backoff2 <= 3000,
                () -> "Expected [2000, 3000] but got " + backoff2);

        setField(client, "reconnectBackoff", new AtomicInteger(4));
        long backoff4 = (long) invokePrivate(client, "calculateBackoff");
        assertTrue(backoff4 >= 8000 && backoff4 <= 12000,
                () -> "Expected [8000, 12000] but got " + backoff4);
    }

    @Test
    void testCalculateBackoffMaxCap() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(10));

        long backoff = (long) invokePrivate(client, "calculateBackoff");

        assertTrue(backoff >= 60000 && backoff <= 90000,
                () -> "Expected [60000, 90000] but got " + backoff);
    }

    @Test
    void testCalculateBackoffIncludesJitter() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(1));
        boolean varied = false;
        long first = (long) invokePrivate(client, "calculateBackoff");
        for (int i = 0; i < 20; i++) {
            if ((long) invokePrivate(client, "calculateBackoff") != first) {
                varied = true;
                break;
            }
        }
        assertTrue(varied, "Backoff should vary due to jitter");
    }

    // ---------- healthCheck tests ----------

    @Test
    void testHealthCheckDoesNotDoubleSubmitWhenAlreadyReconnecting() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnecting", new AtomicBoolean(true));
        doReturn(false).when(client).isOpen();

        invokePrivate(client, "healthCheck");

        assertTrue(((AtomicBoolean) getField(client, "reconnecting")).get());
        verify(client).isOpen();
    }

    @Test
    void testHealthCheckResetsBackoffAndSendsPingWhenOpen() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(5));
        doReturn(true).when(client).isOpen();
        doNothing().when(client).sendPing();
        doNothing().when(client).send(anyString());
        doReturn(URI.create("ws://localhost:9090")).when(client).getURI();

        invokePrivate(client, "healthCheck");

        assertEquals(0, ((AtomicInteger) getField(client, "reconnectBackoff")).get());
        verify(client).sendPing();
    }

    // ---------- doReconnect tests ----------
    // reconnectBlocking() is NOT stubbed — the real parent method throws naturally
    // because the mock has no valid socket connection, so the failure path is tested.

    @Test
    void testDoReconnectIncrementsBackoffOnFailure() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(0));
        doReturn(URI.create("ws://localhost:9090")).when(client).getURI();

        invokePrivate(client, "doReconnect");

        assertEquals(1, ((AtomicInteger) getField(client, "reconnectBackoff")).get());
        assertFalse(((AtomicBoolean) getField(client, "reconnecting")).get());
    }

    @Test
    void testDoReconnectBackoffCappedAtTen() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(10));
        doReturn(URI.create("ws://localhost:9090")).when(client).getURI();

        invokePrivate(client, "doReconnect");

        assertEquals(10, ((AtomicInteger) getField(client, "reconnectBackoff")).get());
    }

    @Test
    void testDoReconnectResetsReconnectingOnFailure() {
        ShenyuWebsocketClient client = createMockClient();
        doReturn(URI.create("ws://localhost:9090")).when(client).getURI();

        invokePrivate(client, "doReconnect");

        assertFalse(((AtomicBoolean) getField(client, "reconnecting")).get());
    }

    @Test
    void testDoReconnectAppliesBackoffSleep() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(1));
        setField(client, "lastReconnectAttemptTime", System.currentTimeMillis());
        doReturn(URI.create("ws://localhost:9090")).when(client).getURI();

        long start = System.currentTimeMillis();
        invokePrivate(client, "doReconnect");
        long elapsed = System.currentTimeMillis() - start;

        assertTrue(elapsed >= 800,
                () -> "Expected >= 800ms backoff sleep, got " + elapsed + "ms");
    }

    @Test
    void testDoReconnectPreservesInterruptStatus() {
        ShenyuWebsocketClient client = createMockClient();
        setField(client, "reconnectBackoff", new AtomicInteger(1));
        setField(client, "lastReconnectAttemptTime", System.currentTimeMillis());

        Thread.currentThread().interrupt();
        invokePrivate(client, "doReconnect");

        assertTrue(Thread.interrupted(), "Interrupt status should be preserved after reconnect");
        assertFalse(((AtomicBoolean) getField(client, "reconnecting")).get());
    }
}
