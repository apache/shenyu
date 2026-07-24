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

package org.apache.shenyu.protocol.tcp;

import com.google.common.eventbus.EventBus;
import io.netty.channel.Channel;
import org.apache.shenyu.protocol.tcp.connection.ActivityConnectionObserver;
import org.apache.shenyu.protocol.tcp.connection.Bridge;
import org.apache.shenyu.protocol.tcp.connection.ConnectionContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class TcpBootstrapServerTest {

    @Mock
    private EventBus eventBus;

    @Mock
    private Bridge bridge;

    @Mock
    private ConnectionContext connectionContext;

    @Mock
    private Connection serverConn;

    @Mock
    private Connection clientConn;

    @Mock
    private Channel channel;

    private TcpBootstrapServer server;

    @BeforeEach
    void setUp() throws Exception {
        server = new TcpBootstrapServer(eventBus);
        setField("bridge", bridge);
        setField("connectionContext", connectionContext);
    }

    @Test
    void shouldUnregisterObserverWhenServerConnectionIsDisposed() throws Exception {
        SocketAddress remoteAddr = new InetSocketAddress("127.0.0.1", 8080);
        when(serverConn.channel()).thenReturn(channel);
        when(channel.remoteAddress()).thenReturn(remoteAddr);

        ArgumentCaptor<Disposable> onDisposeCaptor = ArgumentCaptor.forClass(Disposable.class);
        when(serverConn.onDispose(onDisposeCaptor.capture())).thenReturn(serverConn);

        when(connectionContext.getTcpClientConnection(eq("127.0.0.1"), any(ActivityConnectionObserver.class)))
                .thenReturn(Mono.just(clientConn));

        invokeBridgeConnections(serverConn);

        verify(eventBus).register(any(ActivityConnectionObserver.class));

        onDisposeCaptor.getValue().dispose();

        verify(eventBus).unregister(any(ActivityConnectionObserver.class));
    }

    @Test
    void shouldUnregisterObserverAndDisposeServerConnOnClientConnectionError() throws Exception {
        SocketAddress remoteAddr = new InetSocketAddress("127.0.0.1", 8080);
        when(serverConn.channel()).thenReturn(channel);
        when(channel.remoteAddress()).thenReturn(remoteAddr);

        when(serverConn.onDispose(any(Disposable.class))).thenReturn(serverConn);

        RuntimeException exception = new RuntimeException("Connection refused");
        when(connectionContext.getTcpClientConnection(eq("127.0.0.1"), any(ActivityConnectionObserver.class)))
                .thenReturn(Mono.error(exception));

        invokeBridgeConnections(serverConn);

        verify(eventBus).register(any(ActivityConnectionObserver.class));
        verify(eventBus).unregister(any(ActivityConnectionObserver.class));
        verify(serverConn).dispose();
        verify(bridge, never()).bridge(any(Connection.class), any(Connection.class));
    }

    @Test
    void shouldBridgeConnectionsOnSuccessfulClientConnection() throws Exception {
        SocketAddress remoteAddr = new InetSocketAddress("127.0.0.1", 9090);
        when(serverConn.channel()).thenReturn(channel);
        when(channel.remoteAddress()).thenReturn(remoteAddr);

        when(serverConn.onDispose(any(Disposable.class))).thenReturn(serverConn);

        when(connectionContext.getTcpClientConnection(eq("127.0.0.1"), any(ActivityConnectionObserver.class)))
                .thenReturn(Mono.just(clientConn));

        invokeBridgeConnections(serverConn);

        verify(bridge).bridge(serverConn, clientConn);
    }

    @Test
    void shouldUseSameObserverForEventBusAndConnectionContext() throws Exception {
        SocketAddress remoteAddr = new InetSocketAddress("127.0.0.1", 8080);
        when(serverConn.channel()).thenReturn(channel);
        when(channel.remoteAddress()).thenReturn(remoteAddr);

        when(serverConn.onDispose(any(Disposable.class))).thenReturn(serverConn);

        ArgumentCaptor<ActivityConnectionObserver> registerCaptor =
                ArgumentCaptor.forClass(ActivityConnectionObserver.class);
        ArgumentCaptor<ActivityConnectionObserver> contextCaptor =
                ArgumentCaptor.forClass(ActivityConnectionObserver.class);

        when(connectionContext.getTcpClientConnection(eq("127.0.0.1"), contextCaptor.capture()))
                .thenReturn(Mono.just(clientConn));

        invokeBridgeConnections(serverConn);

        verify(eventBus).register(registerCaptor.capture());
        assertNotNull(registerCaptor.getValue());
        assertNotNull(contextCaptor.getValue());
    }

    private void invokeBridgeConnections(final Connection connection) throws Exception {
        Method method = TcpBootstrapServer.class.getDeclaredMethod("bridgeConnections", Connection.class);
        method.setAccessible(true);
        method.invoke(server, connection);
    }

    private void setField(final String name, final Object value) throws Exception {
        Field field = TcpBootstrapServer.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(server, value);
    }
}
