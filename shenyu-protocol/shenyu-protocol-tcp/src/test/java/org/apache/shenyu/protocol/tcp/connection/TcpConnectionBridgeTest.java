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

package org.apache.shenyu.protocol.tcp.connection;

import io.netty.channel.Channel;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.reactivestreams.Publisher;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.netty.ByteBufFlux;
import reactor.netty.Connection;
import reactor.netty.NettyInbound;
import reactor.netty.NettyOutbound;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentCaptor.forClass;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link TcpConnectionBridge}.
 */
public final class TcpConnectionBridgeTest {

    private Connection server;

    private Connection client;

    private NettyInbound serverInbound;

    private NettyInbound clientInbound;

    private ByteBufFlux serverReceiveFlux;

    private ByteBufFlux clientReceiveFlux;

    private NettyOutbound serverOutbound;

    private NettyOutbound clientOutbound;

    private Channel serverChannel;

    private Channel clientChannel;

    @BeforeEach
    public void setUp() {
        server = mock(Connection.class);
        client = mock(Connection.class);
        serverInbound = mock(NettyInbound.class);
        clientInbound = mock(NettyInbound.class);
        serverOutbound = mock(NettyOutbound.class);
        clientOutbound = mock(NettyOutbound.class);
        serverReceiveFlux = mock(ByteBufFlux.class);
        clientReceiveFlux = mock(ByteBufFlux.class);
        serverChannel = mock(Channel.class);
        clientChannel = mock(Channel.class);

        when(serverReceiveFlux.retain()).thenReturn(serverReceiveFlux);
        when(clientReceiveFlux.retain()).thenReturn(clientReceiveFlux);
        when(server.inbound()).thenReturn(serverInbound);
        when(server.outbound()).thenReturn(serverOutbound);
        when(client.inbound()).thenReturn(clientInbound);
        when(client.outbound()).thenReturn(clientOutbound);
        when(serverInbound.receive()).thenReturn(serverReceiveFlux);
        when(clientInbound.receive()).thenReturn(clientReceiveFlux);
        doReturn(serverOutbound).when(serverOutbound).send(any(Publisher.class));
        doReturn(clientOutbound).when(clientOutbound).send(any(Publisher.class));
        doReturn(Mono.empty()).when(serverOutbound).then();
        doReturn(Mono.empty()).when(clientOutbound).then();
        when(server.channel()).thenReturn(serverChannel);
        when(client.channel()).thenReturn(clientChannel);
        when(server.onDispose(any(Disposable.class))).thenReturn(server);
        when(client.onDispose(any(Disposable.class))).thenReturn(client);
    }

    @Test
    public void bridgeShouldRelayTrafficInBothDirections() {
        TcpConnectionBridge bridge = new TcpConnectionBridge();

        bridge.bridge(server, client);

        verify(serverInbound).receive();
        verify(clientInbound).receive();
        verify(serverOutbound).send(any(Publisher.class));
        verify(clientOutbound).send(any(Publisher.class));
        verify(serverOutbound).then();
        verify(clientOutbound).then();
    }

    @Test
    public void disposingServerShouldCloseClientChannel() {
        ArgumentCaptor<Disposable> captor = forClass(Disposable.class);
        when(server.onDispose(captor.capture())).thenReturn(server);

        new TcpConnectionBridge().bridge(server, client);

        assertNotNull(captor.getValue());
        captor.getValue().dispose();
        verify(clientChannel).close();
    }

    @Test
    public void disposingClientShouldCloseServerChannel() {
        ArgumentCaptor<Disposable> captor = forClass(Disposable.class);
        when(client.onDispose(captor.capture())).thenReturn(client);

        new TcpConnectionBridge().bridge(server, client);

        captor.getValue().dispose();
        verify(serverChannel).close();
    }
}
