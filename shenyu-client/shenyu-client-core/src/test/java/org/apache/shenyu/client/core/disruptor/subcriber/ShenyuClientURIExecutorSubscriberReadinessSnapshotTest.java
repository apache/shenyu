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

package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The read-only registered-URI snapshot and the idempotent heartbeat stop, the seams a
 * lifecycle bridge uses to address exactly the identities the flow registered (#8429).
 */
class ShenyuClientURIExecutorSubscriberReadinessSnapshotTest {

    private ShenyuClientURIExecutorSubscriber subscriber;

    private ShenyuClientRegisterRepository repository;

    private ServerSocket socket;

    private ServerSocket secondSocket;

    @BeforeEach
    public void setUp() throws IOException {
        repository = Mockito.mock(ShenyuClientRegisterRepository.class);
        subscriber = new ShenyuClientURIExecutorSubscriber(repository, 1000L);
        socket = new ServerSocket(0);
        secondSocket = new ServerSocket(0);
    }

    @AfterEach
    public void tearDown() throws IOException {
        socket.close();
        secondSocket.close();
    }

    @Test
    void registeredUrisSnapshotMirrorsTheRegistrationFlow() {
        URIRegisterDTO first = uriDto("ns-1", socket.getLocalPort());
        URIRegisterDTO second = uriDto("ns-1", secondSocket.getLocalPort());
        subscriber.executor(List.of(first, second));
        List<URIRegisterDTO> snapshot = subscriber.registeredUris();
        assertEquals(2, snapshot.size());
        assertEquals(first, snapshot.get(0));
        assertEquals(second, snapshot.get(1));
    }

    @Test
    void snapshotIsUnmodifiableAndIndependentOfLaterRegistration() {
        URIRegisterDTO first = uriDto("ns-1", socket.getLocalPort());
        subscriber.executor(List.of(first));
        List<URIRegisterDTO> snapshot = subscriber.registeredUris();
        assertThrows(UnsupportedOperationException.class,
                () -> snapshot.add(uriDto("ns-1", socket.getLocalPort())));
        subscriber.executor(List.of(uriDto("ns-1", socket.getLocalPort())));
        assertEquals(1, snapshot.size(), "a taken snapshot is not mutated by later registrations");
    }

    @Test
    void emptyBeforeAnyRegistration() {
        assertTrue(subscriber.registeredUris().isEmpty());
    }

    @Test
    void snapshotIsDetachedFromHeartbeatPayloads() {
        URIRegisterDTO first = uriDto("ns-1", socket.getLocalPort());
        subscriber.executor(List.of(first));
        List<URIRegisterDTO> snapshot = subscriber.registeredUris();
        assertNotSame(first, snapshot.get(0), "the snapshot must not share the heartbeat payload instance");
        snapshot.get(0).setEventType(org.apache.shenyu.register.common.enums.EventType.DRAINING);
        assertEquals(org.apache.shenyu.register.common.enums.EventType.REGISTER, first.getEventType(),
                "mutating a snapshot entry must not rewrite the heartbeat payload");
        assertEquals(org.apache.shenyu.register.common.enums.EventType.REGISTER,
                subscriber.registeredUris().get(0).getEventType());
    }

    @Test
    void registrationListenersReceiveDetachedCopyOnFirstRegistrationOnly() {
        AtomicInteger notified = new AtomicInteger();
        AtomicReference<URIRegisterDTO> received = new AtomicReference<>();
        subscriber.addRegistrationListener(uri -> {
            notified.incrementAndGet();
            received.set(uri);
        });
        URIRegisterDTO first = uriDto("ns-1", socket.getLocalPort());
        subscriber.executor(List.of(first));
        assertEquals(1, notified.get());
        assertNotSame(first, received.get(), "listeners must not receive the heartbeat payload instance");
        subscriber.executor(List.of(first));
        assertEquals(1, notified.get(), "re-registering the same identity must not notify again");
    }

    @Test
    void failingRegistrationListenerDoesNotBreakRegistration() {
        subscriber.addRegistrationListener(uri -> {
            throw new IllegalStateException("listener failed");
        });
        URIRegisterDTO first = uriDto("ns-1", socket.getLocalPort());
        assertDoesNotThrow(() -> subscriber.executor(List.of(first)));
        assertEquals(1, subscriber.registeredUris().size());
    }

    @Test
    void heartbeatStopIsIdempotent() {
        subscriber.start();
        subscriber.shutdown();
        assertTrue(subscriber.isHeartbeatShutdown());
        subscriber.shutdown();
        assertDoesNotThrow(subscriber::start, "starting after shutdown must not schedule on a stopped executor");
    }

    @Test
    void startingAfterStopDoesNotScheduleOnAStoppedExecutor() {
        subscriber.shutdown();
        assertDoesNotThrow(subscriber::start);
    }

    private URIRegisterDTO uriDto(final String namespace, final int port) {
        URIRegisterDTO dto = new URIRegisterDTO();
        dto.setNamespaceId(namespace);
        dto.setProtocol("http");
        dto.setAppName("demo-app");
        dto.setContextPath("/demo");
        dto.setRpcType("http");
        dto.setHost("127.0.0.1");
        dto.setPort(port);
        dto.setEventType(org.apache.shenyu.register.common.enums.EventType.REGISTER);
        return dto;
    }
}
