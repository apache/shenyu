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

package org.apache.shenyu.springboot.starter.client.common.lifecycle;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.availability.AvailabilityChangeEvent;
import org.springframework.boot.availability.ReadinessState;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextClosedEvent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.same;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * The Spring readiness/shutdown bridge (issue #8429): ready/not-ready/ready sequences at
 * real transitions only, REFUSING_TRAFFIC never degrades into an offline, draining at
 * context close stops renewal first, and update failures never block shutdown.
 */
class ServingStateLifecycleBridgeTest {

    private ShenyuClientRegisterRepository repository;

    private AtomicInteger heartbeatStops;

    private List<URIRegisterDTO> registered;

    private ServingStateLifecycleBridge bridge;

    @BeforeEach
    public void setUp() {
        repository = mock(ShenyuClientRegisterRepository.class);
        heartbeatStops = new AtomicInteger();
        registered = List.of(uriDto("ns-1", "127.0.0.1", 9195), uriDto("ns-1", "127.0.0.2", 9196));
        bridge = new ServingStateLifecycleBridge(repository, () -> registered, heartbeatStops::incrementAndGet);
    }

    @Test
    void applicationReadyEmitsReadyForEveryRegisteredIdentity() {
        bridge.onApplicationReady(readyEvent());
        verify(repository).ready(same(registered.get(0)));
        verify(repository).ready(same(registered.get(1)));
        verifyNoMoreInteractions(repository);
    }

    @Test
    void refusingTrafficEmitsNotReadyAndNeverOffline() {
        bridge.onApplicationReady(readyEvent());
        bridge.onApplicationEvent(availabilityChange(ReadinessState.REFUSING_TRAFFIC));
        verify(repository).notReady(same(registered.get(0)));
        verify(repository).notReady(same(registered.get(1)));
        verify(repository, never()).offline(any());
    }

    @Test
    void acceptingTrafficAgainReleasesTheRefusal() {
        bridge.onApplicationReady(readyEvent());
        bridge.onApplicationEvent(availabilityChange(ReadinessState.REFUSING_TRAFFIC));
        bridge.onApplicationEvent(availabilityChange(ReadinessState.ACCEPTING_TRAFFIC));
        // two ready transitions (initial + recovery) across two URIs, one not-ready
        verify(repository, times(4)).ready(any());
        verify(repository, times(2)).notReady(any());
    }

    @Test
    void repeatedReadyEventsAreIdempotent() {
        bridge.onApplicationReady(readyEvent());
        bridge.onApplicationReady(readyEvent());
        bridge.onApplicationEvent(availabilityChange(ReadinessState.ACCEPTING_TRAFFIC));
        // exactly one ready transition across two URIs despite repeated events
        verify(repository, times(2)).ready(any());
        verify(repository, never()).notReady(any());
    }

    @Test
    void contextCloseStopsRenewalThenDrainsWhileTheJvmStaysAlive() {
        bridge.onApplicationReady(readyEvent());
        bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class)));
        assertThat(heartbeatStops.get()).isEqualTo(1);
        verify(repository).draining(same(registered.get(0)));
        verify(repository).draining(same(registered.get(1)));
        verify(repository, never()).offline(any());
        // draining is terminal: later readiness changes emit nothing (ready count stays at 2)
        bridge.onApplicationEvent(availabilityChange(ReadinessState.ACCEPTING_TRAFFIC));
        verify(repository, times(2)).ready(any());
    }

    @Test
    void repeatedContextCloseDoesNotDrainTwice() {
        bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class)));
        bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class)));
        // one draining transition across two URIs, renewal stopped once
        verify(repository, times(2)).draining(any());
        assertThat(heartbeatStops.get()).isEqualTo(1);
    }

    @Test
    void drainingFailureDoesNotBlockShutdownOrEmitOffline() {
        doThrow(new IllegalStateException("admin down")).when(repository).draining(any());
        bridge.onApplicationReady(readyEvent());
        bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class)));
        assertThat(heartbeatStops.get()).isEqualTo(1);
        verify(repository, never()).offline(any());
    }

    @Test
    void notReadyBeforeAnyReadinessEmitsNothing() {
        bridge.onApplicationEvent(availabilityChange(ReadinessState.REFUSING_TRAFFIC));
        verifyNoMoreInteractions(repository);
    }

    @Test
    void readyEmissionCompletesBeforeDrainStarts() throws Exception {
        CountDownLatch readyEntered = new CountDownLatch(1);
        CountDownLatch releaseReady = new CountDownLatch(1);
        List<String> order = Collections.synchronizedList(new ArrayList<>());
        doAnswer(invocation -> {
            readyEntered.countDown();
            releaseReady.await(5, TimeUnit.SECONDS);
            order.add("ready");
            return null;
        }).when(repository).ready(any());
        doAnswer(invocation -> {
            order.add("draining");
            return null;
        }).when(repository).draining(any());

        Thread readyThread = new Thread(() -> bridge.onApplicationReady(readyEvent()));
        readyThread.start();
        assertThat(readyEntered.await(5, TimeUnit.SECONDS)).isTrue();

        Thread closeThread = new Thread(() -> bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class))));
        closeThread.start();
        closeThread.join(300);
        assertThat(closeThread.isAlive()).as("drain must wait for the in-flight ready emission").isTrue();

        releaseReady.countDown();
        readyThread.join(5000);
        closeThread.join(5000);
        assertThat(order).containsExactly("ready", "ready", "draining", "draining");
    }

    @Test
    void heartbeatStopFailureStillDrains() {
        bridge = new ServingStateLifecycleBridge(repository, () -> registered, () -> {
            throw new IllegalStateException("stop failed");
        });
        bridge.onApplicationReady(readyEvent());
        assertThatCode(() -> bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class))))
                .doesNotThrowAnyException();
        verify(repository, times(2)).draining(any());
    }

    @Test
    void snapshotFailureStillStopsRenewal() {
        bridge = new ServingStateLifecycleBridge(repository, () -> {
            throw new IllegalStateException("snapshot failed");
        }, heartbeatStops::incrementAndGet);
        assertThatCode(() -> bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class))))
                .doesNotThrowAnyException();
        assertThat(heartbeatStops.get()).isEqualTo(1);
    }

    @Test
    void registrationAfterReadyReceivesTheRememberedState() {
        bridge.onApplicationReady(readyEvent());
        URIRegisterDTO late = uriDto("ns-1", "127.0.0.9", 9200);
        bridge.onRegistered(late);
        verify(repository).ready(same(late));
    }

    @Test
    void registrationWhileNotReadyReceivesTheRefusal() {
        bridge.onApplicationReady(readyEvent());
        bridge.onApplicationEvent(availabilityChange(ReadinessState.REFUSING_TRAFFIC));
        URIRegisterDTO late = uriDto("ns-1", "127.0.0.9", 9200);
        bridge.onRegistered(late);
        verify(repository).notReady(same(late));
    }

    @Test
    void registrationAfterDrainIsDrainedImmediately() {
        bridge.onApplicationReady(readyEvent());
        bridge.onContextClosed(new ContextClosedEvent(mock(ApplicationContext.class)));
        URIRegisterDTO late = uriDto("ns-1", "127.0.0.9", 9200);
        bridge.onRegistered(late);
        verify(repository).draining(same(late));
        verify(repository, never()).ready(same(late));
    }

    @Test
    void registrationBeforeAnyTransitionEmitsNothing() {
        URIRegisterDTO early = uriDto("ns-1", "127.0.0.9", 9200);
        bridge.onRegistered(early);
        verifyNoMoreInteractions(repository);
    }

    @Test
    void identityPassesThroughUnchanged() {
        bridge.onApplicationReady(readyEvent());
        verify(repository, times(2)).ready(Mockito.argThat(dto -> "ns-1".equals(dto.getNamespaceId())
                && "http".equals(dto.getProtocol()) && "/demo".equals(dto.getContextPath())));
    }

    @Test
    void optInConfigurationRegistersTheBridgeOnlyWhenEnabledAndRepositoryExists() {
        new org.springframework.boot.test.context.runner.ApplicationContextRunner()
                .withUserConfiguration(ServingStateLifecycleBridgeConfiguration.class)
                .withBean(ShenyuClientRegisterRepository.class, () -> mock(ShenyuClientRegisterRepository.class))
                .withPropertyValues("shenyu.register.lifecycle-bridge.enabled=true")
                .run(context -> assertThat(context).hasSingleBean(ServingStateLifecycleBridge.class));
        new org.springframework.boot.test.context.runner.ApplicationContextRunner()
                .withUserConfiguration(ServingStateLifecycleBridgeConfiguration.class)
                .withBean(ShenyuClientRegisterRepository.class, () -> mock(ShenyuClientRegisterRepository.class))
                .run(context -> assertThat(context).doesNotHaveBean(ServingStateLifecycleBridge.class));
        new org.springframework.boot.test.context.runner.ApplicationContextRunner()
                .withUserConfiguration(ServingStateLifecycleBridgeConfiguration.class)
                .withPropertyValues("shenyu.register.lifecycle-bridge.enabled=true")
                .run(context -> assertThat(context).doesNotHaveBean(ServingStateLifecycleBridge.class));
    }

    private ApplicationReadyEvent readyEvent() {
        return Mockito.mock(ApplicationReadyEvent.class);
    }

    private AvailabilityChangeEvent<?> availabilityChange(final ReadinessState state) {
        return new AvailabilityChangeEvent<>(new Object(), state);
    }

    private URIRegisterDTO uriDto(final String namespace, final String host, final int port) {
        URIRegisterDTO dto = new URIRegisterDTO();
        dto.setNamespaceId(namespace);
        dto.setProtocol("http");
        dto.setAppName("demo-app");
        dto.setContextPath("/demo");
        dto.setRpcType("http");
        dto.setHost(host);
        dto.setPort(port);
        return dto;
    }
}
