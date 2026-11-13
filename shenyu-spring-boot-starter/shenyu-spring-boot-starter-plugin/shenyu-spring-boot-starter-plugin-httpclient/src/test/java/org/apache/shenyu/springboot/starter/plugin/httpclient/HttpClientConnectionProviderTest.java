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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;

import java.time.Duration;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class HttpClientConnectionProviderTest {

    @Test
    void requestCancellationIsIsolatedAcrossConnectorSubscriptions() {
        ConnectionProvider delegate = mock(ConnectionProvider.class);
        AtomicInteger cancelled = new AtomicInteger();
        when(delegate.acquire(null, null, null, null)).thenAnswer(invocation -> Mono.<Connection>never().doOnCancel(cancelled::incrementAndGet));
        HttpClientConnectionProvider provider = new HttpClientConnectionProvider(delegate, false);
        List<Disposable> acquisitions = new ArrayList<>();
        // Model a connector that subscribes to acquisition separately, as Reactor Netty does.
        Mono<Connection> connector = Mono.create(sink -> acquisitions.add(provider.acquire(null, null, null, null)
                .contextWrite(sink.contextView()).subscribe(sink::success, sink::error)));
        Mono<? extends Connection> cancellable = HttpClientConnectionProvider.propagateCancellation(connector);
        CompletableFuture<? extends Connection> first = cancellable.toFuture();
        CompletableFuture<? extends Connection> second = cancellable.toFuture();
        try {
            assertEquals(2, acquisitions.size());
            assertTrue(first.cancel(true));
            assertEquals(1, cancelled.get());
            assertFalse(second.isDone());
            assertTrue(second.cancel(true));
            assertEquals(2, cancelled.get());
        } finally {
            first.cancel(true);
            second.cancel(true);
            acquisitions.forEach(Disposable::dispose);
        }
    }

    @Test
    void acquisitionWithoutRequestContextPreservesDelegateFailure() {
        ConnectionProvider delegate = mock(ConnectionProvider.class);
        IllegalStateException failure = new IllegalStateException("pool disposed");
        when(delegate.acquire(null, null, null, null)).thenReturn(Mono.error(failure));
        HttpClientConnectionProvider provider = new HttpClientConnectionProvider(delegate, false);

        assertSame(failure, assertThrows(IllegalStateException.class, () -> provider.acquire(null, null, null, null).block(Duration.ofSeconds(5))));
    }

    @Test
    void poolInspectionAndMutationRetainDelegateConfiguration() {
        ConnectionProvider delegate = ConnectionProvider.builder("inspection").maxConnections(3).build();
        HttpClientConnectionProvider provider = new HttpClientConnectionProvider(delegate, false);
        ConnectionProvider mutated = provider.mutate().maxConnections(2).build();
        try {
            assertEquals("inspection", provider.name());
            assertEquals(3, provider.maxConnections());
            assertEquals(delegate.maxConnectionsPerHost(), provider.maxConnectionsPerHost());
            assertEquals(2, mutated.maxConnections());
            assertFalse(mutated instanceof HttpClientConnectionProvider);
        } finally {
            provider.dispose();
            mutated.dispose();
        }
        assertTrue(provider.isDisposed());
    }

    @Test
    void remoteDisposalDoesNotDisposeTheWholeProvider() {
        ConnectionProvider delegate = mock(ConnectionProvider.class);
        InetSocketAddress retired = new InetSocketAddress("127.0.0.1", 9195);
        InetSocketAddress retained = new InetSocketAddress("127.0.0.1", 9196);
        when(delegate.maxConnectionsPerHost()).thenReturn(Map.of(retained, 3));
        HttpClientConnectionProvider provider = new HttpClientConnectionProvider(delegate, false);

        provider.disposeWhen(retired);

        org.mockito.Mockito.verify(delegate).disposeWhen(retired);
        org.mockito.Mockito.verify(delegate, org.mockito.Mockito.never()).dispose();
        assertEquals(Map.of(retained, 3), provider.maxConnectionsPerHost());
    }

    @Test
    void cancellationFailsAcquisitionAndCancelsDelegate() {
        ConnectionProvider delegate = mock(ConnectionProvider.class);
        AtomicBoolean cancelled = new AtomicBoolean();
        when(delegate.acquire(null, null, null, null)).thenAnswer(invocation -> Mono.<Connection>never().doOnCancel(() -> cancelled.set(true)));
        HttpClientConnectionProvider provider = new HttpClientConnectionProvider(delegate, false);
        Sinks.Empty<Void> cancellation = Sinks.empty();
        Object key = ReflectionTestUtils.getField(HttpClientConnectionProvider.class, "CANCELLATION");

        CompletableFuture<? extends Connection> acquired = provider.acquire(null, null, null, null)
                .contextWrite(context -> context.put(key, cancellation.asMono())).toFuture();
        cancellation.tryEmitEmpty();
        // CompletableFuture reports CancellationException directly, not wrapped in ExecutionException.
        assertThrows(CancellationException.class, () -> acquired.get(5, TimeUnit.SECONDS));
        assertTrue(cancelled.get());
    }

    @Test
    void successfulAcquisitionKeepsConnection() {
        ConnectionProvider delegate = mock(ConnectionProvider.class);
        Connection connection = mock(Connection.class);
        when(delegate.acquire(null, null, null, null)).thenAnswer(invocation -> Mono.just(connection));
        HttpClientConnectionProvider provider = new HttpClientConnectionProvider(delegate, false);
        Object key = ReflectionTestUtils.getField(HttpClientConnectionProvider.class, "CANCELLATION");

        assertSame(connection, provider.acquire(null, null, null, null)
                .contextWrite(context -> context.put(key, Mono.never())).block(Duration.ofSeconds(5)));
    }
}
