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
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.Connection;
import reactor.netty.resources.ConnectionProvider;

import java.time.Duration;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class HttpClientConnectionProviderTest {

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
