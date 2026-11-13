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

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;

import java.time.Duration;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class HttpClientPoolMetricsTest {

    private SimpleMeterRegistry registry;

    private String poolName;

    private HttpClientPoolMetrics metrics;

    @BeforeEach
    void setUp() {
        registry = new SimpleMeterRegistry();
        Metrics.addRegistry(registry);
        poolName = "acquisition-test-" + UUID.randomUUID();
        metrics = new HttpClientPoolMetrics(poolName);
    }

    @AfterEach
    void tearDown() {
        registry.getMeters().stream().filter(meter -> poolName.equals(meter.getId().getTag("name")))
                .forEach(meter -> Metrics.globalRegistry.remove(meter.getId()));
        Metrics.removeRegistry(registry);
        registry.close();
    }

    @Test
    void cancellationAfterDeliveryRecordsOnlySuccessForEachSubscription() {
        Connection connection = mock(Connection.class);
        Mono<? extends Connection> observed = metrics.observe(Mono.just(connection));

        // take(1) cancels upstream from onNext, before the source can complete.
        assertSame(connection, observed.flux().take(1).blockLast(Duration.ofSeconds(5)));
        assertSame(connection, observed.flux().take(1).blockLast(Duration.ofSeconds(5)));

        assertEquals(2, count("success"));
        assertEquals(0, count("cancelled"));
        assertEquals(0, count("error"));
        assertEquals(0, count("timeout"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void failedAcquisitionPreservesFailureAndRecordsOnlyItsOutcome(final boolean timeout) {
        Exception failure = timeout ? new TimeoutException("queue deadline") : new IllegalStateException("provider disposed");
        CompletableFuture<? extends Connection> acquired = metrics.observe(Mono.error(failure)).toFuture();

        assertSame(failure, assertThrows(CompletionException.class, acquired::join).getCause());
        assertEquals(timeout ? 1 : 0, count("timeout"));
        assertEquals(timeout ? 0 : 1, count("error"));
        assertEquals(0, count("success"));
        assertEquals(0, count("cancelled"));
    }

    @Test
    void cancellationBeforeDeliveryStopsAcquisitionAndRecordsOnlyCancellation() {
        AtomicBoolean cancelled = new AtomicBoolean();
        CompletableFuture<? extends Connection> acquired = metrics.observe(Mono.<Connection>never()
                .doOnCancel(() -> cancelled.set(true))).toFuture();
        try {
            assertTrue(acquired.cancel(true));
            assertTrue(cancelled.get());
            assertEquals(1, count("cancelled"));
            assertEquals(0, count("success"));
            assertEquals(0, count("error"));
            assertEquals(0, count("timeout"));
        } finally {
            acquired.cancel(true);
        }
    }

    private long count(final String outcome) {
        return registry.get("shenyu.httpclient.pool.acquire").tags("name", poolName, "outcome", outcome).timer().count();
    }
}
