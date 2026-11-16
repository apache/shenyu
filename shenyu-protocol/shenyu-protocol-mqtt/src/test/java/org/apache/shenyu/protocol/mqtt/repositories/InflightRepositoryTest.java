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

package org.apache.shenyu.protocol.mqtt.repositories;

import org.junit.jupiter.api.Test;

import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link InflightRepository}: sequential PUBREL semantics and the
 * register/release concurrency invariant (#8021).
 */
public final class InflightRepositoryTest {

    private static final String CLIENT_ID = "inflight-client";

    private final InflightRepository repository = new InflightRepository();

    @Test
    public void releaseOfTheLastIdEvictsTheClientEntry() {
        repository.register(CLIENT_ID, 1);
        assertNotNull(repository.get(CLIENT_ID));

        repository.release(CLIENT_ID, 1);

        assertNull(repository.get(CLIENT_ID),
                "an emptied client entry must not linger in the map");
    }

    @Test
    public void releaseOfUnknownIdOrClientIsNoOp() {
        repository.release(CLIENT_ID, 7);
        assertNull(repository.get(CLIENT_ID));

        repository.register(CLIENT_ID, 1);
        repository.release(CLIENT_ID, 7);
        assertTrue(repository.get(CLIENT_ID).contains(1),
                "releasing an unrelated id must not touch the in-flight entry");
    }

    @Test
    public void releasedIdBecomesReusable() {
        assertTrue(repository.register(CLIENT_ID, 1));
        assertFalse(repository.register(CLIENT_ID, 1), "the same publish must dedup");

        repository.release(CLIENT_ID, 1);

        assertTrue(repository.register(CLIENT_ID, 1),
                "a packet id is free again once PUBCOMP answers the PUBREL");
    }

    @Test
    public void sessionResetRemovesAllClientEntries() {
        repository.register(CLIENT_ID, 1);
        repository.register(CLIENT_ID, 2);

        repository.remove(CLIENT_ID);

        assertNull(repository.get(CLIENT_ID));
        assertTrue(repository.register(CLIENT_ID, 1),
                "a new session resets the packet id space, so ids register as first-seen");
    }

    /**
     * Two clients publishing concurrently must never lose a freshly registered id
     * to the other one's empty-entry cleanup: once {@code register} returned true,
     * the id is visible in the map until its own release (#8021). The register and
     * release of each id run inside one per-key compute, so the invariant holds by
     * construction; on the previous get/remove-in-two-steps implementation the
     * cleanup could evict a concurrently added id.
     */
    @Test
    public void concurrentRegisterAndReleaseMustNotLoseRegisteredIds() throws Exception {
        final InflightRepository shared = new InflightRepository();
        final int iterations = 100_000;
        final CountDownLatch start = new CountDownLatch(1);
        final AtomicReference<AssertionError> failure = new AtomicReference<>();
        final Thread first = new Thread(() -> publishLoop(shared, start, failure, 1));
        final Thread second = new Thread(() -> publishLoop(shared, start, failure, 2));
        first.start();
        second.start();
        start.countDown();
        first.join(TimeUnit.MINUTES.toMillis(1));
        second.join(TimeUnit.MINUTES.toMillis(1));

        if (Objects.nonNull(failure.get())) {
            throw failure.get();
        }
        assertNull(shared.get(CLIENT_ID),
                "every id was released, so no client entry may remain");
    }

    private void publishLoop(final InflightRepository shared, final CountDownLatch start,
                             final AtomicReference<AssertionError> failure, final int packetId) {
        try {
            start.await();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return;
        }
        for (int i = 0; i < 100_000 && Objects.isNull(failure.get()); i++) {
            if (shared.register(CLIENT_ID, packetId)) {
                final Set<Integer> inFlight = shared.get(CLIENT_ID);
                if (Objects.isNull(inFlight) || !inFlight.contains(packetId)) {
                    failure.compareAndSet(null, new AssertionError(
                            "a freshly registered id was evicted by a concurrent release, iteration " + i));
                    return;
                }
            }
            shared.release(CLIENT_ID, packetId);
        }
    }

    @Test
    public void baseRepositoryAccessorsDelegateToTheSameState() {
        final Set<Integer> ids = Set.of(3, 4);
        repository.add(CLIENT_ID, ids);
        assertEquals(ids, repository.get(CLIENT_ID));

        repository.remove(CLIENT_ID);
        assertNull(repository.get(CLIENT_ID));
    }
}
