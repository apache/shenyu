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

import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.utils.InitialSyncApplication;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests initial synchronization readiness independently of the socket handshake.
 */
class InitialSyncStateTest {

    @Test
    void testInterleavedIncrementalMustFinishBeforeReadiness() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        CompletableFuture<Void> incremental = new CompletableFuture<>();
        state.applyIncremental(() -> InitialSyncApplication.register(incremental));
        state.accept(new WebsocketSyncFrame(id, 0, null), value -> { });
        assertFalse(ready.get());
        incremental.complete(null);
        assertTrue(ready.get());
    }

    @Test
    void testMultipleAdminsCannotCombinePartialAttempts() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState first = new InitialSyncState(ready);
        InitialSyncState second = new InitialSyncState(ready);
        String firstId = first.begin();
        String secondId = second.begin();
        first.accept(new WebsocketSyncFrame(firstId, 0, "data"), value -> { });
        second.accept(new WebsocketSyncFrame(firstId, 1, null), value -> { });
        second.accept(new WebsocketSyncFrame(secondId, 1, null), value -> { });
        assertFalse(ready.get());
        first.accept(new WebsocketSyncFrame(firstId, 1, null), value -> { });
        assertTrue(ready.get());
    }

    @Test
    void testEndFrameWaitsForDeferredApplication() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        CompletableFuture<Void> application = new CompletableFuture<>();
        state.accept(new WebsocketSyncFrame(id, 0, "data"), value -> InitialSyncApplication.register(application));
        state.accept(new WebsocketSyncFrame(id, 1, null), value -> { });
        assertFalse(ready.get());
        application.complete(null);
        assertTrue(ready.get());
    }

    @Test
    void testDeferredFailureAndAbandonedCompletion() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        CompletableFuture<Void> application = new CompletableFuture<>();
        state.accept(new WebsocketSyncFrame(id, 0, "data"), value -> InitialSyncApplication.register(application));
        state.accept(new WebsocketSyncFrame(id, 1, null), value -> { });
        application.completeExceptionally(new IllegalStateException("application failed"));
        assertFalse(ready.get());
        assertTrue(state.needsReconnect());
        String retry = state.begin();
        CompletableFuture<Void> abandoned = new CompletableFuture<>();
        state.accept(new WebsocketSyncFrame(retry, 0, "data"), value -> InitialSyncApplication.register(abandoned));
        state.accept(new WebsocketSyncFrame(retry, 1, null), value -> { });
        state.invalidate();
        String current = state.begin();
        abandoned.complete(null);
        assertFalse(ready.get());
        state.accept(new WebsocketSyncFrame(current, 0, null), value -> { });
        assertTrue(ready.get());
    }

    @Test
    void testUnsupportedPeerTimesOutWithoutOpeningReadiness() throws ReflectiveOperationException {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        state.begin();
        assertFalse(state.needsReconnect());
        Field startedAt = InitialSyncState.class.getDeclaredField("startedAt");
        startedAt.setAccessible(true);
        startedAt.setLong(state, System.nanoTime() - TimeUnit.SECONDS.toNanos(61));
        assertTrue(state.needsReconnect());
        assertFalse(ready.get());
    }

    @Test
    void testEmptySnapshotAndLaterDisconnect() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        assertFalse(ready.get());
        state.accept(new WebsocketSyncFrame(id, 0, null), value -> { });
        assertTrue(ready.get());
        state.invalidate();
        assertTrue(ready.get());
        state.begin();
        assertTrue(ready.get());
    }

    @Test
    void testMissingFrameDoesNotOpenReadiness() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        state.accept(new WebsocketSyncFrame(id, 1, null), value -> { });
        state.accept(new WebsocketSyncFrame(id, 0, null), value -> { });
        assertFalse(ready.get());
    }

    @Test
    void testFailedApplicationAndFreshAttempt() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        assertThrows(IllegalStateException.class, () -> state.accept(new WebsocketSyncFrame(id, 0, "data"), value -> {
            throw new IllegalStateException("subscriber failed");
        }));
        state.accept(new WebsocketSyncFrame(id, 1, null), value -> { });
        assertFalse(ready.get());
        String retry = state.begin();
        state.accept(new WebsocketSyncFrame(id, 0, null), value -> { });
        assertFalse(ready.get());
        state.accept(new WebsocketSyncFrame(retry, 0, "data"), value -> { });
        assertFalse(ready.get());
        state.accept(new WebsocketSyncFrame(retry, 1, null), value -> { });
        assertTrue(ready.get());
    }

    @Test
    void testDisconnectInvalidatesIncompleteAttempt() {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        state.invalidate();
        state.accept(new WebsocketSyncFrame(id, 0, null), value -> { });
        assertFalse(ready.get());
    }

    @Test
    void testCompletionWaitsForApplication() throws Exception {
        AtomicBoolean ready = new AtomicBoolean();
        InitialSyncState state = new InitialSyncState(ready);
        String id = state.begin();
        CountDownLatch applying = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<?> data = executor.submit(() -> state.accept(new WebsocketSyncFrame(id, 0, "data"), value -> {
                applying.countDown();
                try {
                    if (!release.await(5, TimeUnit.SECONDS)) {
                        throw new IllegalStateException("test application timed out");
                    }
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(ex);
                }
            }));
            assertTrue(applying.await(5, TimeUnit.SECONDS));
            final Future<?> completion = executor.submit(() -> state.accept(new WebsocketSyncFrame(id, 1, null), value -> { }));
            assertFalse(ready.get());
            release.countDown();
            data.get(5, TimeUnit.SECONDS);
            completion.get(5, TimeUnit.SECONDS);
            assertTrue(ready.get());
        } finally {
            release.countDown();
            executor.shutdownNow();
        }
    }
}
