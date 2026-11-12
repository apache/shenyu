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

package org.apache.shenyu.plugin.mcp.server.transport;

import io.modelcontextprotocol.spec.McpServerSession;
import org.junit.jupiter.api.Test;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;
import reactor.test.scheduler.VirtualTimeScheduler;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class McpSessionLifecycleTest {

    @Test
    void concurrentCreationNeverExceedsCapacityAndRemovalIsIdempotent() {
        final AtomicInteger created = new AtomicInteger();
        final AtomicInteger removed = new AtomicInteger();
        try (McpSessionLifecycle lifecycle = new McpSessionLifecycle(new McpSessionPolicy(2, Duration.ZERO, Duration.ofSeconds(1)), id -> removed.incrementAndGet())) {
            final List<CompletableFuture<Void>> contenders = new ArrayList<>();
            for (int i = 0; i < 40; i++) {
                contenders.add(CompletableFuture.runAsync(() -> {
                    try {
                        lifecycle.create(() -> session("session-" + created.incrementAndGet()), item -> { });
                    } catch (ResponseStatusException expected) {
                        assertEquals(503, expected.getStatusCode().value());
                    }
                }));
            }
            CompletableFuture.allOf(contenders.toArray(CompletableFuture[]::new)).join();
            assertEquals(2, created.get());
            lifecycle.remove("session-1");
            lifecycle.remove("session-1");
            assertEquals(1, removed.get());
            lifecycle.create(() -> session("replacement"), item -> { });
        }
        assertEquals(3, removed.get());
    }

    @Test
    void factoryAndInstallFailureReleaseCapacity() {
        try (McpSessionLifecycle lifecycle = new McpSessionLifecycle(new McpSessionPolicy(1, Duration.ZERO, Duration.ofSeconds(1)), id -> { })) {
            assertThrows(IllegalStateException.class, () -> lifecycle.create(() -> {
                throw new IllegalStateException("factory");
            }, item -> { }));
            final McpServerSession failed = session("failed");
            assertThrows(IllegalStateException.class, () -> lifecycle.create(() -> failed, item -> {
                throw new IllegalStateException("install");
            }));
            verify(failed).close();
            lifecycle.create(() -> session("next"), item -> { });
        }
    }

    @Test
    void activeRequestSurvivesIdleAgeAndCompletionRestartsClock() {
        final VirtualTimeScheduler scheduler = VirtualTimeScheduler.create();
        final AtomicInteger removed = new AtomicInteger();
        final Sinks.One<String> result = Sinks.one();
        try (McpSessionLifecycle lifecycle = timed(scheduler, removed)) {
            lifecycle.create(() -> session("active"), item -> { });
            StepVerifier.create(lifecycle.withActivity("active", result::asMono))
                    .then(() -> scheduler.advanceTimeBy(Duration.ofSeconds(20)))
                    .then(() -> assertEquals(0, removed.get()))
                    .then(() -> result.tryEmitValue("done"))
                    .expectNext("done").verifyComplete();
            scheduler.advanceTimeBy(Duration.ofSeconds(4));
            assertEquals(0, removed.get());
            scheduler.advanceTimeBy(Duration.ofSeconds(1));
            assertEquals(1, removed.get());
            StepVerifier.create(lifecycle.withActivity("active", () -> Mono.just("late"))).expectError(ResponseStatusException.class).verify();
        } finally {
            scheduler.dispose();
        }
    }

    @Test
    void cancellationAndErrorReleaseActivityAndShutdownCancelsOwnedWork() {
        final VirtualTimeScheduler scheduler = VirtualTimeScheduler.create();
        final AtomicInteger removed = new AtomicInteger();
        final AtomicBoolean cancelled = new AtomicBoolean();
        try (McpSessionLifecycle lifecycle = timed(scheduler, removed)) {
            lifecycle.create(() -> session("cancel"), item -> { });
            final Disposable pending = lifecycle.withActivity("cancel", () -> Mono.never().doOnCancel(() -> cancelled.set(true))).subscribe();
            pending.dispose();
            assertTrue(cancelled.get());
            StepVerifier.create(lifecycle.withActivity("cancel", () -> Mono.error(new IllegalStateException("handler"))))
                    .expectError(IllegalStateException.class).verify();
            scheduler.advanceTimeBy(Duration.ofSeconds(5));
            assertEquals(1, removed.get());
            final McpServerSession broken = session("shutdown");
            doThrow(new IllegalStateException("close")).when(broken).close();
            lifecycle.create(() -> broken, item -> { });
            StepVerifier.create(lifecycle.withActivity("shutdown", Mono::never)).then(lifecycle::close)
                    .expectError(ResponseStatusException.class).verify();
            lifecycle.close();
            verify(broken, times(1)).close();
            assertEquals(2, removed.get());
            assertThrows(ResponseStatusException.class, () -> lifecycle.create(() -> session("late"), item -> { }));
        } finally {
            scheduler.dispose();
        }
    }

    @Test
    void rejectsInvalidPoliciesAndKeepsUnlimitedCompatibility() {
        assertThrows(IllegalArgumentException.class, () -> new McpSessionPolicy(-1, Duration.ZERO, Duration.ofSeconds(1)));
        assertThrows(IllegalArgumentException.class, () -> new McpSessionPolicy(1, Duration.ofSeconds(-1), Duration.ofSeconds(1)));
        assertThrows(IllegalArgumentException.class, () -> new McpSessionPolicy(1, Duration.ofSeconds(1), Duration.ZERO));
        assertThrows(IllegalArgumentException.class, () -> new McpSessionPolicy(1, Duration.ofSeconds(1), Duration.ofSeconds(2)));
        assertThrows(ArithmeticException.class, () -> new McpSessionPolicy(1, Duration.ofSeconds(Long.MAX_VALUE), Duration.ofSeconds(1)));
        assertEquals(0, McpSessionPolicy.unlimited().maxSessions());
    }

    @Test
    void bindingRacingRemovalCannotResurrectGlobalState() {
        final ConcurrentHashMap<String, String> bindings = new ConcurrentHashMap<>();
        try (McpSessionLifecycle lifecycle = new McpSessionLifecycle(McpSessionPolicy.unlimited(), bindings::remove)) {
            for (int i = 0; i < 100; i++) {
                final String id = "race-" + i;
                lifecycle.create(() -> session(id), item -> { });
                CompletableFuture.allOf(
                        CompletableFuture.runAsync(() -> lifecycle.bind(id, () -> bindings.put(id, "exchange"))),
                        CompletableFuture.runAsync(() -> lifecycle.remove(id))).join();
                assertTrue(bindings.isEmpty());
                assertTrue(!lifecycle.bind(id, () -> bindings.put(id, "late")));
            }
        }
    }

    @Test
    void expiredCloseFailureDoesNotCloseActivePeerOrLeakCapacity() {
        final VirtualTimeScheduler scheduler = VirtualTimeScheduler.create();
        final AtomicInteger removed = new AtomicInteger();
        try (McpSessionLifecycle lifecycle = timed(scheduler, removed)) {
            final McpServerSession broken = session("broken");
            doThrow(new IllegalStateException("fixture close failure")).when(broken).close();
            lifecycle.create(() -> broken, item -> { });
            lifecycle.create(() -> session("peer"), item -> { });
            StepVerifier.create(lifecycle.withActivity("peer", Mono::never))
                    .then(() -> scheduler.advanceTimeBy(Duration.ofSeconds(5)))
                    .then(() -> assertEquals(1, removed.get()))
                    .then(() -> lifecycle.create(() -> session("replacement"), item -> { }))
                    .thenCancel().verify(Duration.ofSeconds(5));
            verify(broken).close();
        } finally {
            scheduler.dispose();
        }
    }

    private McpSessionLifecycle timed(final VirtualTimeScheduler scheduler, final AtomicInteger removed) {
        return new McpSessionLifecycle(new McpSessionPolicy(2, Duration.ofSeconds(5), Duration.ofSeconds(1)),
                id -> removed.incrementAndGet(), () -> scheduler.now(TimeUnit.NANOSECONDS), scheduler);
    }

    private McpServerSession session(final String id) {
        final McpServerSession session = mock(McpServerSession.class);
        when(session.getId()).thenReturn(id);
        return session;
    }
}
