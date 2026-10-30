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

package org.apache.shenyu.plugin.agent.gateway.tool;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentResourceAdmission;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentToolRegistryTest {

    @Test
    void shouldRejectDuplicateNames() {
        AgentToolProvider tool = tool(input -> Mono.just(input.getArguments()));
        assertThrows(IllegalArgumentException.class, () -> new AgentToolRegistry(List.of(tool, tool)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"global", "local", "timeout", "cancel"})
    void admissionRejectionMustNotRecordAnExecutionAttempt(final String scenario) {
        AtomicInteger attempts = new AtomicInteger();
        AtomicInteger invocations = new AtomicInteger();
        AtomicInteger subscriptions = new AtomicInteger();
        AgentToolRegistry registry = registry(input -> {
            invocations.incrementAndGet();
            return Mono.fromSupplier(() -> {
                subscriptions.incrementAndGet();
                return input.getArguments();
            });
        });
        boolean queued = "timeout".equals(scenario) || "cancel".equals(scenario);
        try (AgentResourceAdmission admission = new AgentResourceAdmission(
                new AgentGatewayAdmissionConfig("local".equals(scenario) ? 2 : 1, queued ? 1 : 0, 1, 0, 1, 0))) {
            reactor.core.Disposable active = admission.withLocal(Instant.now().plusSeconds(30), Mono.never()).subscribe();
            try {
                Instant deadline = Instant.now().plusMillis("timeout".equals(scenario) ? 150 : 30000);
                Mono<JsonObject> execution = registry.invoke("order_status", Set.of("order_status"), () -> invocation("A"), attempts::incrementAndGet,
                        action -> admission.withLocal(deadline, action));
                if ("cancel".equals(scenario)) {
                    StepVerifier.create(execution).then(() -> assertTrue(admission.snapshot().stream().anyMatch(state -> state.queued() == 1)))
                            .thenCancel().verify(Duration.ofSeconds(5));
                } else {
                    StepVerifier.create(execution).expectErrorMatches(error -> error instanceof AgentResourceAdmission.AgentAdmissionException
                            && ((AgentResourceAdmission.AgentAdmissionException) error).isTimeout() == queued).verify(Duration.ofSeconds(5));
                }
                assertEquals(0, attempts.get());
                assertEquals(0, invocations.get());
                assertEquals(0, subscriptions.get());
            } finally {
                active.dispose();
            }
            StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation("A"), attempts::incrementAndGet,
                    execution -> admission.withLocal(Instant.now().plusSeconds(30), execution))).expectNextCount(1).verifyComplete();
            assertEquals(1, attempts.get());
            assertEquals(1, invocations.get());
            assertEquals(1, subscriptions.get());
            assertTrue(admission.snapshot().stream().allMatch(state -> state.inFlight() == 0 && state.queued() == 0));
        }
    }

    @Test
    void shouldKeepRegistrationAndSchemaSnapshots() {
        List<AgentToolProvider> tools = new ArrayList<>();
        tools.add(tool(input -> Mono.just(input.getArguments())));
        AgentToolRegistry registry = new AgentToolRegistry(tools);
        tools.clear();
        Map<String, JsonObject> visible = registry.list(Set.of("order_status"));
        visible.get("order_status").addProperty("type", "changed");
        assertEquals("object", registry.list(Set.of("order_status")).get("order_status").get("type").getAsString());
        assertTrue(registry.list(Set.of()).isEmpty());
        assertThrows(UnsupportedOperationException.class, visible::clear);
    }

    @Test
    void shouldRejectUnauthorizedAndUnknownToolsBeforeCreatingInput() {
        AtomicInteger calls = new AtomicInteger();
        AgentToolRegistry registry = registry(input -> {
            calls.incrementAndGet();
            return Mono.just(input.getArguments());
        });
        for (String name : List.of("order_status", "unknown")) {
            StepVerifier.create(registry.invoke(name, Set.of(), () -> {
                throw new AssertionError("input must not be created");
            })).expectError(SecurityException.class).verify();
        }
        StepVerifier.create(registry.invoke("unknown", Set.of("unknown"), () -> invocation("A")))
                .expectError(SecurityException.class).verify();
        assertEquals(0, calls.get());
    }

    @Test
    void shouldRejectInvalidArgumentsBeforeInvocation() {
        AtomicInteger calls = new AtomicInteger();
        AgentToolRegistry registry = registry(input -> {
            calls.incrementAndGet();
            return Mono.just(input.getArguments());
        });
        StepVerifier.create(registry.invoke("order_status", Set.of("order_status"),
                () -> new AgentToolInvocation("id", "A", new JsonObject())))
                .expectError(IllegalArgumentException.class).verify();
        assertEquals(0, calls.get());
    }

    @Test
    void shouldBeLazyAndCreateIndependentInvocations() {
        List<AgentToolInvocation> calls = new ArrayList<>();
        AgentToolRegistry registry = registry(input -> {
            calls.add(input);
            return Mono.just(input.getArguments());
        });
        AtomicInteger ids = new AtomicInteger();
        Mono<JsonObject> result = registry.invoke("order_status", Set.of("order_status"),
                () -> invocation("request-" + ids.incrementAndGet()));
        assertTrue(calls.isEmpty());
        StepVerifier.create(result).expectNextCount(1).verifyComplete();
        StepVerifier.create(result).expectNextCount(1).verifyComplete();
        assertEquals(2, calls.size());
        assertNotSame(calls.get(0), calls.get(1));
        assertEquals("request-1", calls.get(0).getRequestId());
        assertEquals("request-2", calls.get(1).getRequestId());
    }

    @Test
    void shouldCopyArgumentsAndResults() {
        JsonObject arguments = arguments("A");
        AgentToolInvocation invocation = new AgentToolInvocation("id", "A", arguments);
        arguments.addProperty("orderId", "changed");
        invocation.getArguments().addProperty("orderId", "also changed");
        assertEquals("A", invocation.getArguments().get("orderId").getAsString());
        JsonObject providerResult = arguments("original");
        AgentToolRegistry registry = registry(input -> Mono.just(providerResult));
        StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation))
                .assertNext(result -> result.addProperty("orderId", "consumer mutation"))
                .verifyComplete();
        assertEquals("original", providerResult.get("orderId").getAsString());
    }

    @Test
    void shouldRejectEmptyCompletion() {
        StepVerifier.create(registry(input -> Mono.empty())
                .invoke("order_status", Set.of("order_status"), () -> invocation("A")))
                .expectError(IllegalStateException.class).verify();
    }

    @Test
    void shouldPropagateFailureWithoutRetry() {
        AtomicInteger calls = new AtomicInteger();
        AgentToolRegistry registry = registry(input -> {
            calls.incrementAndGet();
            return Mono.error(new IllegalStateException("failure"));
        });
        StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation("A")))
                .expectErrorMessage("failure").verify();
        assertEquals(1, calls.get());
    }

    @Test
    void shouldCancelOnlyOneConcurrentInvocation() {
        AtomicBoolean cancelled = new AtomicBoolean();
        Sinks.One<JsonObject> survivor = Sinks.one();
        AgentToolRegistry registry = registry(input -> {
            if ("A".equals(input.getSubject())) {
                return Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true));
            }
            return survivor.asMono();
        });
        StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation("B")))
                .then(() -> {
                    StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation("A")))
                            .thenCancel().verify();
                    assertTrue(cancelled.get());
                    assertEquals(Sinks.EmitResult.OK, survivor.tryEmitValue(arguments("B")));
                })
                .assertNext(result -> assertEquals("B", result.get("orderId").getAsString()))
                .verifyComplete();
    }

    @Test
    void shouldPreserveContextAcrossScheduling() {
        AgentToolRegistry registry = registry(input -> Mono.deferContextual(context ->
                Mono.just(arguments(context.get("subject")))).subscribeOn(Schedulers.parallel()));
        StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation("A"))
                .contextWrite(context -> context.put("subject", "A")))
                .assertNext(result -> assertEquals("A", result.get("orderId").getAsString()))
                .verifyComplete();
    }

    @Test
    void shouldPropagateCallerTimeoutToProvider() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentToolRegistry registry = registry(input -> Mono.<JsonObject>never().doOnCancel(() -> cancelled.set(true)));
        StepVerifier.withVirtualTime(() -> registry.invoke("order_status", Set.of("order_status"), () -> invocation("A"))
                .timeout(Duration.ofSeconds(1)))
                .expectSubscription()
                .thenAwait(Duration.ofSeconds(1))
                .expectError(java.util.concurrent.TimeoutException.class)
                .verify();
        assertTrue(cancelled.get());
    }

    @Test
    void shouldNotReportNormalCompletionAsCancellation() {
        AtomicBoolean cancelled = new AtomicBoolean();
        AgentToolRegistry registry = registry(input -> Mono.just(input.getArguments()).doOnCancel(() -> cancelled.set(true)));
        StepVerifier.create(registry.invoke("order_status", Set.of("order_status"), () -> invocation("A")))
                .expectNextCount(1).verifyComplete();
        assertFalse(cancelled.get());
    }

    @Test
    void shouldIsolateThirtyTwoConcurrentCalls() {
        AgentToolRegistry registry = registry(input -> Mono.deferContextual(context -> {
            assertEquals(input.getSubject(), context.get("subject"));
            assertEquals(input.getSubject(), input.getArguments().get("orderId").getAsString());
            return Mono.just(input.getArguments());
        }).subscribeOn(Schedulers.parallel()));
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
            String subject = "subject-" + index;
            return registry.invoke("order_status", Set.of("order_status"), () -> invocation(subject))
                    .contextWrite(context -> context.put("subject", subject));
        }, 32).collectList())
                .assertNext(results -> assertEquals(32, results.stream()
                        .map(result -> result.get("orderId").getAsString()).distinct().count()))
                .verifyComplete();
    }

    @Test
    void shouldFreezePermissionsBeforeSubscription() {
        Set<String> permissions = new HashSet<>(Set.of("order_status"));
        AgentToolRegistry registry = registry(input -> Mono.just(input.getArguments()));
        Mono<JsonObject> result = registry.invoke("order_status", permissions, () -> invocation("A"));
        permissions.clear();
        StepVerifier.create(result).expectNextCount(1).verifyComplete();
        StepVerifier.create(registry.invoke("order_status", permissions, () -> invocation("A")))
                .expectError(SecurityException.class).verify();
    }

    @Test
    void shouldRejectMissingTrustedIdentity() {
        assertThrows(IllegalArgumentException.class, () -> new AgentToolInvocation("id", " ", arguments("A")));
        assertThrows(NullPointerException.class, () -> new AgentToolInvocation("id", null, arguments("A")));
    }

    private AgentToolRegistry registry(final Function<AgentToolInvocation, Mono<JsonObject>> action) {
        return new AgentToolRegistry(List.of(tool(action)));
    }

    private AgentToolProvider tool(final Function<AgentToolInvocation, Mono<JsonObject>> action) {
        return new AgentToolProvider() {
            @Override
            public String getName() {
                return "order_status";
            }

            @Override
            public String getDescription() {
                return "Read a test order";
            }

            @Override
            public JsonObject getInputSchema() {
                JsonObject schema = new JsonObject();
                schema.addProperty("type", "object");
                return schema;
            }

            @Override
            public void validate(final JsonObject input) {
                if (!input.has("orderId")) {
                    throw new IllegalArgumentException("orderId is required");
                }
            }

            @Override
            public Mono<JsonObject> invoke(final AgentToolInvocation input) {
                return action.apply(input);
            }
        };
    }

    private AgentToolInvocation invocation(final String subject) {
        return new AgentToolInvocation(subject, subject, arguments(subject));
    }

    private JsonObject arguments(final String orderId) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("orderId", orderId);
        return arguments;
    }
}
