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

package org.apache.shenyu.plugin.agent.gateway.prompt;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AgentPromptRegistryTest {

    private final ObjectMapper json = new ObjectMapper();

    @Test
    void freezesMetadataArgumentsAndResultsWithoutMutatingTheProvider() {
        ObjectNode source = metadata();
        AgentPromptDefinition definition = new AgentPromptDefinition(source);
        source.put("name", "changed");
        definition.metadata().remove("arguments");
        assertEquals("summary", definition.name());
        definition.validate(Map.of("topic", "gateway"));
        assertThrows(IllegalArgumentException.class, () -> definition.validate(Map.of()));
        assertThrows(IllegalArgumentException.class, () -> definition.validate(Map.of("topic", "a", "unknown", "b")));
        Map<String, String> supplied = new HashMap<>(Map.of("topic", "before"));
        AgentPromptInvocation invocation = invocation(supplied);
        supplied.put("topic", "after");
        assertEquals("before", invocation.arguments().get("topic"));
        assertThrows(UnsupportedOperationException.class, () -> invocation.arguments().clear());
        ObjectNode result = result("original");
        AgentPromptDefinition.validateResult(result).remove("messages");
        assertEquals(1, result.path("messages").size());
    }

    @Test
    void deniesAndArgumentErrorsNeverEnterProviderAndAdmissionCanDeferIt() {
        AtomicInteger calls = new AtomicInteger();
        AgentPromptRegistry registry = registry(input -> {
            calls.incrementAndGet();
            return Mono.just(result(input.subject()));
        });
        StepVerifier.create(registry.get("summary", Set.of(), invocation(Map.of("topic", "a"))))
                .expectError(SecurityException.class).verify();
        StepVerifier.create(registry.get("summary", Set.of("summary"), invocation(Map.of())))
                .expectError(IllegalArgumentException.class).verify();
        Mono<ObjectNode> pending = registry.get("summary", Set.of("summary"), invocation(Map.of("topic", "a")));
        assertEquals(0, calls.get());
        assertEquals("trusted", pending.block().path("messages").get(0).path("content").path("text").asText());
        assertEquals(1, calls.get());
    }

    @Test
    void cancellationIsForwardedAndEmptyOrInvalidResultsAreNotSuccesses() {
        AtomicInteger cancelled = new AtomicInteger();
        StepVerifier.create(registry(input -> Mono.<ObjectNode>never().doOnCancel(cancelled::incrementAndGet))
                .get("summary", Set.of("summary"), invocation(Map.of("topic", "a")))).thenCancel().verify();
        assertEquals(1, cancelled.get());
        StepVerifier.create(registry(input -> Mono.empty()).get("summary", Set.of("summary"), invocation(Map.of("topic", "a"))))
                .expectError(IllegalStateException.class).verify();
        ObjectNode invalid = result("text");
        ((ObjectNode) invalid.path("messages").get(0).path("content")).put("type", "image");
        StepVerifier.create(registry(input -> Mono.just(invalid)).get("summary", Set.of("summary"), invocation(Map.of("topic", "a"))))
                .expectError(IllegalArgumentException.class).verify();
        assertThrows(IllegalArgumentException.class, () -> AgentPromptDefinition.validateResult(result("x".repeat(1048576))));
    }

    @Test
    void rejectsDuplicatesAndBoundsAndAllowsMultilineDescriptions() {
        AgentPromptProvider provider = provider(input -> Mono.just(result("ok")));
        assertThrows(IllegalArgumentException.class, () -> new AgentPromptRegistry(List.of(provider, provider)));
        assertThrows(IllegalArgumentException.class, () -> new AgentPromptRegistry(new ArrayList<>(java.util.Collections.nCopies(17, provider))));
        ObjectNode source = metadata();
        source.put("description", "A description\nwith another line.");
        new AgentPromptDefinition(source);
        source.withArray("arguments").add(source.path("arguments").get(0).deepCopy());
        assertThrows(IllegalArgumentException.class, () -> new AgentPromptDefinition(source));
        ObjectNode invalid = json.createObjectNode().put("name", "bad/name");
        assertThrows(IllegalArgumentException.class, () -> new AgentPromptDefinition(invalid));
        assertThrows(IllegalArgumentException.class, () -> new AgentPromptDefinition(metadata().put("description", "x".repeat(4097))));
    }

    private AgentPromptInvocation invocation(final Map<String, String> arguments) {
        return new AgentPromptInvocation("request", "tenant", "trusted", "rule", 1, Instant.now().plusSeconds(10), arguments);
    }

    private AgentPromptRegistry registry(final java.util.function.Function<AgentPromptInvocation, Mono<ObjectNode>> get) {
        return new AgentPromptRegistry(List.of(provider(get)));
    }

    private AgentPromptProvider provider(final java.util.function.Function<AgentPromptInvocation, Mono<ObjectNode>> get) {
        return new AgentPromptProvider() {
            @Override
            public AgentPromptDefinition definition() {
                return new AgentPromptDefinition(metadata());
            }

            @Override
            public Mono<ObjectNode> get(final AgentPromptInvocation invocation) {
                return get.apply(invocation);
            }
        };
    }

    private ObjectNode metadata() {
        ObjectNode result = json.createObjectNode().put("name", "summary");
        result.putArray("arguments").addObject().put("name", "topic").put("required", true);
        return result;
    }

    private ObjectNode result(final String text) {
        ObjectNode result = json.createObjectNode();
        result.putArray("messages").addObject().put("role", "user").putObject("content").put("type", "text").put("text", text);
        return result;
    }
}
