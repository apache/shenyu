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
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentToolCapabilityTest {

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"sampling\":null}", "{\"sampling\":true}", "{\"sampling\":\"true\"}",
            "{\"sampling\":[]}", "{\"sampling\":{}}", "{\"sampling\":{\"tools\":false}}"})
    void shouldRejectMissingObjectsAndNestedMarkersBeforeValidation(final String declared) {
        AtomicInteger validation = new AtomicInteger();
        AtomicInteger calls = new AtomicInteger();
        AgentToolRegistry registry = registry(parse("{\"sampling\":{\"tools\":true}}"), validation, calls);
        StepVerifier.create(registry.invoke("tool", Set.of("tool"), () -> invocation(parse(declared))))
                .expectErrorSatisfies(error -> {
                    JsonObject missing = ((AgentToolCapabilityException) error).getRequiredCapabilities();
                    assertEquals(parse("{\"sampling\":{\"tools\":true}}"), missing);
                    missing.remove("sampling");
                    assertTrue(((AgentToolCapabilityException) error).getRequiredCapabilities().has("sampling"));
                }).verify();
        assertEquals(0, validation.get());
        assertEquals(0, calls.get());
    }

    @Test
    void shouldAcceptSupersetAndFreezeProviderAndInvocationSnapshots() {
        AtomicInteger validation = new AtomicInteger();
        AtomicInteger calls = new AtomicInteger();
        JsonObject required = parse("{\"sampling\":{\"tools\":true}}");
        AgentToolRegistry registry = registry(required, validation, calls);
        required.add("roots", new JsonObject());
        registry.listDefinitions(Set.of("tool")).get("tool").getRequiredClientCapabilities().add("roots", new JsonObject());
        JsonObject declared = parse("{\"sampling\":{\"tools\":true,\"extra\":{}},\"roots\":{}}");
        AgentToolInvocation input = invocation(declared);
        declared.remove("sampling");
        input.getClientCapabilities().remove("sampling");
        StepVerifier.create(registry.invoke("tool", Set.of("tool"), () -> input)).expectNextCount(1).verifyComplete();
        assertEquals(1, validation.get());
        assertEquals(1, calls.get());
        StepVerifier.create(registry.invoke("tool", Set.of("tool"), () -> invocation(new JsonObject())))
                .expectError(AgentToolCapabilityException.class).verify();
        assertEquals(1, calls.get());
    }

    @Test
    void shouldReportOnlyMissingPartsOfMultipleRequirements() {
        AgentToolDefinition definition = new AgentToolDefinition("tool", "Read a record", parse("{\"type\":\"object\"}"),
                parse("{\"sampling\":{},\"elicitation\":{\"form\":{},\"url\":{}}}"));
        assertEquals(parse("{\"elicitation\":{\"url\":{}}}"),
                definition.missingCapabilities(parse("{\"sampling\":{},\"elicitation\":{\"form\":{}}}")));
        assertEquals(0, definition.missingCapabilities(parse("{\"sampling\":{},\"elicitation\":{\"form\":{},\"url\":{}}}")).size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"sampling\":true}", "{\"sampling\":null}", "{\"sampling\":[]}", "{\"sampling\":{\"tools\":false}}",
            "{\"sampling\":{\"tools\":\"true\"}}", "{\"sampling\":{\"tools\":1}}", "{\" \":{}}"})
    void shouldFailRegistrationForUnsupportedRequirementShapes(final String requirements) {
        assertThrows(IllegalArgumentException.class, () -> registry(parse(requirements), new AtomicInteger(), new AtomicInteger()));
    }

    @Test
    void shouldRejectNullAndExcessivelyDeepRequirements() {
        assertThrows(NullPointerException.class, () -> registry(null, new AtomicInteger(), new AtomicInteger()));
        JsonObject deep = new JsonObject();
        JsonObject current = deep;
        for (int level = 0; level < 34; level++) {
            JsonObject nested = new JsonObject();
            current.add("nested", nested);
            current = nested;
        }
        assertThrows(IllegalArgumentException.class, () -> registry(deep, new AtomicInteger(), new AtomicInteger()));
    }

    private AgentToolRegistry registry(final JsonObject required, final AtomicInteger validation, final AtomicInteger calls) {
        AgentToolProvider tool = org.mockito.Mockito.mock(AgentToolProvider.class);
        org.mockito.Mockito.when(tool.getName()).thenReturn("tool");
        org.mockito.Mockito.when(tool.getDescription()).thenReturn("Capability contract fixture");
        org.mockito.Mockito.when(tool.getInputSchema()).thenReturn(parse("{\"type\":\"object\"}"));
        org.mockito.Mockito.when(tool.getRequiredClientCapabilities()).thenReturn(required);
        org.mockito.Mockito.doAnswer(invocation -> {
            validation.incrementAndGet();
            return null;
        }).when(tool).validate(org.mockito.ArgumentMatchers.any());
        org.mockito.Mockito.when(tool.invoke(org.mockito.ArgumentMatchers.any())).thenAnswer(invocation -> {
            calls.incrementAndGet();
            return Mono.just(invocation.<AgentToolInvocation>getArgument(0).getClientCapabilities());
        });
        return new AgentToolRegistry(List.of(tool));
    }

    private AgentToolInvocation invocation(final JsonObject capabilities) {
        return new AgentToolInvocation("request-id", "trusted-subject", new JsonObject(), "rule", 1, Instant.MAX, capabilities);
    }

    private JsonObject parse(final String value) {
        return JsonParser.parseString(value).getAsJsonObject();
    }
}
