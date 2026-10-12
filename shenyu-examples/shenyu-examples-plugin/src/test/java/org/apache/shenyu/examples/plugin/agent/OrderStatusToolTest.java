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

package org.apache.shenyu.examples.plugin.agent;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolExecutionException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Bounded input and record-level authorization of the synthetic read-only tool.
 */
class OrderStatusToolTest {

    private final OrderStatusTool tool = new OrderStatusTool();

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"orderId\":null}", "{\"orderId\":1}", "{\"orderId\":[]}", "{\"orderId\":\"\"}",
        "{\"orderId\":\"demo/A\"}", "{\"orderId\":\"demo-A-001\",\"extra\":true}"})
    void shouldRejectInvalidArguments(final String json) {
        assertThrows(IllegalArgumentException.class, () -> tool.validate(JsonParser.parseString(json).getAsJsonObject()));
    }

    @Test
    void shouldEnforceMaximumStringLength() {
        JsonObject args = new JsonObject();
        args.addProperty("orderId", "x".repeat(65));
        assertThrows(IllegalArgumentException.class, () -> tool.validate(args));
    }

    @Test
    void shouldNotExposeAnotherOwnersOrderOrDistinguishUnknownOrders() {
        StepVerifier.create(tool.invoke(input("agent-a", "demo-B-001"))).expectError(AgentToolExecutionException.class).verify();
        StepVerifier.create(tool.invoke(input("agent-a", "missing"))).expectError(AgentToolExecutionException.class).verify();
        StepVerifier.create(tool.invoke(input("agent-none", "demo-A-001"))).expectError(AgentToolExecutionException.class).verify();
    }

    @Test
    void shouldReturnIndependentResultsAndPreserveInvocationMetadata() {
        AgentToolInvocation input = input("agent-a", "demo-A-001");
        assertEquals("rule", input.getRuleId());
        assertEquals(7, input.getConfigurationVersion());
        assertEquals(Instant.MAX, input.getDeadline());
        StepVerifier.create(tool.invoke(input)).assertNext(result -> {
            assertEquals("SHIPPED", result.get("status").getAsString());
            assertEquals("request-agent-a", result.get("requestId").getAsString());
            result.addProperty("status", "mutated");
        }).verifyComplete();
        StepVerifier.create(tool.invoke(input)).assertNext(result -> assertEquals("SHIPPED", result.get("status").getAsString())).verifyComplete();
    }

    @Test
    void shouldIsolateConcurrentOwners() {
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> {
            String subject = index % 2 == 0 ? "agent-a" : "agent-b";
            String order = index % 2 == 0 ? "demo-A-001" : "demo-B-001";
            return tool.invoke(input(subject, order)).doOnNext(result -> {
                assertEquals(order, result.get("orderId").getAsString());
                assertEquals("request-" + subject, result.get("requestId").getAsString());
            });
        })).expectNextCount(32).verifyComplete();
    }

    private AgentToolInvocation input(final String subject, final String order) {
        JsonObject args = new JsonObject();
        args.addProperty("orderId", order);
        tool.validate(args);
        return new AgentToolInvocation("request-" + subject, subject, args, "rule", 7, Instant.MAX);
    }
}
