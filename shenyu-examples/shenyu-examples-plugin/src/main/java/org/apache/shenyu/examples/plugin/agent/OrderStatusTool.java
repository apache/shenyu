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
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Set;

/**
 * Explicitly registered read-only example. Contains synthetic data only.
 */
public final class OrderStatusTool implements AgentToolProvider {

    private static final Map<String, String> ORDERS = Map.of("agent-a", "demo-A-001", "agent-b", "demo-B-001");

    @Override
    public String getName() {
        return "order_status";
    }

    @Override
    public String getDescription() {
        return "Read the status of your synthetic demo order by orderId.";
    }

    @Override
    public JsonObject getInputSchema() {
        return JsonParser.parseString("{\"type\":\"object\",\"properties\":{\"orderId\":{\"type\":\"string\",\"minLength\":1,\"maxLength\":64}},"
                + "\"required\":[\"orderId\"],\"additionalProperties\":false}").getAsJsonObject();
    }

    @Override
    public void validate(final JsonObject arguments) {
        if (!arguments.keySet().equals(Set.of("orderId")) || !arguments.get("orderId").isJsonPrimitive()
                || !arguments.getAsJsonPrimitive("orderId").isString() || !arguments.get("orderId").getAsString().matches("[A-Za-z0-9-]{1,64}")) {
            throw new IllegalArgumentException("An explicit bounded orderId is required");
        }
    }

    @Override
    public Mono<JsonObject> invoke(final AgentToolInvocation input) {
        return Mono.defer(() -> {
            String requested = input.getArguments().get("orderId").getAsString();
            if (!requested.equals(ORDERS.get(input.getSubject()))) {
                return Mono.error(new AgentToolExecutionException("Order is unavailable"));
            }
            JsonObject result = new JsonObject();
            result.addProperty("orderId", requested);
            result.addProperty("status", "SHIPPED");
            result.addProperty("requestId", input.getRequestId());
            return Mono.just(result);
        });
    }
}
