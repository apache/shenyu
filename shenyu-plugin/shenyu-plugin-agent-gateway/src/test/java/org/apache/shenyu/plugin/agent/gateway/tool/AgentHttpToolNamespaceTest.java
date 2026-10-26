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
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Request-local HTTP registrations do not thaw or overwrite startup-local metadata.
 */
class AgentHttpToolNamespaceTest {

    @Test
    void retainsTheOriginalFrozenMetadataWhenAddingAnHttpTool() {
        MutableProvider provider = new MutableProvider();
        AgentToolRegistry registry = new AgentToolRegistry(List.of(provider));
        provider.schema.addProperty("type", "invalid-after-registration");
        AgentToolRegistry captured = registry.withProviders(List.of(httpProvider("http_lookup")));
        assertEquals("object", captured.list(Set.of("local")).get("local").get("type").getAsString());
        assertFalse(registry.isRegistered("http_lookup"));
        assertEquals(2, captured.listDefinitions(Set.of("local", "http_lookup")).size());
    }

    @Test
    void rejectsCollisionsWithoutReplacingTheOriginalProvider() {
        AgentToolRegistry registry = new AgentToolRegistry(List.of(new MutableProvider()));
        assertThrows(IllegalArgumentException.class, () -> registry.withProviders(List.of(httpProvider("local"))));
        assertEquals(1, registry.listDefinitions(Set.of("local")).size());
    }

    private AgentHttpToolProvider httpProvider(final String name) {
        JsonObject source = JsonParser.parseString("""
                {"description":"HTTP lookup","inputSchema":{"type":"object","properties":{}},
                 "requestConfig":{"requestTemplate":{"method":"GET","url":"/orders/status"},"argsPosition":{}}}
                """).getAsJsonObject();
        JsonObject plans = new JsonObject();
        plans.add(name, source);
        return new AgentHttpToolProvider(AgentGatewayHttpToolConfig.parse(plans, Set.of(name)).getTools().get(name), null);
    }

    private static final class MutableProvider implements AgentToolProvider {

        private final JsonObject schema = JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject();

        @Override
        public String getName() {
            return "local";
        }

        @Override
        public String getDescription() {
            return "Startup local provider";
        }

        @Override
        public JsonObject getInputSchema() {
            return schema;
        }

        @Override
        public void validate(final JsonObject arguments) {
        }

        @Override
        public Mono<JsonObject> invoke(final AgentToolInvocation invocation) {
            return Mono.just(invocation.getArguments());
        }
    }
}
