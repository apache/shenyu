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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptDefinition;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptInvocation;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptProvider;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;

class KeyAuthMcpPromptHttpTest {

    @ParameterizedTest
    @ValueSource(strings = {"json", "sse"})
    void verifiedApiKeysKeepPromptTenantsAndDenialsSeparateFromTools(final String mode) {
        AtomicInteger calls = new AtomicInteger();
        try (KeyAuthMcpTestFixture fixture = new KeyAuthMcpTestFixture(mode,
                List.of(provider("template-a", calls), provider("template-b", calls), provider("blocked", calls)))) {
            for (String tenant : List.of("a", "b")) {
                JsonObject listed = fixture.success("/" + tenant, "key-" + tenant, "prompts/list", "");
                assertEquals(1, listed.getAsJsonObject("result").getAsJsonArray("prompts").size());
                assertEquals("template-" + tenant, listed.getAsJsonObject("result").getAsJsonArray("prompts").get(0).getAsJsonObject().get("name").getAsString());
                JsonObject result = fixture.success("/" + tenant, "key-" + tenant, "prompts/get", "template-" + tenant);
                assertEquals("tenant-" + tenant + ":service", result.getAsJsonObject("result").getAsJsonArray("messages")
                        .get(0).getAsJsonObject().getAsJsonObject("content").get("text").getAsString());
                fixture.request("/" + tenant, "key-" + tenant, "prompts/get", "blocked").expectStatus().isForbidden();
            }
            fixture.request("/a", "key-a", "prompts/get", "template-b").expectStatus().isForbidden();
            fixture.request("/a", "key-a", "prompts/get", "read-a").expectStatus().isForbidden();
            fixture.request("/unbound", "key-unbound", "prompts/get", "template-a").expectStatus().isUnauthorized();
            assertEquals(2, calls.get());
            fixture.success("/a", "key-a", "tools/call", "read-a");
            assertEquals(1, fixture.calls());
        }
    }

    private AgentPromptProvider provider(final String name, final AtomicInteger calls) {
        ObjectMapper json = new ObjectMapper();
        return new AgentPromptProvider() {
            @Override
            public AgentPromptDefinition definition() {
                return new AgentPromptDefinition(json.createObjectNode().put("name", name));
            }

            @Override
            public Mono<ObjectNode> get(final AgentPromptInvocation invocation) {
                return Mono.fromSupplier(() -> {
                    calls.incrementAndGet();
                    ObjectNode result = json.createObjectNode();
                    result.putArray("messages").addObject().put("role", "user").putObject("content")
                            .put("type", "text").put("text", invocation.tenant() + ":" + invocation.subject());
                    return result;
                }).subscribeOn(Schedulers.parallel());
            }
        };
    }
}
