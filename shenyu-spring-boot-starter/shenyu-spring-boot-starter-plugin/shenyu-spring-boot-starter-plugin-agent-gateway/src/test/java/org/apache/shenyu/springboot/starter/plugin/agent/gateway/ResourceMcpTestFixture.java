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
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.remote.RemoteResourceDirectory;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceContent;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceDefinition;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceProvider;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceRegistry;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

/** Shared real local registry and namespaced remote directory for authenticated HTTP tests. */
final class ResourceMcpTestFixture {

    static final String RULE = "[\"local_status\",\"local_template\",\"hidden\",\"orders.status\",\"orders.template\"]";

    static final List<String> GRANTS = List.of("local_status", "local_template", "orders.status", "orders.template", "outside");

    private ResourceMcpTestFixture() {
    }

    static AgentMcpDispatcher dispatcher(final AgentToolRegistry tools) {
        ObjectMapper mapper = new ObjectMapper();
        RemoteResourceDirectory directory = RemoteResourceDirectory.discover(List.of(new RemoteResourceDirectory.Target("orders",
                cursor -> Mono.just(page(mapper, false)), cursor -> Mono.just(page(mapper, true)),
                uri -> {
                    ObjectNode result = mapper.createObjectNode();
                    result.putArray("contents").addObject().put("uri", uri).put("text", "remote");
                    return Mono.just(result);
                }))).block();
        AgentMcpRemoteCatalog remote = new AgentMcpRemoteCatalog() {
            @Override
            public Mono<ObjectNode> withSnapshot(final Function<Snapshot, Mono<ObjectNode>> operation) {
                return Mono.defer(() -> operation.apply(new Snapshot() {
                    @Override
                    public Map<String, ObjectNode> definitions() {
                        return Map.of();
                    }

                    @Override
                    public Map<String, ObjectNode> resourceDefinitions() {
                        return directory.definitions();
                    }

                    @Override
                    public Mono<ObjectNode> readResource(final String uri, final AgentMcpExecutionContext context) {
                        return directory.read(uri, context.getAllowedResources());
                    }

                    @Override
                    public Mono<ObjectNode> invoke(final String name, final JsonObject arguments, final AgentMcpExecutionContext context) {
                        return Mono.error(new AssertionError("No remote tools"));
                    }
                }));
            }
        };
        return new AgentMcpDispatcher(tools, "fixture", "1", remote, new AgentResourceRegistry(List.of(
                provider("local_status", "test://status", false), provider("local_template", "test://record/{id}", true),
                provider("hidden", "test://hidden", false), provider("outside", "test://outside", false))));
    }

    static void assertResources(final java.util.function.BiFunction<String, String,
            org.springframework.test.web.reactive.server.WebTestClient.ResponseSpec> request, final boolean granted) {
        request.apply("resources/list", "").expectStatus().isOk().expectBody(String.class).value(body -> {
            org.junit.jupiter.api.Assertions.assertEquals(granted, body.contains("local_status"));
            org.junit.jupiter.api.Assertions.assertEquals(granted, body.contains("orders.status"));
            org.junit.jupiter.api.Assertions.assertFalse(body.contains("hidden") || body.contains("outside"));
        });
        request.apply("resources/templates/list", "").expectStatus().isOk().expectBody(String.class).value(body -> {
            org.junit.jupiter.api.Assertions.assertEquals(granted, body.contains("local_template"));
            org.junit.jupiter.api.Assertions.assertEquals(granted, body.contains("orders.template"));
        });
        for (String uri : List.of("test://status", "test://record/42", org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceNamespace.expose("orders", "test://remote-status", false),
                org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceNamespace.expose("orders", "test://remote/42", false))) {
            if (granted) {
                request.apply("resources/read", uri).expectStatus().isOk().expectBody(String.class)
                        .value(body -> org.junit.jupiter.api.Assertions.assertTrue(body.contains("contents")));
            } else {
                request.apply("resources/read", uri).expectStatus().isForbidden();
            }
        }
        for (String uri : List.of("test://hidden", "test://outside")) {
            request.apply("resources/read", uri).expectStatus().isForbidden();
        }
    }

    private static ObjectNode page(final ObjectMapper mapper, final boolean template) {
        ObjectNode result = mapper.createObjectNode();
        result.putArray(template ? "resourceTemplates" : "resources").addObject().put("name", template ? "template" : "status")
                .put(template ? "uriTemplate" : "uri", template ? "test://remote/{id}" : "test://remote-status").put("description", "fixture");
        return result;
    }

    private static AgentResourceProvider provider(final String name, final String uri, final boolean template) {
        AgentResourceProvider provider = org.mockito.Mockito.mock(AgentResourceProvider.class);
        org.mockito.Mockito.when(provider.getDefinition()).thenReturn(new AgentResourceDefinition(name, uri, "fixture", null, template));
        org.mockito.Mockito.when(provider.read(org.mockito.ArgumentMatchers.any())).thenAnswer(invocation -> {
            org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceInvocation input = invocation.getArgument(0);
            return Mono.just(List.of(AgentResourceContent.text(input.getUri(), null, "local")));
        });
        return provider;
    }
}
