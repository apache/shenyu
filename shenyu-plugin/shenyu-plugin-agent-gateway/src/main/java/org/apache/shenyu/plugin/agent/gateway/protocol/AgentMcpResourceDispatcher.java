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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceDefinition;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceRegistry;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/** Local and pinned remote resource dispatch with independent resource authorization. */
final class AgentMcpResourceDispatcher {

    private final AgentResourceRegistry registry;

    private final ObjectMapper mapper = new ObjectMapper();

    AgentMcpResourceDispatcher(final AgentResourceRegistry registry) {
        this.registry = Objects.requireNonNull(registry, "resources");
    }

    void validate(final AgentMcpExecutionContext context, final AgentMcpRemoteCatalog.Snapshot snapshot) {
        Map<String, ObjectNode> remote = remote(snapshot);
        if (remote.size() + registry.getNames().size() > 32 || remote.keySet().stream().anyMatch(registry.getNames()::contains)) {
            throw new IllegalStateException("Resource namespace collision or limit");
        }
        for (String name : context.getRuleResources()) {
            if (!registry.getNames().contains(name) && !remote.containsKey(name)) {
                throw new IllegalStateException("Configured resource is not registered");
            }
        }
    }

    Mono<ObjectNode> dispatch(final AgentMcpRequest request, final AgentMcpExecutionContext context, final AgentMcpRemoteCatalog.Snapshot snapshot) {
        Map<String, ObjectNode> remote = remote(snapshot);
        if (!"resources/read".equals(request.getMethod())) {
            if (request.getParams().has("cursor")) {
                return Mono.error(new AgentMcpProtocolException(400, -32602, "Cursor is not supported by this complete resource list", request.getId(), null));
            }
            boolean templates = "resources/templates/list".equals(request.getMethod());
            ObjectNode result = mapper.createObjectNode();
            ArrayNode values = result.putArray(templates ? "resourceTemplates" : "resources");
            registry.list(context.getAllowedResources(), templates).forEach(value -> {
                try {
                    values.add(mapper.readTree(value.toString()));
                } catch (java.io.IOException error) {
                    throw new IllegalStateException("Invalid resource metadata", error);
                }
            });
            remote.forEach((name, metadata) -> {
                if (context.getAllowedResources().contains(name) && metadata.has("uriTemplate") == templates) {
                    values.add(metadata.deepCopy());
                }
            });
            return Mono.just(result);
        }
        String uri = request.getParams().path("uri").textValue();
        Map<String, AgentResourceDefinition> matches = new HashMap<>();
        registry.getDefinitions().forEach((name, definition) -> {
            if (context.getAllowedResources().contains(name) && definition.matches(uri)) {
                matches.put(name, definition);
            }
        });
        remote.forEach((name, metadata) -> {
            if (context.getAllowedResources().contains(name)) {
                AgentResourceDefinition definition = definition(name, metadata);
                if (definition.matches(uri)) {
                    matches.put(name, definition);
                }
            }
        });
        if (matches.size() != 1) {
            return Mono.error(new AgentMcpProtocolException(403, -32002, "Resource is not available", request.getId(), null));
        }
        if (remote.containsKey(matches.keySet().iterator().next())) {
            return snapshot.readResource(uri, context);
        }
        return registry.read(uri, context.getAllowedResources(), () -> new AgentToolInvocation(context.getRequestId(), context.getSubject(), new JsonObject(),
                        context.getRuleId(), context.getConfigurationVersion(), context.getDeadline(), request.getClientCapabilities()))
                .map(contents -> {
                    try {
                        ObjectNode result = mapper.createObjectNode();
                        result.set("contents", mapper.readTree(contents.toString()));
                        return result;
                    } catch (java.io.IOException error) {
                        throw new IllegalStateException("Invalid resource contents", error);
                    }
                });
    }

    private AgentResourceDefinition definition(final String name, final ObjectNode metadata) {
        boolean template = metadata.has("uriTemplate");
        return new AgentResourceDefinition(name, metadata.path(template ? "uriTemplate" : "uri").textValue(),
                metadata.path("description").asText(""), metadata.has("mimeType") ? metadata.get("mimeType").textValue() : null, template);
    }

    private Map<String, ObjectNode> remote(final AgentMcpRemoteCatalog.Snapshot snapshot) {
        return Objects.isNull(snapshot) ? Map.of() : snapshot.resourceDefinitions();
    }
}
