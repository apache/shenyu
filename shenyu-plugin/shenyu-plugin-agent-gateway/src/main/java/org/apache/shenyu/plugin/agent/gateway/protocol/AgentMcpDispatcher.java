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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolArgumentException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolCapabilityException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolDefinition;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolExecutionException;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * Request-local discover/list/call dispatch, without HTTP or security-source adaptation.
 */
public final class AgentMcpDispatcher {

    private final AgentToolRegistry registry;

    private final String serverName;

    private final String serverVersion;

    private final ObjectMapper mapper = new ObjectMapper();

    public AgentMcpDispatcher(final AgentToolRegistry registry, final String serverName, final String serverVersion) {
        this.registry = Objects.requireNonNull(registry, "registry");
        if (Objects.requireNonNull(serverName, "serverName").isBlank() || Objects.requireNonNull(serverVersion, "serverVersion").isBlank()) {
            throw new IllegalArgumentException("Server identity must not be blank");
        }
        this.serverName = serverName;
        this.serverVersion = serverVersion;
    }

    /**
     * Dispatch lazily with fresh trusted context for each subscription, without retry.
     * The factory must obtain identity and grants from server-side security, not request metadata.
     *
     * @param request a parsed, validated request
     * @param contextFactory request-local trusted context factory
     * @return one JSON-RPC response or a typed protocol failure, linked to provider cancellation
     */
    public Mono<ObjectNode> dispatch(final AgentMcpRequest request, final Supplier<AgentMcpExecutionContext> contextFactory) {
        Objects.requireNonNull(request, "request");
        Objects.requireNonNull(contextFactory, "contextFactory");
        return Mono.defer(() -> {
            AgentMcpExecutionContext context = Objects.requireNonNull(contextFactory.get(), "execution context");
            if (!registry.listDefinitions(context.getRuleTools()).keySet().containsAll(context.getRuleTools())) {
                return Mono.error(failure(request, 500, -32603, "Configured tool is not registered"));
            }
            return route(request, context).contextWrite(reactorContext -> reactorContext.put(AgentMcpExecutionContext.class, context));
        }).onErrorMap(error -> {
            if (error instanceof AgentMcpProtocolException) {
                return error;
            }
            AgentMcpProtocolException failure = failure(request, 500, -32603, "Internal error");
            failure.initCause(error);
            return failure;
        });
    }

    private Mono<ObjectNode> route(final AgentMcpRequest request, final AgentMcpExecutionContext context) {
        switch (request.getMethod()) {
            case "server/discover":
                ObjectNode discovery = complete();
                cacheMetadata(discovery);
                discovery.putArray("supportedVersions").add(AgentMcpRequestParser.VERSION);
                discovery.putObject("capabilities").putObject("tools");
                return Mono.fromSupplier(() -> response(request, discovery.deepCopy()));
            case "tools/list":
                return Mono.fromSupplier(() -> list(request, context));
            case "tools/call":
                return call(request, context);
            default:
                return Mono.error(failure(request, 404, -32601, "Method not found"));
        }
    }

    private ObjectNode list(final AgentMcpRequest request, final AgentMcpExecutionContext context) {
        if (request.getParams().has("cursor")) {
            throw failure(request, 400, -32602, "Cursor is not supported by this unpaginated tool list");
        }
        ObjectNode result = complete();
        cacheMetadata(result);
        ArrayNode tools = result.putArray("tools");
        registry.listDefinitions(context.getAllowedTools()).forEach((name, definition) -> {
            ObjectNode tool = tools.addObject().put("name", name).put("description", definition.getDescription());
            tool.set("inputSchema", toTree(definition.getInputSchema()));
        });
        return response(request, result);
    }

    private Mono<ObjectNode> call(final AgentMcpRequest request, final AgentMcpExecutionContext context) {
        ObjectNode params = request.getParams();
        String name = params.get("name").textValue();
        AgentToolDefinition definition = registry.listDefinitions(context.getAllowedTools()).get(name);
        if (Objects.isNull(definition)) {
            return Mono.error(failure(request, 403, -32602, "Tool is not available"));
        }
        JsonObject arguments = params.has("arguments") ? JsonParser.parseString(params.get("arguments").toString()).getAsJsonObject() : new JsonObject();
        JsonObject capabilities = request.getClientCapabilities();
        return registry.invoke(name, context.getAllowedTools(), () -> new AgentToolInvocation(context.getRequestId(), context.getSubject(), arguments,
                        context.getRuleId(), context.getConfigurationVersion(), context.getDeadline(), capabilities))
                .map(value -> {
                    ObjectNode result = complete();
                    JsonNode content = toTree(value);
                    result.set("structuredContent", content);
                    result.putArray("content").addObject().put("type", "text").put("text", content.toString());
                    result.put("isError", false);
                    return response(request, result);
                })
                .onErrorResume(AgentToolArgumentException.class, error -> Mono.just(toolFailure(request, "Invalid tool arguments")))
                .onErrorResume(AgentToolExecutionException.class, error -> Mono.just(toolFailure(request, "Tool execution failed")))
                .onErrorMap(AgentToolCapabilityException.class, error -> {
                    ObjectNode data = mapper.createObjectNode();
                    data.set("requiredCapabilities", toTree(error.getRequiredCapabilities()));
                    return new AgentMcpProtocolException(400, -32021, "Missing required client capability", request.getId(), data);
                })
                .onErrorMap(SecurityException.class, error -> failure(request, 403, -32602, "Tool is not available"));
    }

    private ObjectNode toolFailure(final AgentMcpRequest request, final String message) {
        ObjectNode result = complete();
        result.putArray("content").addObject().put("type", "text").put("text", message);
        result.put("isError", true);
        return response(request, result);
    }

    private ObjectNode complete() {
        ObjectNode result = mapper.createObjectNode().put("resultType", "complete");
        result.putObject("_meta").putObject("io.modelcontextprotocol/serverInfo").put("name", serverName).put("version", serverVersion);
        return result;
    }

    private void cacheMetadata(final ObjectNode result) {
        result.put("ttlMs", 0).put("cacheScope", "private");
    }

    private ObjectNode response(final AgentMcpRequest request, final ObjectNode result) {
        ObjectNode response = mapper.createObjectNode().put("jsonrpc", "2.0");
        response.set("id", request.getId());
        response.set("result", result);
        return response;
    }

    private JsonNode toTree(final JsonObject value) {
        try {
            return mapper.readTree(value.toString());
        } catch (JsonProcessingException error) {
            throw new IllegalStateException("Tool JSON cannot be encoded", error);
        }
    }

    private AgentMcpProtocolException failure(final AgentMcpRequest request, final int status, final int code, final String message) {
        return new AgentMcpProtocolException(status, code, message, request.getId(), null);
    }
}
