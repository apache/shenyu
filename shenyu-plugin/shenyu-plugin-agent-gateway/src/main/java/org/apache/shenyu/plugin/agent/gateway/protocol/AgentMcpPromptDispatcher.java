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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptDefinition;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptInvocation;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptRegistry;
import org.apache.shenyu.common.utils.AgentPromptNames;
import reactor.core.publisher.Mono;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

/** Prompt-specific authorization and dispatch on one pinned catalog generation. */
final class AgentMcpPromptDispatcher {

    private final AgentPromptRegistry local;

    private final ObjectMapper json = new ObjectMapper();

    AgentMcpPromptDispatcher(final AgentPromptRegistry local) {
        this.local = Objects.requireNonNull(local, "prompts");
    }

    Mono<ObjectNode> dispatch(final AgentMcpRequest request, final AgentMcpExecutionContext context,
                              final AgentMcpRemoteCatalog.Snapshot snapshot, final AgentMcpResourceAdmission admission) {
        Map<String, AgentPromptDefinition> definitions = new TreeMap<>(local.definitions());
        Map<String, ObjectNode> remote = Objects.isNull(snapshot) ? Map.of() : snapshot.promptDefinitions();
        remote.forEach((name, metadata) -> {
            AgentPromptDefinition definition = new AgentPromptDefinition(metadata);
            if (!AgentPromptNames.remote(name)
                    || !name.equals(definition.name()) || Objects.nonNull(definitions.putIfAbsent(name, definition))) {
                throw new IllegalStateException("Prompt namespace collision");
            }
        });
        if (definitions.size() > 32 || !definitions.keySet().containsAll(context.getRulePrompts())) {
            return Mono.error(new IllegalStateException("Configured prompt is not registered"));
        }
        if ("prompts/list".equals(request.getMethod())) {
            if (request.getParams().has("cursor")) {
                return Mono.error(failure(request, 400, -32602, "Cursor is not supported by this complete prompt list"));
            }
            ObjectNode result = json.createObjectNode();
            var prompts = result.putArray("prompts");
            definitions.forEach((name, definition) -> {
                if (context.getAllowedPrompts().contains(name)) {
                    prompts.add(definition.metadata());
                }
            });
            return Mono.just(result);
        }
        String name = request.getParams().path("name").textValue();
        if (!context.getAllowedPrompts().contains(name) || !definitions.containsKey(name)) {
            return Mono.error(failure(request, 403, -32002, "Prompt is not available"));
        }
        Map<String, String> arguments = new LinkedHashMap<>();
        JsonNode supplied = request.getParams().get("arguments");
        if (Objects.nonNull(supplied)) {
            supplied.fields().forEachRemaining(entry -> arguments.put(entry.getKey(), entry.getValue().textValue()));
        }
        try {
            definitions.get(name).validate(arguments);
        } catch (IllegalArgumentException error) {
            return Mono.error(failure(request, 400, -32602, "Invalid prompt arguments"));
        }
        Mono<ObjectNode> execution;
        if (remote.containsKey(name)) {
            execution = admission.withRemote(name.substring(0, name.indexOf('.')), context.getDeadline(),
                    Mono.defer(() -> snapshot.getPrompt(name, arguments, context)));
        } else {
            execution = admission.withLocal(context.getDeadline(), local.get(name, context.getAllowedPrompts(),
                    new AgentPromptInvocation(context.getRequestId(), context.getTenant().orElse(null), context.getSubject(), context.getRuleId(),
                            context.getConfigurationVersion(), context.getDeadline(), arguments)));
        }
        return execution.map(AgentPromptDefinition::validateResult)
                .onErrorMap(IllegalArgumentException.class, error -> failure(request, 502, -32603, "Invalid prompt result"))
                .onErrorMap(SecurityException.class, error -> failure(request, 403, -32002, "Prompt is not available"));
    }

    private AgentMcpProtocolException failure(final AgentMcpRequest request, final int status, final int code, final String message) {
        return new AgentMcpProtocolException(status, code, message, request.getId(), null);
    }
}
