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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.utils.AgentPromptNames;

import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Frozen bounded prompt metadata shared by local and remote registrations. */
public final class AgentPromptDefinition {

    private final ObjectNode metadata;

    public AgentPromptDefinition(final ObjectNode source) {
        Objects.requireNonNull(source, "metadata");
        text(source.get("name"), AgentPromptNames.MAX_EXPOSED_LENGTH);
        if (!AgentPromptNames.exposed(source.path("name").asText())) {
            throw new IllegalArgumentException("Invalid prompt name");
        }
        if (source.has("description")) {
            description(source.get("description"));
        }
        if (source.toString().getBytes(StandardCharsets.UTF_8).length > 16384) {
            throw new IllegalArgumentException("Prompt metadata exceeds limit");
        }
        Set<String> names = new HashSet<>();
        if (source.has("arguments")) {
            if (!source.get("arguments").isArray() || source.get("arguments").size() > 32) {
                throw new IllegalArgumentException("Invalid prompt arguments");
            }
            for (JsonNode argument : source.get("arguments")) {
                text(argument.get("name"), 128);
                if (!names.add(argument.get("name").textValue()) || argument.has("required") && !argument.get("required").isBoolean()) {
                    throw new IllegalArgumentException("Invalid prompt argument");
                }
                if (argument.has("description")) {
                    description(argument.get("description"));
                }
            }
        }
        metadata = source.deepCopy();
    }

    public String name() {
        return metadata.get("name").textValue();
    }

    public ObjectNode metadata() {
        return metadata.deepCopy();
    }

    public void validate(final Map<String, String> arguments) {
        if (arguments.size() > 32) {
            throw new IllegalArgumentException("Too many prompt arguments");
        }
        Set<String> names = new HashSet<>();
        if (metadata.has("arguments")) {
            for (JsonNode declaration : metadata.get("arguments")) {
                String name = declaration.get("name").textValue();
                names.add(name);
                if (declaration.path("required").asBoolean(false) && !arguments.containsKey(name)) {
                    throw new IllegalArgumentException("Missing required prompt argument");
                }
            }
        }
        if (!names.containsAll(arguments.keySet()) || arguments.values().stream().anyMatch(value -> Objects.isNull(value) || value.length() > 16384)) {
            throw new IllegalArgumentException("Invalid prompt arguments");
        }
    }

    public static ObjectNode validateResult(final ObjectNode result) {
        if (!result.path("messages").isArray() || result.path("messages").isEmpty() || result.get("messages").size() > 64
                || result.toString().getBytes(StandardCharsets.UTF_8).length > 1048576) {
            throw new IllegalArgumentException("Invalid prompt result");
        }
        if (result.has("description")) {
            description(result.get("description"));
        }
        for (JsonNode message : result.get("messages")) {
            if (!Set.of("user", "assistant").contains(message.path("role").asText())
                    || !message.path("content").isObject() || !"text".equals(message.path("content").path("type").asText())
                    || !message.path("content").path("text").isTextual()) {
                throw new IllegalArgumentException("Unsupported prompt message");
            }
        }
        return result.deepCopy();
    }

    private static void description(final JsonNode value) {
        if (!value.isTextual() || value.textValue().length() > 4096) {
            throw new IllegalArgumentException("Invalid prompt description");
        }
    }

    private static void text(final JsonNode value, final int maximum) {
        if (Objects.isNull(value) || !value.isTextual() || value.textValue().isBlank() || value.textValue().length() > maximum
                || !value.textValue().equals(value.textValue().strip()) || value.textValue().chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Invalid prompt metadata");
        }
    }
}
