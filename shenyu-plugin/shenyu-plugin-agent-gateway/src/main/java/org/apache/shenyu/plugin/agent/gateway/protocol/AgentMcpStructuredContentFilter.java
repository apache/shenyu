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
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Immutable, bounded deletion plan for fields under structuredContent.
 * This primitive does not rewrite content text, metadata, or tool schemas.
 */
final class AgentMcpStructuredContentFilter {

    private static final int MAX_RESULT_DEPTH = 32;

    private static final int MAX_RESULT_NODES = 16384;

    private static final long MAX_RESULT_CHARACTERS = 2097152;

    private final List<List<String>> paths;

    AgentMcpStructuredContentFilter(final List<String> fieldPaths) {
        paths = AgentGatewayResultFilterConfig.compilePaths(fieldPaths);
    }

    AgentMcpStructuredContentFilter(final AgentGatewayResultFilterConfig config, final String toolName) {
        paths = config.getPlan(toolName);
    }

    /**
     * Apply the plan to a private result copy; an empty plan leaves its input untouched.
     * Missing fields are no-ops, but incompatible traversal fails without raw fallback.
     *
     * @param result native MCP tool result, not a JSON-RPC envelope
     * @return unchanged input for an empty plan, otherwise a request-owned result copy
     */
    ObjectNode filter(final ObjectNode result) {
        Objects.requireNonNull(result, "result");
        if (paths.isEmpty()) {
            return result;
        }
        checkBudget(result, 0, new AtomicInteger(), new AtomicLong());
        ObjectNode filtered = result.deepCopy();
        JsonNode structured = filtered.get("structuredContent");
        if (Objects.isNull(structured)) {
            return filtered;
        }
        if (!structured.isObject()) {
            throw invalid("structuredContent must be an object");
        }
        for (List<String> path : paths) {
            remove((ObjectNode) structured, path);
        }
        return filtered;
    }

    private void remove(final ObjectNode structured, final List<String> tokens) {
        JsonNode parent = structured;
        for (int index = 0; index < tokens.size() - 1; index++) {
            if (parent.isObject()) {
                parent = parent.get(tokens.get(index));
            } else if (parent.isArray()) {
                parent = parent.get(arrayIndex(tokens.get(index)));
            } else {
                throw invalid("Field deletion cannot traverse a scalar value");
            }
            if (Objects.isNull(parent)) {
                return;
            }
        }
        if (!parent.isObject()) {
            throw invalid("Field deletion target must be an object field");
        }
        ((ObjectNode) parent).remove(tokens.get(tokens.size() - 1));
    }

    private int arrayIndex(final String token) {
        if (token.isEmpty() || token.length() > 10 || (token.length() > 1 && token.startsWith("0"))
                || token.chars().anyMatch(character -> character < '0' || character > '9')) {
            throw invalid("Invalid array traversal index");
        }
        try {
            return Integer.parseInt(token);
        } catch (NumberFormatException error) {
            throw invalid("Invalid array traversal index");
        }
    }

    private void checkBudget(final JsonNode node, final int depth, final AtomicInteger nodes, final AtomicLong characters) {
        if (depth > MAX_RESULT_DEPTH || nodes.incrementAndGet() > MAX_RESULT_NODES) {
            throw invalid("Tool result exceeds the deletion traversal budget");
        }
        if (node.isPojo() || node.isBinary() || node.isMissingNode()) {
            throw invalid("Tool result contains an unsupported node");
        }
        if (node.isObject()) {
            node.fieldNames().forEachRemaining(name -> countCharacters(name.length(), characters));
        } else if (node.isValueNode()) {
            countCharacters(node.asText().length(), characters);
        }
        for (JsonNode child : node) {
            checkBudget(child, depth + 1, nodes, characters);
        }
    }

    private void countCharacters(final int length, final AtomicLong characters) {
        if (characters.addAndGet(length) > MAX_RESULT_CHARACTERS) {
            throw invalid("Tool result exceeds the deletion character budget");
        }
    }

    private IllegalArgumentException invalid(final String message) {
        return new IllegalArgumentException(message);
    }
}
