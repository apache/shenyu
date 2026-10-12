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

package org.apache.shenyu.common.dto;

import com.google.gson.JsonElement;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Shared immutable, explicit per-tool structuredContent field-deletion plans.
 * Paths are relative to structuredContent; no content text or schema is rewritten.
 */
public final class AgentGatewayResultFilterConfig {

    private final Map<String, List<List<String>>> plans;

    private AgentGatewayResultFilterConfig(final Map<String, List<List<String>>> values) {
        plans = Map.copyOf(values);
    }

    /**
     * Return an inactive configuration.
     * @return empty immutable plans
     */
    public static AgentGatewayResultFilterConfig empty() {
        return new AgentGatewayResultFilterConfig(Map.of());
    }

    /**
     * Parse bounded plans consistently for Admin and gateway rule snapshots.
     * @param element tool-name to path-array object, or absent value
     * @param allowedTools explicit rule tool allowlist
     * @return immutable compiled configuration
     */
    public static AgentGatewayResultFilterConfig parse(final JsonElement element, final Set<String> allowedTools) {
        Objects.requireNonNull(allowedTools, "allowedTools");
        if (Objects.isNull(element)) {
            return empty();
        }
        if (!element.isJsonObject() || element.getAsJsonObject().size() > 32) {
            throw invalid();
        }
        Map<String, List<List<String>>> plans = new LinkedHashMap<>();
        int total = 0;
        for (Map.Entry<String, JsonElement> entry : element.getAsJsonObject().entrySet()) {
            if (!allowedTools.contains(entry.getKey()) || entry.getKey().length() > 128 || !entry.getValue().isJsonArray()
                    || entry.getValue().getAsJsonArray().isEmpty()) {
                throw invalid();
            }
            List<String> paths = new ArrayList<>();
            for (JsonElement path : entry.getValue().getAsJsonArray()) {
                if (!path.isJsonPrimitive() || !path.getAsJsonPrimitive().isString() || ++total > 128) {
                    throw invalid();
                }
                paths.add(path.getAsString());
            }
            plans.put(entry.getKey(), compilePaths(paths));
        }
        return new AgentGatewayResultFilterConfig(plans);
    }

    /**
     * Compile one bounded exact field-deletion plan.
     * @param fieldPaths relative field paths
     * @return deeply immutable decoded path tokens
     */
    public static List<List<String>> compilePaths(final List<String> fieldPaths) {
        if (Objects.isNull(fieldPaths) || fieldPaths.size() > 32) {
            throw invalid();
        }
        Set<String> unique = new HashSet<>();
        List<List<String>> compiled = new ArrayList<>();
        for (String path : fieldPaths) {
            if (Objects.isNull(path) || path.isEmpty() || path.length() > 512 || !path.startsWith("/") || !unique.add(path)) {
                throw invalid();
            }
            String[] segments = path.substring(1).split("/", -1);
            if (segments.length > 16) {
                throw invalid();
            }
            List<String> tokens = new ArrayList<>();
            for (String segment : segments) {
                tokens.add(decode(segment));
            }
            compiled.add(List.copyOf(tokens));
        }
        return List.copyOf(compiled);
    }

    /**
     * Get the immutable plan for one complete exposed tool name.
     * @param toolName complete exposed name, not a split server or suffix
     * @return decoded field paths, or an empty inactive plan
     */
    public List<List<String>> getPlan(final String toolName) {
        return plans.getOrDefault(toolName, List.of());
    }

    private static String decode(final String token) {
        final StringBuilder decoded = new StringBuilder();
        boolean escaped = false;
        for (int index = 0; index < token.length(); index++) {
            char character = token.charAt(index);
            if (escaped) {
                if (character != '0' && character != '1') {
                    throw invalid();
                }
                decoded.append(character == '0' ? '~' : '/');
                escaped = false;
            } else if (character == '~') {
                escaped = true;
            } else {
                decoded.append(character);
            }
        }
        if (escaped) {
            throw invalid();
        }
        return decoded.toString();
    }

    private static IllegalArgumentException invalid() {
        return new IllegalArgumentException("mcp.structuredContentRemovals must contain bounded explicit field-deletion plans for allowed tools");
    }
}
