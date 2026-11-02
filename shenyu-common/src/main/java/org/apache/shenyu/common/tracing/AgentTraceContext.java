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

package org.apache.shenyu.common.tracing;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * SDK-independent keys and bounded snapshots for explicitly propagated Agent trace headers.
 * These values are telemetry, never authentication or authorization input.
 */
public final class AgentTraceContext {

    /**
     * Existing TracingConstants.REACTOR_CONTEXT_KEY, shared without depending on the tracing plugin.
     */
    public static final String GATEWAY_REACTOR_CONTEXT_KEY = "org.apache.shenyu.plugin.tracing.context";

    /**
     * Immutable logical-call headers carried in Reactor context and McpTransportContext.
     */
    public static final String HEADERS_KEY = "org.apache.shenyu.agent.trace.headers";

    private static final Pattern TRACEPARENT = Pattern.compile("00-[0-9a-f]{32}-[0-9a-f]{16}-[0-9a-f]{2}");

    private static final Pattern STATE_KEY = Pattern.compile("(?:[a-z][a-z0-9_*/-]{0,255}|[a-z0-9][a-z0-9_*/-]{0,240}@[a-z][a-z0-9_*/-]{0,13})");

    private AgentTraceContext() {
    }

    /**
     * Snapshot only locally encoded W3C version-00 trace headers, excluding all other entries.
     * Invalid telemetry is omitted instead of breaking an application request.
     * @param value explicit header carrier, never the inbound HTTP headers
     * @return immutable traceparent and optional tracestate, or an empty map
     */
    public static Map<String, String> headers(final Object value) {
        if (!(value instanceof Map<?, ?> headers) || !(headers.get("traceparent") instanceof String parent)
                || !TRACEPARENT.matcher(parent).matches()
                || "00000000000000000000000000000000".equals(parent.substring(3, 35))
                || "0000000000000000".equals(parent.substring(36, 52))) {
            return Map.of();
        }
        Object state = headers.get("tracestate");
        if (state instanceof String text && validState(text)) {
            return Map.of("traceparent", parent, "tracestate", text);
        }
        return Map.of("traceparent", parent);
    }

    private static boolean validState(final String state) {
        if (state.isEmpty() || state.length() > 512 || state.chars().anyMatch(character -> character < 32 || character > 126)) {
            return false;
        }
        String[] entries = state.split(",", -1);
        if (entries.length > 32) {
            return false;
        }
        Set<String> keys = new HashSet<>();
        for (String entry : entries) {
            String member = entry.trim();
            int separator = member.indexOf('=');
            if (separator < 1 || separator != member.lastIndexOf('=') || separator == member.length() - 1
                    || member.length() - separator - 1 > 256) {
                return false;
            }
            String key = member.substring(0, separator);
            if (!STATE_KEY.matcher(key).matches() || !keys.add(key)) {
                return false;
            }
        }
        return true;
    }
}
