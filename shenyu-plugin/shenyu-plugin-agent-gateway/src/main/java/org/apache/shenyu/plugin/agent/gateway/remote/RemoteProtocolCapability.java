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

package org.apache.shenyu.plugin.agent.gateway.remote;

import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig.ProtocolProfile;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRequestParser;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/** Directional, payload-free description of one published upstream binding. */
public record RemoteProtocolCapability(String server, String entry, String ingressVersion, String outboundProfile, String outboundVersion,
                                       List<String> supportedMethods, List<String> supportedResultTypes, List<String> responseModes,
                                       boolean resourcesSupported, List<String> knownTranslationLosses) {

    public RemoteProtocolCapability {
        supportedMethods = List.copyOf(supportedMethods);
        supportedResultTypes = List.copyOf(supportedResultTypes);
        responseModes = List.copyOf(responseModes);
        knownTranslationLosses = List.copyOf(knownTranslationLosses);
    }

    /**
     * Describe the operations supported by the configured protocol adapter.
     * @param server stable name
     * @param profile outbound protocol
     * @param resourcesEnabled whether resource aggregation is enabled
     * @return directional capabilities for a tools-capable target
     */
    public static RemoteProtocolCapability forProfile(final String server, final ProtocolProfile profile, final boolean resourcesEnabled) {
        return forProfile(server, profile, true, resourcesEnabled);
    }

    static RemoteProtocolCapability forProfile(final String server, final ProtocolProfile profile, final boolean toolsSupported, final boolean resourcesEnabled) {
        boolean legacy = profile == ProtocolProfile.LEGACY_2025_06_18;
        boolean resources = legacy && resourcesEnabled;
        List<String> methods = new ArrayList<>(legacy ? List.of("initialize", "notifications/initialized") : List.of("server/discover"));
        if (toolsSupported) {
            methods.addAll(List.of("tools/list", "tools/call"));
        }
        if (resources) {
            methods.addAll(List.of("resources/list", "resources/templates/list", "resources/read"));
        }
        return new RemoteProtocolCapability(server, "agentGateway.mcp", AgentMcpRequestParser.VERSION, profile.id(), profile.version(),
                methods, toolsSupported ? List.of("complete") : List.of(), List.of("json", "sse"), resources,
                legacy ? List.of("Legacy session handshake", "Tool results translated to complete", "Single final response")
                        : List.of("Single final response", "No input_required continuation", "No remote resources",
                                "No x-mcp-header argument mirroring (Mcp-Param-*)", "Tool listings omit unsupported display fields and schema annotations"));
    }

    /**
     * Export only protocol features and the stable configured server name.
     * @return immutable diagnostic fields
     */
    public Map<String, Object> toMap() {
        return Map.of("server", server, "entry", entry, "ingressVersion", ingressVersion, "outboundProfile", outboundProfile,
                "outboundVersion", outboundVersion, "supportedMethods", supportedMethods, "supportedResultTypes", supportedResultTypes,
                "responseModes", responseModes, "resourcesSupported", resourcesSupported, "knownTranslationLosses", knownTranslationLosses);
    }
}
