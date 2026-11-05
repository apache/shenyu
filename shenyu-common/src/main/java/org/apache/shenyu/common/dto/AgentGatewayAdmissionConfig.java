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

import com.fasterxml.jackson.databind.JsonNode;

import java.util.Objects;

/**
 * Strict fixed resource-admission limits shared by Admin and the gateway.
 */
public record AgentGatewayAdmissionConfig(int globalInFlight, int globalQueued,
                                          int localInFlight, int localQueued,
                                          int remoteInFlight, int remoteQueued) {

    private static final AgentGatewayAdmissionConfig DEFAULT = new AgentGatewayAdmissionConfig(128, 64, 32, 16, 8, 16);

    public AgentGatewayAdmissionConfig {
        range("globalInFlight", globalInFlight, 1, 128);
        range("globalQueued", globalQueued, 0, 256);
        range("localInFlight", localInFlight, 1, 64);
        range("localQueued", localQueued, 0, 128);
        range("remoteInFlight", remoteInFlight, 1, 32);
        range("remoteQueued", remoteQueued, 0, 128);
    }

    /**
     * Parse the optional trusted plugin field. Null selects safe operational defaults.
     * @param node admission object from plugin configuration
     * @return immutable admission limits
     */
    public static AgentGatewayAdmissionConfig parse(final JsonNode node) {
        if (Objects.isNull(node)) {
            return DEFAULT;
        }
        if (!node.isObject() || node.isEmpty()) {
            return DEFAULT;
        }
        java.util.Set<String> fields = new java.util.HashSet<>();
        node.fieldNames().forEachRemaining(fields::add);
        if (!fields.equals(java.util.Set.of("globalInFlight", "globalQueued", "localInFlight",
                "localQueued", "remoteInFlight", "remoteQueued"))) {
            throw new IllegalArgumentException("Unknown or missing admission field");
        }
        return new AgentGatewayAdmissionConfig(number(node, "globalInFlight"), number(node, "globalQueued"),
                number(node, "localInFlight"), number(node, "localQueued"),
                number(node, "remoteInFlight"), number(node, "remoteQueued"));
    }

    private static int number(final JsonNode node, final String field) {
        if (!node.path(field).isInt()) {
            throw new IllegalArgumentException("Admission field must be an integer");
        }
        return node.get(field).asInt();
    }

    private static void range(final String field, final int value, final int minimum, final int maximum) {
        if (value < minimum || value > maximum) {
            throw new IllegalArgumentException("Admission field is outside its hard maximum: " + field);
        }
    }
}
