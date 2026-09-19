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

package org.apache.shenyu.plugin.agent.gateway.handle;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import org.apache.commons.lang3.StringUtils;

import java.util.Set;

/**
 * Strict parser for agent gateway rule handles.
 */
public final class AgentGatewayRuleHandleParser {

    private static final String TRAFFIC_TYPE = "trafficType";

    private static final String RESPONSE_REQUEST_ID = "responseRequestId";

    private static final Set<String> SUPPORTED_FIELDS = Set.of(TRAFFIC_TYPE, RESPONSE_REQUEST_ID);

    /**
     * Parse one rule handle without silently accepting unknown fields.
     *
     * @param rawHandle raw rule handle
     * @return parsed handle or an invalid handle with a stable reason
     */
    public AgentGatewayRuleHandle parse(final String rawHandle) {
        if (StringUtils.isBlank(rawHandle)) {
            return AgentGatewayRuleHandle.invalid(rawHandle, "rule handle must not be empty");
        }
        try {
            final JsonElement element = JsonParser.parseString(rawHandle);
            if (!element.isJsonObject()) {
                return invalid(rawHandle, "rule handle must be a JSON object");
            }
            final JsonObject object = element.getAsJsonObject();
            for (String field : object.keySet()) {
                if (!SUPPORTED_FIELDS.contains(field)) {
                    return invalid(rawHandle, "unknown field: " + field);
                }
            }
            if (!object.has(TRAFFIC_TYPE) || !object.get(TRAFFIC_TYPE).isJsonPrimitive()
                    || !object.getAsJsonPrimitive(TRAFFIC_TYPE).isString()) {
                return invalid(rawHandle, "trafficType must be a string");
            }
            final String trafficType = object.get(TRAFFIC_TYPE).getAsString();
            if (!"LLM".equals(trafficType)) {
                return invalid(rawHandle, "trafficType must be LLM");
            }
            boolean responseRequestId = false;
            if (object.has(RESPONSE_REQUEST_ID)) {
                final JsonElement responseElement = object.get(RESPONSE_REQUEST_ID);
                if (!responseElement.isJsonPrimitive()
                        || !responseElement.getAsJsonPrimitive().isBoolean()) {
                    return invalid(rawHandle, "responseRequestId must be a boolean");
                }
                responseRequestId = responseElement.getAsBoolean();
            }
            return AgentGatewayRuleHandle.valid(rawHandle, "LLM", responseRequestId);
        } catch (JsonParseException | IllegalStateException | UnsupportedOperationException ex) {
            return invalid(rawHandle, "rule handle is not valid JSON");
        }
    }

    private AgentGatewayRuleHandle invalid(final String rawHandle, final String message) {
        return AgentGatewayRuleHandle.invalid(rawHandle, message);
    }
}
