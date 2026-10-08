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

package org.apache.shenyu.admin.validation.validator;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;

import java.util.Objects;
import java.util.Set;

/**
 * Validates agent gateway rule handles before Admin stores them.
 */
public final class AgentGatewayRuleHandleValidator {

    private static final String TRAFFIC_TYPE = "trafficType";

    private static final String RESPONSE_REQUEST_ID = "responseRequestId";

    private static final Set<String> SUPPORTED_FIELDS = Set.of(TRAFFIC_TYPE, RESPONSE_REQUEST_ID);

    private AgentGatewayRuleHandleValidator() {
    }

    /**
     * Validate the complete rule handle, including unknown fields.
     *
     * @param handle rule handle supplied to Admin
     */
    public static void validate(final String handle) {
        if (StringUtils.isBlank(handle)) {
            throw new ShenyuAdminException("agentGateway rule handle must not be empty");
        }
        final JsonElement element;
        try {
            element = JsonParser.parseString(handle);
        } catch (JsonParseException | IllegalStateException ex) {
            throw new ShenyuAdminException("agentGateway rule handle must be valid JSON");
        }
        if (!element.isJsonObject()) {
            throw new ShenyuAdminException("agentGateway rule handle must be a JSON object");
        }
        final JsonObject object = element.getAsJsonObject();
        for (String field : object.keySet()) {
            if (!SUPPORTED_FIELDS.contains(field)) {
                throw new ShenyuAdminException("agentGateway rule handle contains an unknown field; supported fields: trafficType, responseRequestId");
            }
        }
        final JsonElement trafficType = object.get(TRAFFIC_TYPE);
        if (Objects.isNull(trafficType) || !trafficType.isJsonPrimitive() || !trafficType.getAsJsonPrimitive().isString()
                || !"LLM".equals(trafficType.getAsString())) {
            throw new ShenyuAdminException("agentGateway trafficType must be LLM");
        }
        if (object.has(RESPONSE_REQUEST_ID)) {
            final JsonElement responseRequestId = object.get(RESPONSE_REQUEST_ID);
            if (!responseRequestId.isJsonPrimitive() || !responseRequestId.getAsJsonPrimitive().isBoolean()) {
                throw new ShenyuAdminException("agentGateway responseRequestId must be a boolean");
            }
        }
    }
}
