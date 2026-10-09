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
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

/**
 * Validated protocol data, not a source of trusted identity or permissions.
 */
public final class AgentMcpRequest {

    private final JsonNode id;

    private final String method;

    private final ObjectNode params;

    private final JsonObject clientCapabilities;

    AgentMcpRequest(final JsonNode id, final String method, final ObjectNode params) {
        this.id = id.deepCopy();
        this.method = method;
        this.params = params.deepCopy();
        clientCapabilities = toGson(this.params.path("_meta").path("io.modelcontextprotocol/clientCapabilities")).getAsJsonObject();
    }

    public JsonNode getId() {
        return id.deepCopy();
    }

    public String getMethod() {
        return method;
    }

    public ObjectNode getParams() {
        return params.deepCopy();
    }

    /**
     * Reuse the validated capability snapshot without a JSON text round trip.
     * @return an independent capability object
     */
    public JsonObject getClientCapabilities() {
        return clientCapabilities.deepCopy();
    }

    private static JsonElement toGson(final JsonNode node) {
        if (node.isObject()) {
            JsonObject object = new JsonObject();
            node.fields().forEachRemaining(field -> object.add(field.getKey(), toGson(field.getValue())));
            return object;
        }
        if (node.isArray()) {
            JsonArray array = new JsonArray();
            node.forEach(value -> array.add(toGson(value)));
            return array;
        }
        if (node.isNumber()) {
            return new JsonPrimitive(node.numberValue());
        }
        if (node.isBoolean()) {
            return new JsonPrimitive(node.booleanValue());
        }
        if (node.isTextual()) {
            return new JsonPrimitive(node.textValue());
        }
        return JsonNull.INSTANCE;
    }
}
