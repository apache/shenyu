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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.observation.AiUpstreamUsageTap;

import java.util.Objects;

/**
 * Native OpenAI Responses protocol.
 *
 * <p>Decodes client requests and maps gateway overrides to Responses fields.
 * Native responses use the default byte passthrough behavior. Cross-protocol
 * request and response conversion belongs to registered converters.</p>
 */
public final class OpenAiResponses implements AiProxyProtocol {

    /**
     * Selector handle protocol identifier for Responses clients.
     */
    public static final String NAME = "openai-responses";

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public AiProtocolResponseInspector responseInspector() {
        return (response, observation, stream) -> new AiUpstreamUsageTap(
                observation, response.getHeaders(), stream, false).attach(response);
    }

    @Override
    public boolean matches(final JsonNode payload) {
        return Objects.nonNull(payload) && payload.isObject() && payload.has("input") && !payload.has("messages");
    }

    @Override
    public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
        validateRequest(payload);

        JsonNode model = payload.get("model");
        JsonNode stream = payload.get("stream");
        JsonNode maxTokens = payload.get("max_output_tokens");

        ShenyuAiRequest request = new ShenyuAiRequest();
        request.setProtocol(NAME);
        request.setOperation(ShenyuAiRequest.Operation.RESPONSE);
        request.setRawBody(rawBody);
        request.setRawPayload(payload.deepCopy());
        request.setModel(hasValue(model) ? model.textValue() : null);
        request.setStream(hasValue(stream) ? stream.booleanValue() : null);
        request.setMaxTokens(hasValue(maxTokens) ? maxTokens.intValue() : null);
        return request;
    }

    private void validateRequest(final ObjectNode payload) {
        JsonNode input = payload.get("input");
        if (Objects.isNull(input) || !(input.isTextual() || input.isArray())) {
            throw new ShenyuException("OpenAI Responses input must be a string or array");
        }
        JsonNode stream = payload.get("stream");
        if (hasValue(stream) && !stream.isBoolean()) {
            throw new ShenyuException("OpenAI Responses stream must be boolean");
        }
        JsonNode model = payload.get("model");
        if (hasValue(model) && !model.isTextual()) {
            throw new ShenyuException("OpenAI Responses model must be a string");
        }
        JsonNode maxTokens = payload.get("max_output_tokens");
        if (hasValue(maxTokens)
                && (!maxTokens.isIntegralNumber() || !maxTokens.canConvertToInt() || maxTokens.intValue() <= 0)) {
            throw new ShenyuException("OpenAI Responses max_output_tokens must be a positive integer");
        }
    }

    private static boolean hasValue(final JsonNode node) {
        return Objects.nonNull(node) && !node.isNull();
    }

    @Override
    public ObjectNode prepareOutgoingRequest(final ObjectNode payload, final AiProtocolRequestOverrides overrides) {
        Objects.requireNonNull(payload, "OpenAI Responses payload is required");
        Objects.requireNonNull(overrides, "OpenAI Responses request overrides are required");
        if (Objects.nonNull(overrides.temperature())) {
            payload.put("temperature", overrides.temperature());
        }
        if (Objects.nonNull(overrides.maxTokens())) {
            if (overrides.maxTokens() <= 0) {
                throw new IllegalArgumentException("AI maxTokens must be positive");
            }
            payload.remove("max_output_tokens");
            payload.put("max_output_tokens", overrides.maxTokens());
        }
        return payload;
    }
}
