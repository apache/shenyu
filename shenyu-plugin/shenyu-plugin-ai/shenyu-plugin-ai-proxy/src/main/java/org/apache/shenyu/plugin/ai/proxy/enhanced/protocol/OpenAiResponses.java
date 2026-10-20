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

import java.util.Objects;

/**
 * Responses request boundary; Connect owns conversion of its protocol fields.
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
    public boolean matches(final JsonNode payload) {
        return Objects.nonNull(payload) && payload.isObject() && payload.has("input") && !payload.has("messages");
    }

    @Override
    public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
        JsonNode input = payload.get("input");
        if (Objects.isNull(input) || !(input.isTextual() || input.isArray())) {
            throw new ShenyuException("OpenAI Responses input must be a string or array");
        }
        JsonNode stream = payload.get("stream");
        if (Objects.nonNull(stream) && !stream.isNull() && !stream.isBoolean()) {
            throw new ShenyuException("OpenAI Responses stream must be boolean");
        }
        JsonNode model = payload.get("model");
        if (Objects.nonNull(model) && !model.isNull() && !model.isTextual()) {
            throw new ShenyuException("OpenAI Responses model must be a string");
        }
        JsonNode maxTokens = payload.get("max_output_tokens");
        if (Objects.nonNull(maxTokens) && !maxTokens.isNull()
                && (!maxTokens.isIntegralNumber() || !maxTokens.canConvertToInt() || maxTokens.intValue() <= 0)) {
            throw new ShenyuException("OpenAI Responses max_output_tokens must be a positive integer");
        }
        ShenyuAiRequest request = new ShenyuAiRequest();
        request.setProtocol(NAME);
        request.setOperation(ShenyuAiRequest.Operation.RESPONSE);
        request.setRawBody(rawBody);
        request.setRawPayload(payload.deepCopy());
        request.setModel(Objects.isNull(model) || model.isNull() ? null : model.textValue());
        request.setStream(Objects.isNull(stream) || stream.isNull() ? null : stream.booleanValue());
        request.setMaxTokens(Objects.isNull(maxTokens) || maxTokens.isNull() ? null : maxTokens.intValue());
        return request;
    }
}
