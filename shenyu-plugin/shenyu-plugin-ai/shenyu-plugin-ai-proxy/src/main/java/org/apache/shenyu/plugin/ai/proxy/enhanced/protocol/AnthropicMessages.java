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

/** Native Messages pass-through. Select anthropic-messages explicitly; no Chat conversion is claimed. */
public final class AnthropicMessages implements AiProxyProtocol {

    public static final String NAME = "anthropic-messages";

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public boolean supportsAutoDetection() {
        return false;
    }

    @Override
    public boolean matches(final JsonNode payload) {
        return Objects.nonNull(payload) && payload.isObject() && payload.path("messages").isArray() && !payload.has("input");
    }

    @Override
    public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
        if (!matches(payload) || payload.path("messages").isEmpty()) {
            throw new ShenyuException("Anthropic Messages requires a nonempty messages array");
        }
        for (JsonNode message : payload.path("messages")) {
            JsonNode content = message.get("content");
            if (!message.isObject() || !java.util.Set.of("user", "assistant").contains(message.path("role").asText())
                    || Objects.isNull(content) || !(content.isTextual() || content.isArray() && !content.isEmpty())) {
                throw new ShenyuException("Invalid Anthropic message");
            }
        }
        JsonNode maximum = payload.get("max_tokens");
        if (Objects.nonNull(maximum) && (!maximum.isIntegralNumber() || !maximum.canConvertToInt() || maximum.intValue() < 1)) {
            throw new ShenyuException("Invalid Anthropic max_tokens");
        }
        JsonNode stream = payload.get("stream");
        if (Objects.nonNull(stream) && !stream.isBoolean()) {
            throw new ShenyuException("Invalid Anthropic stream");
        }
        JsonNode model = payload.get("model");
        if (Objects.nonNull(model) && !model.isTextual()) {
            throw new ShenyuException("Invalid Anthropic model");
        }
        ShenyuAiRequest request = new ShenyuAiRequest();
        request.setProtocol(NAME);
        request.setOperation(ShenyuAiRequest.Operation.CHAT_COMPLETION);
        request.setRawBody(rawBody);
        request.setRawPayload(payload.deepCopy());
        request.setModel(Objects.isNull(model) ? null : model.textValue());
        request.setStream(Objects.isNull(stream) ? null : stream.booleanValue());
        request.setMaxTokens(Objects.isNull(maximum) ? null : maximum.intValue());
        return request;
    }

    @Override
    public ObjectNode prepareOutgoingRequest(final ObjectNode payload, final AiProtocolRequestOverrides overrides) {
        Objects.requireNonNull(overrides, "overrides");
        if (Objects.nonNull(overrides.temperature())) {
            payload.put("temperature", overrides.temperature());
        }
        if (Objects.nonNull(overrides.maxTokens())) {
            payload.put("max_tokens", overrides.maxTokens());
        }
        if (!payload.path("max_tokens").isIntegralNumber() || !payload.path("max_tokens").canConvertToInt() || payload.path("max_tokens").intValue() < 1) {
            throw new ShenyuException("Anthropic Messages requires positive max_tokens");
        }
        if (!payload.path("model").isTextual() || payload.path("model").asText().isBlank()) {
            throw new ShenyuException("Anthropic Messages requires a model");
        }
        // Native usage is emitted automatically; do not add OpenAI stream_options or invent token counts.
        return payload;
    }
}
