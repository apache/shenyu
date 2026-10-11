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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * OpenAI provider adapter.
 */
public final class OpenAi implements AiProxyProvider {

    public static final String NAME = "openai";

    private static final String CHAT_COMPLETIONS_PATH = "/v1/chat/completions";

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
        validateConfig(config);
        if (!OpenAiChat.NAME.equals(request.getProtocol())) {
            throw new IllegalArgumentException(
                    "Unsupported OpenAI protocol: " + request.getProtocol());
        }

        final ObjectNode payload = request.getRawPayload().deepCopy();
        boolean changed = false;

        if (Objects.nonNull(config.getModel())) {
            payload.put("model", config.getModel());
            changed = true;
        }
        if (Objects.nonNull(config.getTemperature())) {
            payload.put("temperature", config.getTemperature());
            changed = true;
        }
        if (Objects.nonNull(config.getMaxTokens())) {
            payload.remove("max_tokens");
            payload.put("max_completion_tokens", config.getMaxTokens());
            changed = true;
        }

        final boolean stream = request.hasStream()
                ? request.isStream()
                : Boolean.TRUE.equals(config.getStream());
        if (!request.hasStream() && Objects.nonNull(config.getStream())) {
            payload.put("stream", stream);
            changed = true;
        }
        if (stream) {
            changed = enableStreamUsage(payload) || changed;
        }

        final Map<String, List<String>> headers =
                new LinkedHashMap<>();
        headers.put(
                HttpHeaders.AUTHORIZATION,
                List.of("Bearer " + config.getApiKey()));
        headers.put(
                HttpHeaders.CONTENT_TYPE,
                List.of(MediaType.APPLICATION_JSON_VALUE));
        headers.put(
                HttpHeaders.ACCEPT,
                List.of(stream
                        ? MediaType.TEXT_EVENT_STREAM_VALUE
                        : MediaType.APPLICATION_JSON_VALUE));

        final AiUpstreamRequest upstreamRequest =
                new AiUpstreamRequest();
        upstreamRequest.setMethod(HttpMethod.POST.name());
        upstreamRequest.setUri(resolveEndpoint(
                config.getBaseUrl(),
                CHAT_COMPLETIONS_PATH));
        upstreamRequest.setHeaders(headers);
        final String body = changed ? payload.toString() : request.getRawBody();
        upstreamRequest.setBody(body.getBytes(StandardCharsets.UTF_8));

        return upstreamRequest;
    }

    private boolean enableStreamUsage(final ObjectNode payload) {
        final JsonNode current = payload.get("stream_options");
        final ObjectNode streamOptions;
        if (Objects.isNull(current) || current.isNull()) {
            streamOptions = payload.putObject("stream_options");
        } else if (current.isObject()) {
            streamOptions = (ObjectNode) current;
        } else {
            throw new ShenyuException("OpenAI Chat field 'stream_options' must be an object");
        }
        if (!streamOptions.has("include_usage")) {
            streamOptions.put("include_usage", true);
            return true;
        }
        return false;
    }

    private void validateConfig(final AiCommonConfig config) {
        if (Objects.isNull(config.getBaseUrl()) || config.getBaseUrl().isBlank()) {
            throw new IllegalArgumentException("OpenAI baseUrl must not be empty");
        }
        if (Objects.isNull(config.getApiKey()) || config.getApiKey().isBlank()) {
            throw new IllegalArgumentException("OpenAI apiKey must not be empty");
        }
    }

    private URI resolveEndpoint(
            final String baseUrl,
            final String path) {

        final String normalizedBaseUrl =
                baseUrl.endsWith("/") ? baseUrl : baseUrl + "/";
        final String relativePath =
                path.startsWith("/") ? path.substring(1) : path;

        return URI.create(normalizedBaseUrl).resolve(relativePath);
    }
}
