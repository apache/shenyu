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
import io.github.agenticai.connect.ProviderProfile;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.ConnectProtocolAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;

import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.regex.Pattern;

/**
 * OpenAI provider adapter; request construction opens no HTTP connections.
 */
public final class OpenAi implements AiProxyProvider {

    public static final String NAME = "openai";

    private static final Pattern HEADER_NAME = Pattern.compile("[!#$%&'*+.^_`|~0-9A-Za-z-]+");

    private final OpenAiProviderOptions options;

    /**
     * Create an OpenAI Chat upstream.
     */
    public OpenAi() {
        this(OpenAiProviderOptions.openAi());
    }

    /**
     * Retain the explicit Connect dialect constructor for existing extensions.
     *
     * @param profile explicitly selected provider dialect
     */
    public OpenAi(final ProviderProfile profile) {
        this(OpenAiProviderOptions.generic(profile));
    }

    /**
     * Create a Chat upstream with explicit provider options.
     *
     * @param options provider options
     */
    private OpenAi(final OpenAiProviderOptions options) {
        this.options = Objects.requireNonNull(options, "AI provider options are required");
    }

    /**
     * Create a provider with explicit options while retaining the legacy dialect constructor.
     *
     * @param options provider options
     * @return configured provider
     */
    public static OpenAi withOptions(final OpenAiProviderOptions options) {
        return new OpenAi(options);
    }

    @Override
    public String name() {
        return NAME;
    }

    @Override
    public AiProviderCapabilities capabilities() {
        return options.capabilities();
    }

    @Override
    public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
        validateConfig(config);
        if (!supports(request.getProtocol())) {
            throw new IllegalArgumentException("Unsupported OpenAI protocol: " + request.getProtocol());
        }
        boolean stream = request.hasStream() ? request.isStream() : Boolean.TRUE.equals(config.getStream());
        capabilities().validate(request, stream);
        ObjectNode payload = request.getRawPayload().deepCopy();
        payload.remove("fallbackConfig");
        options.defaults().fields().forEachRemaining(entry -> {
            if (!payload.has(entry.getKey())) {
                payload.set(entry.getKey(), entry.getValue());
            }
        });
        String configuredModel = config.getModel();
        String model = Objects.nonNull(configuredModel) && !configuredModel.isBlank() ? configuredModel : payload.path("model").asText(null);
        String mappedModel = Objects.isNull(model) ? null : options.modelMappings().getOrDefault(model, model);
        if (Objects.nonNull(mappedModel)) {
            payload.put("model", mappedModel);
        }
        if (Objects.nonNull(config.getTemperature())) {
            payload.put("temperature", config.getTemperature());
        }
        if (Objects.nonNull(config.getMaxTokens())) {
            if (config.getMaxTokens() <= 0) {
                throw new IllegalArgumentException("AI maxTokens must be positive");
            }
            if (OpenAiResponses.NAME.equals(request.getProtocol())) {
                payload.put("max_output_tokens", config.getMaxTokens());
            } else {
                String field = options.maxTokensField().isEmpty()
                        ? (payload.hasNonNull("max_completion_tokens") ? "max_completion_tokens" : "max_tokens") : options.maxTokensField();
                payload.remove("max_tokens");
                payload.remove("max_completion_tokens");
                payload.put(field, config.getMaxTokens());
            }
        }
        if (!request.hasStream() && Objects.nonNull(config.getStream())) {
            payload.put("stream", stream);
        }
        ConnectProtocolAdapter adapter = new ConnectProtocolAdapter(request.getProtocol(), mappedModel, options.connectProfile());
        ObjectNode converted = adapter.convertRequest(payload);
        JsonNode convertedStream = converted.get("stream");
        if (Objects.nonNull(convertedStream) && !convertedStream.isNull() && !convertedStream.isBoolean()
                || converted.path("stream").asBoolean() != stream) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI provider dialect must preserve the selected streaming mode", null);
        }
        if (stream && capabilities().features().contains(AiProviderCapabilities.Feature.STREAM_USAGE)) {
            enableStreamUsage(converted);
        }
        capabilities().validatePayload(converted);
        AiUpstreamRequest upstream = new AiUpstreamRequest();
        upstream.setMethod(HttpMethod.POST.name());
        upstream.setUri(resolveEndpoint(config));
        upstream.setHeaders(headers(config, stream));
        String body = request.getRawPayload().equals(converted) ? request.getRawBody() : converted.toString();
        upstream.setBody(body.getBytes(StandardCharsets.UTF_8));
        upstream.setResponseAdapter(response -> adapter.convertResponse(response, stream));
        return upstream;
    }

    private Map<String, List<String>> headers(final AiCommonConfig config, final boolean stream) {
        Map<String, List<String>> headers = new LinkedHashMap<>();
        options.headers().forEach((name, value) -> putHeader(headers, name, value));
        putHeader(headers, HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
        putHeader(headers, HttpHeaders.ACCEPT, stream ? MediaType.TEXT_EVENT_STREAM_VALUE : MediaType.APPLICATION_JSON_VALUE);
        if (options.authentication() == OpenAiProviderOptions.Authentication.HEADER) {
            putHeader(headers, options.credentialName(), options.credentialPrefix() + config.getApiKey());
        }
        return headers;
    }

    private void putHeader(final Map<String, List<String>> headers, final String name, final String value) {
        if (!HEADER_NAME.matcher(name).matches() || value.contains("\r") || value.contains("\n")) {
            throw new IllegalArgumentException("Invalid AI provider header: " + name);
        }
        headers.keySet().removeIf(existing -> existing.equalsIgnoreCase(name));
        headers.put(name, List.of(value));
    }

    private void enableStreamUsage(final ObjectNode payload) {
        JsonNode current = payload.get("stream_options");
        ObjectNode streamOptions;
        if (Objects.isNull(current) || current.isNull()) {
            streamOptions = payload.putObject("stream_options");
        } else if (current.isObject()) {
            streamOptions = (ObjectNode) current;
        } else {
            throw new ShenyuException("OpenAI Chat field 'stream_options' must be an object");
        }
        if (!streamOptions.has("include_usage")) {
            streamOptions.put("include_usage", true);
        }
    }

    private void validateConfig(final AiCommonConfig config) {
        Objects.requireNonNull(config, "AI provider configuration is required");
        if (Objects.isNull(config.getBaseUrl()) || config.getBaseUrl().isBlank()) {
            throw new IllegalArgumentException("AI baseUrl must not be empty");
        }
        if (options.authentication() != OpenAiProviderOptions.Authentication.NONE
                && (Objects.isNull(config.getApiKey()) || config.getApiKey().isBlank())) {
            throw new IllegalArgumentException("AI apiKey must not be empty");
        }
    }

    private URI resolveEndpoint(final AiCommonConfig config) {
        URI base = URI.create(config.getBaseUrl());
        if (!("http".equalsIgnoreCase(base.getScheme()) || "https".equalsIgnoreCase(base.getScheme()))
                || Objects.isNull(base.getHost()) || Objects.nonNull(base.getRawQuery()) || Objects.nonNull(base.getRawFragment()) || Objects.nonNull(base.getRawUserInfo())) {
            throw new IllegalArgumentException("AI baseUrl must be an HTTP(S) endpoint without credentials, query or fragment");
        }
        String path = options.path().startsWith("/") ? options.path().substring(1) : options.path();
        String basePath = base.getPath().endsWith("/") ? base.getPath().substring(0, base.getPath().length() - 1) : base.getPath();
        URI endpoint;
        if (basePath.endsWith("/" + path)) {
            endpoint = URI.create(base.toString().replaceAll("/+$", ""));
        } else {
            if (basePath.endsWith("/v1") && path.startsWith("v1/")) {
                path = path.substring(3);
            }
            URI directory = URI.create(base.toString().endsWith("/") ? base.toString() : base + "/");
            endpoint = directory.resolve(path);
        }
        Map<String, String> query = new TreeMap<>(options.queryParameters());
        if (options.authentication() == OpenAiProviderOptions.Authentication.QUERY) {
            query.put(options.credentialName(), options.credentialPrefix() + config.getApiKey());
        }
        if (query.isEmpty()) {
            return endpoint;
        }
        StringBuilder uri = new StringBuilder(endpoint.toString()).append('?');
        query.forEach((name, value) -> uri.append(encode(name)).append('=').append(encode(value)).append('&'));
        uri.setLength(uri.length() - 1);
        return URI.create(uri.toString());
    }

    private String encode(final String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8).replace("+", "%20");
    }

    @Override
    public boolean supports(final String protocol) {
        return Objects.nonNull(protocol) && capabilities().protocols().contains(protocol);
    }
}
