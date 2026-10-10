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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Coordinates protocol decoding and provider request construction.
 */
public final class AiProxyEngine {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final AiProxyProtocolFactory aiProxyProtocolFactory;

    private final AiProxyProviderFactory aiProxyProviderFactory;

    private final AiProxyTransport aiProxyTransport;

    /**
     * Create an AI proxy engine.
     *
     * @param aiProxyProtocolFactory protocol registry
     * @param aiProxyProviderFactory provider registry
     * @param aiProxyTransport upstream transport
     */
    public AiProxyEngine(final AiProxyProtocolFactory aiProxyProtocolFactory,
            final AiProxyProviderFactory aiProxyProviderFactory, final AiProxyTransport aiProxyTransport) {
        this.aiProxyProtocolFactory = aiProxyProtocolFactory;
        this.aiProxyProviderFactory = aiProxyProviderFactory;
        this.aiProxyTransport = aiProxyTransport;
    }

    /**
     * Execute a request through protocol, provider and transport layers.
     *
     * @param requestBody original client request body
     * @param config resolved AI configuration
     * @return upstream response
     */
    public Mono<AiUpstreamResponse> execute(final String requestBody, final AiCommonConfig config) {
        final AiUpstreamRequest request = buildRequest(requestBody, config);

        return aiProxyTransport.execute(request);
    }

    /**
     * Decode a client request and build its provider-specific upstream request.
     *
     * @param requestBody original client request body
     * @param config resolved AI configuration
     * @return provider-specific upstream request
     */
    public AiUpstreamRequest buildRequest(final String requestBody, final AiCommonConfig config) {
        if (Objects.isNull(requestBody) || requestBody.isBlank()) {
            throw new ShenyuException("AI request body must not be empty");
        }
        final JsonNode root;
        try {
            root = OBJECT_MAPPER.readTree(requestBody);
        } catch (JsonProcessingException ex) {
            throw new ShenyuException("AI request body must be valid JSON", ex);
        }
        if (!root.isObject()) {
            throw new ShenyuException("AI request body must be a JSON object");
        }
        final ObjectNode payload = (ObjectNode) root;
        final AiProxyProtocol protocol = Objects.isNull(config.getProtocol())
                ? aiProxyProtocolFactory.detect(payload)
                : aiProxyProtocolFactory.getProtocol(config.getProtocol());
        if (!protocol.matches(payload)) {
            throw new ShenyuException("Request body does not match AI protocol: " + protocol.name());
        }
        final ShenyuAiRequest request = protocol.decodeRequest(requestBody, payload);
        final AiProxyProvider provider = aiProxyProviderFactory.getProvider(config.getProvider());
        return provider.buildRequest(request, config);
    }
}
