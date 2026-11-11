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
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiResponseAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.PreparedAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProtocolRequestOverrides;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderCapabilities;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRoute;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;

import java.util.Objects;

/**
 * Plans an AI upstream exchange from the client request and resolved configuration.
 */
final class AiExchangePlanner {

    private static final ObjectMapper OBJECT_MAPPER =
            new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final AiProxyProtocolFactory protocolFactory;

    private final AiProxyProviderFactory providerFactory;

    private final AiProtocolRouteResolver routeResolver;

    AiExchangePlanner(final AiProxyProtocolFactory protocolFactory,
                      final AiProxyProviderFactory providerFactory,
                      final AiProtocolRouteResolver routeResolver) {
        this.protocolFactory = Objects.requireNonNull(protocolFactory, "protocolFactory must not be null");
        this.providerFactory = Objects.requireNonNull(providerFactory, "providerFactory must not be null");
        this.routeResolver = Objects.requireNonNull(routeResolver, "routeResolver must not be null");
    }

    AiPreparedExchange prepare(final String requestBody, final AiCommonConfig config) {
        ObjectNode payload = parseRequestPayload(requestBody);
        String upstreamBody = removeFallbackConfig(payload, requestBody);
        AiProxyProtocol clientProtocol = resolveClientProtocol(payload, config);
        validateClientProtocol(clientProtocol, payload);
        ShenyuAiRequest clientRequest = decodeClientRequest(clientProtocol, upstreamBody, payload, config);
        AiProxyProvider provider = resolveProvider(config);
        boolean stream = resolveStreamingMode(clientRequest, config);
        AiProtocolRoute route = resolveUpstreamRoute(clientRequest, provider, stream);
        ShenyuAiRequest providerRequest = route.conversion().request();
        validateProviderRequest(provider, providerRequest, stream);
        AiProxyProtocol targetProtocol = protocolFactory.getProtocol(route.targetProtocol());
        prepareOutgoingPayload(providerRequest, targetProtocol, provider, config, stream);
        return createPreparedExchange(
                providerRequest, provider, clientProtocol, targetProtocol, route, config, stream);
    }

    private ObjectNode parseRequestPayload(final String requestBody) {
        if (Objects.isNull(requestBody) || requestBody.isBlank()) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI request body must not be empty", null);
        }
        final JsonNode root;
        try {
            root = OBJECT_MAPPER.readTree(requestBody);
        } catch (JsonProcessingException error) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI request body must be valid JSON", error);
        }
        if (!root.isObject()) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST, "AI request body must be a JSON object", null);
        }
        return (ObjectNode) root;
    }

    private String removeFallbackConfig(final ObjectNode payload, final String requestBody) {
        if (payload.has("fallbackConfig")) {
            payload.remove("fallbackConfig");
            return payload.toString();
        }
        return requestBody;
    }

    private AiProxyProtocol resolveClientProtocol(final ObjectNode payload, final AiCommonConfig config) {
        return Objects.isNull(config.getProtocol()) || config.getProtocol().isBlank()
                ? protocolFactory.detect(payload) : protocolFactory.getProtocol(config.getProtocol());
    }

    private void validateClientProtocol(final AiProxyProtocol protocol, final ObjectNode payload) {
        if (!protocol.matches(payload)) {
            throw new AiProxyException(AiProxyException.Kind.REQUEST,
                    "Request body does not match AI protocol: " + protocol.name(), null);
        }
    }

    private ShenyuAiRequest decodeClientRequest(final AiProxyProtocol protocol, final String requestBody,
                                                final ObjectNode payload, final AiCommonConfig config) {
        return applyModelOverride(protocol.decodeRequest(requestBody, payload), protocol, config);
    }

    private AiProxyProvider resolveProvider(final AiCommonConfig config) {
        return providerFactory.getProvider(providerName(config));
    }

    private boolean resolveStreamingMode(final ShenyuAiRequest request, final AiCommonConfig config) {
        return request.hasStream() ? request.isStream() : Boolean.TRUE.equals(config.getStream());
    }

    private AiProtocolRoute resolveUpstreamRoute(final ShenyuAiRequest request, final AiProxyProvider provider,
                                                 final boolean stream) {
        return routeResolver.resolve(request, provider.capabilities().protocols(), stream);
    }

    private void validateProviderRequest(final AiProxyProvider provider, final ShenyuAiRequest providerRequest,
                                         final boolean stream) {
        if (!provider.supports(providerRequest.getProtocol())) {
            throw new ShenyuException("Unsupported AI upstream protocol/provider combination: "
                    + providerRequest.getProtocol() + " -> " + provider.name());
        }
        provider.capabilities().validate(providerRequest, stream);
    }

    private void prepareOutgoingPayload(final ShenyuAiRequest providerRequest,
                                        final AiProxyProtocol targetProtocol,
                                        final AiProxyProvider provider,
                                        final AiCommonConfig config,
                                        final boolean stream) {
        AiProtocolRequestOverrides requestOverrides = new AiProtocolRequestOverrides(
                config.getTemperature(), config.getMaxTokens(), provider.capabilities().features().contains(
                        AiProviderCapabilities.Feature.STREAM_USAGE));
        ObjectNode originalPayload = providerRequest.getRawPayload();
        ObjectNode payloadToPrepare = originalPayload.deepCopy();
        if (!providerRequest.hasStream() && Objects.nonNull(config.getStream())) {
            payloadToPrepare.put("stream", stream);
        }
        ObjectNode outgoingPayload = Objects.requireNonNull(
                targetProtocol.prepareOutgoingRequest(payloadToPrepare, requestOverrides),
                "AI protocol prepared payload is required");
        if (!originalPayload.equals(outgoingPayload)) {
            providerRequest.setRawPayload(outgoingPayload);
            providerRequest.setRawBody(outgoingPayload.toString());
        }
        providerRequest.setStream(stream);
    }

    private AiPreparedExchange createPreparedExchange(final ShenyuAiRequest providerRequest,
                                                       final AiProxyProvider provider,
                                                       final AiProxyProtocol clientProtocol,
                                                       final AiProxyProtocol targetProtocol,
                                                       final AiProtocolRoute route,
                                                       final AiCommonConfig config,
                                                       final boolean stream) {
        try {
            PreparedAiRequest preparedRequest = provider.prepareRequest(providerRequest, config);
            AiResponseAdapter clientResponseAdapter = response -> clientProtocol.encodeResponse(
                    route.conversion().responseAdapter().adapt(
                            preparedRequest.responseAdapter().adapt(response)), stream);
            boolean providerDeclaredReplaySafe = provider.capabilities()
                    .isReplaySafe(providerRequest.getOperation());
            return new AiPreparedExchange(preparedRequest.request(), clientResponseAdapter,
                    targetProtocol.responseInspector(),
                    route.clientProtocol(), route.targetProtocol(), stream,
                    AiProxyRetryPolicy.replaySafe(providerRequest), providerRequest.getOperation(),
                    providerDeclaredReplaySafe, provider.name(),
                    clientProtocol.isExactResponseCacheCompatible()
                            && targetProtocol.isExactResponseCacheCompatible()
                            && provider.isExactResponseCacheCompatible(route.targetProtocol()));
        } catch (IllegalArgumentException error) {
            throw new AiProxyException(AiProxyException.Kind.PROVIDER, "Invalid AI provider configuration", error);
        }
    }

    boolean providerDeclaredReplaySafe(final AiCommonConfig config,
                                       final ShenyuAiRequest.Operation operation) {
        return providerFactory.getProvider(providerName(config))
                .capabilities()
                .isReplaySafe(operation);
    }

    private ShenyuAiRequest applyModelOverride(final ShenyuAiRequest request,
                                               final AiProxyProtocol protocol,
                                               final AiCommonConfig config) {
        String configuredModel = config.getModel();
        if (Objects.isNull(configuredModel) || configuredModel.isBlank()
                || configuredModel.equals(request.getModel())) {
            return request;
        }
        ObjectNode effectivePayload = request.getRawPayload().deepCopy();
        effectivePayload.put("model", configuredModel);
        return protocol.decodeRequest(effectivePayload.toString(), effectivePayload);
    }

    private String providerName(final AiCommonConfig config) {
        return Objects.isNull(config.getProvider()) || config.getProvider().isBlank()
                ? "openai" : config.getProvider();
    }
}
