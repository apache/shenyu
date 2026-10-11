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
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

/**
 * Coordinates the complete reactive AI exchange and owns retry and fallback policy.
 */
public final class AiProxyEngine {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final AiProxyProtocolFactory aiProxyProtocolFactory;

    private final AiProxyProviderFactory aiProxyProviderFactory;

    private final AiProxyTransport aiProxyTransport;

    private final Duration retryBackoff;

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers, final AiProxyTransport transport) {
        this(protocols, providers, transport, Duration.ofSeconds(1));
    }

    public AiProxyEngine(final AiProxyProtocolFactory protocols, final AiProxyProviderFactory providers,
                        final AiProxyTransport transport, final Duration retryBackoff) {
        this.aiProxyProtocolFactory = protocols;
        this.aiProxyProviderFactory = providers;
        this.aiProxyTransport = transport;
        this.retryBackoff = retryBackoff;
    }

    /**
     * Execute and consume a complete exchange with retry and fallback before body emission.
     * Keeping the consumer inside this scope preserves upstream lifetime and backpressure.
     *
     * @param requestBody original client request
     * @param primaryConfig resolved primary configuration
     * @param fallbackConfig resolved fallback configuration
     * @param responseConsumer response boundary consuming the reactive body once
     * @return completion or a classified failure
     */
    public Mono<Void> execute(final String requestBody, final AiCommonConfig primaryConfig,
                              final Optional<AiCommonConfig> fallbackConfig,
                              final Function<AiUpstreamResponse, Mono<Void>> responseConsumer) {
        return Mono.defer(() -> {
            PreparedExchange primary = prepare(requestBody, new AiCommonConfig(primaryConfig));
            Optional<AiCommonConfig> fallback = fallbackConfig.map(config -> {
                AiCommonConfig copy = new AiCommonConfig(config);
                copy.setStream(primary.stream());
                return copy;
            });
            return attempt(requestBody, primary, fallback, primary.stream() ? 1 : 3, responseConsumer, new AtomicBoolean());
        });
    }

    /**
     * Decode and adapt a request without opening an HTTP connection.
     *
     * @param requestBody original client body
     * @param config resolved configuration
     * @return upstream request
     */
    public AiUpstreamRequest buildRequest(final String requestBody, final AiCommonConfig config) {
        return prepare(requestBody, config).request();
    }

    private Mono<Void> attempt(final String requestBody, final PreparedExchange prepared,
                               final Optional<AiCommonConfig> fallback, final int retries,
                               final Function<AiUpstreamResponse, Mono<Void>> consumer, final AtomicBoolean emitted) {
        return Mono.defer(() -> aiProxyTransport.execute(prepared.request()))
                .flatMap(response -> {
                    int status = response.getStatusCode();
                    if (status >= 400 && ((retryableStatus(status) && retries > 0) || fallback.isPresent())) {
                        // Release this response without accumulating the error body before another attempt.
                        return response.getBody().then(Mono.error(new UpstreamStatusFailure(status)));
                    }
                    AiUpstreamResponse adapted = prepared.provider().adaptResponse(response);
                    AiUpstreamResponse encoded = prepared.protocol().encodeResponse(adapted, prepared.stream());
                    return encoded.getBody().switchOnFirst((signal, body) -> {
                        if (signal.isOnError()) {
                            return Flux.error(Objects.requireNonNull(signal.getThrowable()));
                        }
                        AiUpstreamResponse ready = new AiUpstreamResponse(encoded.getStatusCode(), encoded.getHeaders(),
                                body.doOnNext(bytes -> emitted.set(true)));
                        return Mono.defer(() -> consumer.apply(ready)).flux();
                    }).then();
                })
                .onErrorResume(error -> {
                    boolean transportFailure = error instanceof AiProxyException
                            && ((AiProxyException) error).getKind() == AiProxyException.Kind.TRANSPORT;
                    boolean statusFailure = error instanceof UpstreamStatusFailure;
                    if (emitted.get() || !(transportFailure || statusFailure)) {
                        return Mono.error(error);
                    }
                    boolean retryable = transportFailure || retryableStatus(((UpstreamStatusFailure) error).status);
                    if (retryable && retries > 0) {
                        Duration delay = prepared.stream() ? Duration.ZERO : retryBackoff.multipliedBy(1L << (3 - retries));
                        return Mono.delay(delay).then(attempt(requestBody, prepared, fallback, retries - 1, consumer, emitted));
                    }
                    if (fallback.isPresent()) {
                        return Mono.defer(() -> attempt(requestBody, prepare(requestBody, fallback.get()), Optional.empty(), 0, consumer, emitted));
                    }
                    return Mono.error(error);
                });
    }

    private boolean retryableStatus(final int status) {
        return status == 429 || (status >= 500 && status < 600);
    }

    private PreparedExchange prepare(final String requestBody, final AiCommonConfig config) {
        if (Objects.isNull(requestBody) || requestBody.isBlank()) {
            throw new ShenyuException("AI request body must not be empty");
        }
        final JsonNode root;
        try {
            root = OBJECT_MAPPER.readTree(requestBody);
        } catch (JsonProcessingException error) {
            throw new ShenyuException("AI request body must be valid JSON", error);
        }
        if (!root.isObject()) {
            throw new ShenyuException("AI request body must be a JSON object");
        }
        ObjectNode payload = (ObjectNode) root;
        String upstreamBody = requestBody;
        if (payload.has("fallbackConfig")) {
            payload.remove("fallbackConfig");
            upstreamBody = payload.toString();
        }
        AiProxyProtocol protocol = Objects.isNull(config.getProtocol()) || config.getProtocol().isBlank()
                ? aiProxyProtocolFactory.detect(payload) : aiProxyProtocolFactory.getProtocol(config.getProtocol());
        if (!protocol.matches(payload)) {
            throw new ShenyuException("Request body does not match AI protocol: " + protocol.name());
        }
        ShenyuAiRequest decoded = protocol.decodeRequest(upstreamBody, payload);
        AiProxyProvider provider = aiProxyProviderFactory.getProvider(
                Objects.isNull(config.getProvider()) || config.getProvider().isBlank() ? "openai" : config.getProvider());
        if (!provider.supports(protocol.name())) {
            throw new ShenyuException("Unsupported AI protocol/provider combination");
        }
        try {
            AiUpstreamRequest request = provider.buildRequest(decoded, config);
            boolean stream = decoded.hasStream() ? decoded.isStream() : Boolean.TRUE.equals(config.getStream());
            return new PreparedExchange(protocol, provider, request, stream);
        } catch (IllegalArgumentException error) {
            throw new AiProxyException(AiProxyException.Kind.PROVIDER, "Invalid AI provider configuration", error);
        }
    }

    private record PreparedExchange(AiProxyProtocol protocol, AiProxyProvider provider, AiUpstreamRequest request, boolean stream) {
    }

    private static final class UpstreamStatusFailure extends RuntimeException {

        private static final long serialVersionUID = 1L;

        private final int status;

        private UpstreamStatusFailure(final int status) {
            super("AI upstream returned HTTP " + status);
            this.status = status;
        }
    }
}
