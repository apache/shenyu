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

package org.apache.shenyu.plugin.ai.protocol;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.constant.Constants;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Resolves the AI protocol of a request from three levels of evidence: the request
 * path, exact header hints and, only when the stronger levels are inconclusive,
 * the request body shape. Every level must yield exactly one candidate; multiple
 * candidates at any level resolve to unknown instead of a silent order based
 * choice.
 */
public final class AiProtocolDetector {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private static final List<AiProtocolDescriptor> DESCRIPTORS = List.of(new OpenAiChatProtocol(), new OpenAiResponsesProtocol());

    /**
     * The sentinel cached for requests whose protocol stays unknown, so the evidence
     * levels are evaluated at most once per request.
     */
    private static final String UNKNOWN = AiProtocols.UNKNOWN;

    private AiProtocolDetector() {
    }

    /**
     * Detect the protocol identifier of a request from its evidence.
     *
     * @param path the request path, null skips the path level
     * @param headers the request headers, null skips the header level
     * @param rawBody the raw request body, parsed only when the path and header
     *                levels are inconclusive
     * @return the single matching protocol identifier, empty for unknown or
     *         ambiguous evidence
     */
    public static Optional<String> detect(final String path, final HttpHeaders headers, final String rawBody) {
        return detect(DESCRIPTORS, path, headers, rawBody);
    }

    static Optional<String> detect(final List<AiProtocolDescriptor> descriptors, final String path,
                                   final HttpHeaders headers, final String rawBody) {
        return detect(descriptors, path, headers, () -> parseBody(rawBody));
    }

    /**
     * Detect the protocol identifier from an already parsed request payload, so
     * callers that decoded the body for their own work do not pay a second parse
     * for the body evidence level. This is the same three level detection as the
     * raw body overload; no caller should decide the body shape on its own.
     *
     * @param path the request path, null skips the path level
     * @param headers the request headers, null skips the header level
     * @param payload the parsed request payload
     * @return the single matching protocol identifier, empty for unknown or
     *         ambiguous evidence
     */
    public static Optional<String> detect(final String path, final HttpHeaders headers, final JsonNode payload) {
        return detect(DESCRIPTORS, path, headers, payload);
    }

    static Optional<String> detect(final List<AiProtocolDescriptor> descriptors, final String path,
                                   final HttpHeaders headers, final JsonNode payload) {
        return detect(descriptors, path, headers, () -> payload);
    }

    private static Optional<String> detect(final List<AiProtocolDescriptor> descriptors, final String path,
                                           final HttpHeaders headers, final Supplier<JsonNode> payloadSupplier) {
        if (Objects.nonNull(path)) {
            List<String> matches = pathCandidates(descriptors, path);
            if (matches.size() == 1) {
                return Optional.of(matches.get(0));
            }
            if (matches.size() > 1) {
                return Optional.empty();
            }
        }
        if (Objects.nonNull(headers)) {
            List<String> matches = headerCandidates(descriptors, headers);
            if (matches.size() == 1) {
                return Optional.of(matches.get(0));
            }
            if (matches.size() > 1) {
                return Optional.empty();
            }
        }
        List<String> matches = bodyCandidates(descriptors, payloadSupplier.get());
        return matches.size() == 1 ? Optional.of(matches.get(0)) : Optional.empty();
    }

    /**
     * Resolve the protocol of the exchange and cache it under
     * {@link Constants#AI_PROTOCOL} for the later AI plugins of the same request.
     *
     * @param exchange the exchange whose request supplies the evidence
     * @param rawBody the raw request body
     * @return the resolved protocol identifier, empty when it stays unknown
     */
    public static Optional<String> resolve(final ServerWebExchange exchange, final String rawBody) {
        exchange.getAttributes().computeIfAbsent(Constants.AI_PROTOCOL,
            key -> detect(exchange.getRequest().getPath().value(), exchange.getRequest().getHeaders(), rawBody).orElse(UNKNOWN));
        return resolved(exchange);
    }

    /**
     * Resolve and cache the protocol from an already parsed request payload.
     *
     * @param exchange the exchange whose request supplies the path and header evidence
     * @param payload the parsed request payload used for the body evidence level
     * @return the resolved protocol identifier, empty when it stays unknown
     */
    public static Optional<String> resolve(final ServerWebExchange exchange, final JsonNode payload) {
        exchange.getAttributes().computeIfAbsent(Constants.AI_PROTOCOL,
            key -> detect(exchange.getRequest().getPath().value(), exchange.getRequest().getHeaders(), payload)
                    .orElse(UNKNOWN));
        return resolved(exchange);
    }

    /**
     * Read the protocol already resolved for the exchange without resolving anything,
     * so a component that owns the authoritative protocol can compare its configuration
     * with the detection performed by the plugins that ran before it.
     *
     * @param exchange the exchange to inspect
     * @return the cached protocol identifier, empty when nothing was resolved or the
     *         request stayed unknown
     */
    public static Optional<String> resolved(final ServerWebExchange exchange) {
        Object cached = exchange.getAttributes().get(Constants.AI_PROTOCOL);
        if (!(cached instanceof String)) {
            return Optional.empty();
        }
        String protocol = (String) cached;
        return protocol.isEmpty() ? Optional.empty() : Optional.of(protocol);
    }

    /**
     * Whether detection can produce the given protocol identifier, that is whether a
     * built-in descriptor owns it.
     *
     * @param protocol the protocol identifier to check
     * @return true when a built-in descriptor declares this identifier
     */
    public static boolean isDetectable(final String protocol) {
        return DESCRIPTORS.stream().anyMatch(descriptor -> descriptor.id().equals(protocol));
    }

    private static List<String> pathCandidates(final List<AiProtocolDescriptor> descriptors, final String path) {
        // the descriptors declare normalized paths, so a trailing slash on the request
        // path must not change the verdict; case is preserved because paths are case sensitive
        String normalizedPath = stripTrailingSlashes(path);
        return descriptors.stream()
                .filter(descriptor -> descriptor.paths().stream().anyMatch(normalizedPath::endsWith))
                .map(AiProtocolDescriptor::id)
                .collect(Collectors.toList());
    }

    private static String stripTrailingSlashes(final String path) {
        int end = path.length();
        while (end > 1 && path.charAt(end - 1) == '/') {
            end--;
        }
        return path.substring(0, end);
    }

    private static List<String> headerCandidates(final List<AiProtocolDescriptor> descriptors, final HttpHeaders headers) {
        return descriptors.stream()
                .filter(descriptor -> matchesHeaders(descriptor, headers))
                .map(AiProtocolDescriptor::id)
                .collect(Collectors.toList());
    }

    private static boolean matchesHeaders(final AiProtocolDescriptor descriptor, final HttpHeaders headers) {
        Map<String, String> hints = descriptor.headerHints();
        if (hints.isEmpty()) {
            return false;
        }
        for (Map.Entry<String, String> hint : hints.entrySet()) {
            List<String> values = headers.get(hint.getKey());
            if (Objects.isNull(values) || !values.contains(hint.getValue())) {
                return false;
            }
        }
        return true;
    }

    private static JsonNode parseBody(final String rawBody) {
        if (Objects.isNull(rawBody)) {
            return null;
        }
        try {
            return MAPPER.readTree(rawBody);
        } catch (JsonProcessingException e) {
            return null;
        }
    }

    private static List<String> bodyCandidates(final List<AiProtocolDescriptor> descriptors, final JsonNode payload) {
        if (Objects.isNull(payload)) {
            return Collections.emptyList();
        }
        return descriptors.stream()
                .filter(descriptor -> descriptor.bodyMatches(payload))
                .map(AiProtocolDescriptor::id)
                .collect(Collectors.toList());
    }
}
