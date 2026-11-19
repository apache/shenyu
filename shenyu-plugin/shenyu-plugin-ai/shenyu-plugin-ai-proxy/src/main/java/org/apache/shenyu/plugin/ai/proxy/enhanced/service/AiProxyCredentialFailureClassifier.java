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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable, operator-declared credential classification for exact upstream endpoints.
 */
final class AiProxyCredentialFailureClassifier {

    private static final int HTTPS_PORT = 443;

    private static final int MAX_VALUE_LENGTH = 128;

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
            .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION);

    private final Map<Scope, List<Rule>> declarations;

    AiProxyCredentialFailureClassifier(final AiProxyRetryProperties.CredentialFailureFallbackProperties properties,
                                      final AiProxyProtocolFactory protocols) {
        final Map<Scope, List<Rule>> compiled = new LinkedHashMap<>();
        int index = 0;
        try {
            for (AiProxyRetryProperties.CredentialFailureDeclaration declaration : properties.getDeclarations()) {
                addDeclaration(compiled, declaration, protocols);
                index++;
            }
        } catch (IllegalArgumentException | ShenyuException e) {
            // Neither the rejected configuration nor a nested exception may leak into startup logs.
            throw new IllegalArgumentException("credentialFailureFallback.invalidDeclaration[" + index + "]");
        }
        if (properties.isEnabled() && compiled.isEmpty()) {
            throw new IllegalArgumentException("credentialFailureFallback.declarationsRequired");
        }
        compiled.replaceAll((scope, rules) -> List.copyOf(rules));
        declarations = properties.isEnabled() ? Map.copyOf(compiled) : Map.of();
    }

    List<Rule> findRules(final URI uri, final String protocol, final AiUpstreamResponse response) {
        if (declarations.isEmpty() || !isCredentialStatus(response.getStatusCode())) {
            return List.of();
        }
        final Endpoint endpoint;
        try {
            endpoint = endpoint(AiProxyConfigValidator.parseBaseUrl(uri.toString()));
        } catch (IllegalArgumentException e) {
            return List.of();
        }
        final List<Rule> rules = declarations.get(new Scope(endpoint, protocol, response.getStatusCode()));
        return Objects.nonNull(rules) && jsonResponse(response.getHeaders()) ? rules : List.of();
    }

    boolean matches(final byte[] body, final List<Rule> rules) {
        if (Objects.isNull(body) || body.length == 0 || body.length > AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES) {
            return false;
        }
        try {
            final JsonNode root = MAPPER.readTree(body);
            if (Objects.isNull(root)) {
                return false;
            }
            final JsonNode error = root.path("error");
            return root.isObject() && error.isObject() && rules.stream().anyMatch(rule -> {
                final JsonNode actual = error.path(rule.field());
                return actual.isTextual() && rule.value().equals(actual.textValue());
            });
        } catch (IOException e) {
            // An unknown response is forwarded, without recording raw provider errors or parser messages.
            return false;
        }
    }

    static boolean isCredentialStatus(final int status) {
        return status == HttpStatus.UNAUTHORIZED.value() || status == HttpStatus.FORBIDDEN.value();
    }

    private static void addDeclaration(final Map<Scope, List<Rule>> compiled,
                                       final AiProxyRetryProperties.CredentialFailureDeclaration declaration,
                                       final AiProxyProtocolFactory protocols) {
        if (Objects.isNull(declaration) || Objects.isNull(declaration.getProtocol())
                || !isCredentialStatus(declaration.getStatus())) {
            throw new IllegalArgumentException("invalid declaration");
        }
        protocols.getProtocol(declaration.getProtocol());
        final Rule rule = rule(declaration.getType(), declaration.getValue());
        final Endpoint endpoint = endpoint(AiProxyConfigValidator.parseBaseUrl(declaration.getEndpoint()));
        final List<Rule> rules = compiled.computeIfAbsent(new Scope(endpoint, declaration.getProtocol(), declaration.getStatus()), ignored -> new ArrayList<>());
        if (rules.contains(rule)) {
            throw new IllegalArgumentException("duplicate declaration");
        }
        rules.add(rule);
    }

    private static Rule rule(final String type, final String value) {
        if (!"error.code".equals(type) && !"error.type".equals(type)) {
            throw new IllegalArgumentException("invalid error field");
        }
        if (Objects.isNull(value) || value.isBlank() || value.length() > MAX_VALUE_LENGTH
                || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("invalid error value");
        }
        return new Rule(type.substring("error.".length()), value);
    }

    private static Endpoint endpoint(final URI uri) {
        if (!"https".equalsIgnoreCase(uri.getScheme()) || Objects.isNull(uri.getRawPath())
                || !uri.getRawPath().startsWith("/") || uri.getRawPath().contains("*")) {
            throw new IllegalArgumentException("exact HTTPS endpoint with path required");
        }
        return new Endpoint(uri.getHost().toLowerCase(Locale.ROOT), uri.getPort() == -1 ? HTTPS_PORT : uri.getPort(), uri.getRawPath());
    }

    private static boolean jsonResponse(final Map<String, List<String>> headers) {
        final List<String> types = values(headers, HttpHeaders.CONTENT_TYPE);
        final List<String> encodings = values(headers, HttpHeaders.CONTENT_ENCODING);
        if (types.size() != 1 || encodings.size() > 1
                || !encodings.isEmpty() && !"identity".equalsIgnoreCase(encodings.get(0))) {
            return false;
        }
        try {
            final MediaType type = MediaType.parseMediaType(types.get(0));
            return "application".equalsIgnoreCase(type.getType()) && !type.isWildcardSubtype()
                    && ("json".equalsIgnoreCase(type.getSubtype()) || type.getSubtype().toLowerCase(Locale.ROOT).endsWith("+json"));
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private static List<String> values(final Map<String, List<String>> headers, final String name) {
        return headers.entrySet().stream().filter(entry -> name.equalsIgnoreCase(entry.getKey()))
                .flatMap(entry -> entry.getValue().stream()).toList();
    }

    record Rule(String field, String value) {
    }

    private record Endpoint(String host, int port, String path) {
    }

    private record Scope(Endpoint endpoint, String protocol, int status) {
    }
}
