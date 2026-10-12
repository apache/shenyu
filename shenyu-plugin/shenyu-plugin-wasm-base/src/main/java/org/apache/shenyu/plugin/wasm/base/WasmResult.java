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

package org.apache.shenyu.plugin.wasm.base;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * Validated guest result, detached from guest memory before continuing the reactive chain.
 */
final class WasmResult {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
            .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION);

    private static final Set<String> FIELDS = Set.of("version", "action", "status", "body", "requestHeaders", "responseHeaders");

    private static final Set<String> FRAMING_HEADERS = Set.of("host", "content-length", "transfer-encoding", "connection",
            "keep-alive", "proxy-authenticate", "proxy-authorization", "te", "trailer", "upgrade");

    private static final Pattern HEADER_NAME = Pattern.compile("[!#$%&'*+.^_`|~0-9A-Za-z-]+");

    private final boolean reject;

    private final int status;

    private final byte[] body;

    private final Map<String, List<String>> requestHeaders;

    private final Map<String, List<String>> responseHeaders;

    private WasmResult(final boolean reject, final int status, final byte[] body,
                       final Map<String, List<String>> requestHeaders, final Map<String, List<String>> responseHeaders) {
        this.reject = reject;
        this.status = status;
        this.body = body;
        this.requestHeaders = requestHeaders;
        this.responseHeaders = responseHeaders;
    }

    static WasmResult continueChain() {
        return new WasmResult(false, 0, new byte[0], Map.of(), Map.of());
    }

    static WasmResult parse(final byte[] bytes) {
        final JsonNode node;
        try {
            String json = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString();
            node = MAPPER.readTree(json);
        } catch (CharacterCodingException e) {
            throw new ShenyuWasmException("WASM result is not UTF-8", e);
        } catch (IOException e) {
            throw new ShenyuWasmException("WASM result is not valid JSON", e);
        }
        if (Objects.isNull(node) || !node.isObject() || !node.path("version").isIntegralNumber()
                || !node.path("version").canConvertToInt() || node.path("version").intValue() != 1) {
            throw new ShenyuWasmException("WASM result requires an object with version 1");
        }
        node.fieldNames().forEachRemaining(field -> {
            if (!FIELDS.contains(field)) {
                throw new ShenyuWasmException("Unsupported WASM result field: " + field);
            }
        });
        String action = node.path("action").asText();
        if (!"CONTINUE".equals(action) && !"REJECT".equals(action)) {
            throw new ShenyuWasmException("WASM result action must be CONTINUE or REJECT");
        }
        boolean reject = "REJECT".equals(action);
        int status = 0;
        byte[] body = new byte[0];
        if (reject) {
            JsonNode statusNode = node.path("status");
            if (!statusNode.isIntegralNumber() || !statusNode.canConvertToInt() || statusNode.intValue() < 400 || statusNode.intValue() > 599) {
                throw new ShenyuWasmException("WASM rejection requires an HTTP status from 400 to 599");
            }
            status = statusNode.intValue();
            if (node.has("body")) {
                if (!node.get("body").isTextual()) {
                    throw new ShenyuWasmException("WASM rejection body must be a string");
                }
                body = node.get("body").textValue().getBytes(StandardCharsets.UTF_8);
            }
            if (node.has("requestHeaders")) {
                throw new ShenyuWasmException("WASM rejection cannot modify request headers");
            }
        } else if (node.has("status") || node.has("body")) {
            throw new ShenyuWasmException("WASM continuation cannot set a response status or body");
        }
        return new WasmResult(reject, status, body, parseHeaders(node.path("requestHeaders")), parseHeaders(node.path("responseHeaders")));
    }

    private static Map<String, List<String>> parseHeaders(final JsonNode node) {
        if (node.isMissingNode()) {
            return Map.of();
        }
        if (!node.isObject()) {
            throw new ShenyuWasmException("WASM headers must be an object of string arrays");
        }
        Map<String, List<String>> headers = new LinkedHashMap<>();
        Iterator<Map.Entry<String, JsonNode>> fields = node.fields();
        while (fields.hasNext()) {
            Map.Entry<String, JsonNode> field = fields.next();
            String name = field.getKey();
            if (!HEADER_NAME.matcher(name).matches() || FRAMING_HEADERS.contains(name.toLowerCase(Locale.ROOT))) {
                throw new ShenyuWasmException("Invalid or unsupported WASM header name");
            }
            if (!field.getValue().isArray() || headers.containsKey(name.toLowerCase(Locale.ROOT))) {
                throw new ShenyuWasmException("WASM header values must be arrays with unique header names");
            }
            List<String> values = new ArrayList<>();
            for (JsonNode value : field.getValue()) {
                if (!value.isTextual() || value.textValue().chars().anyMatch(c -> c < 32 && c != 9 || c == 127)) {
                    throw new ShenyuWasmException("Invalid WASM header value");
                }
                values.add(value.textValue());
            }
            headers.put(name.toLowerCase(Locale.ROOT), List.copyOf(values));
        }
        return headers;
    }

    Mono<Void> apply(final ServerWebExchange exchange, final Function<ServerWebExchange, Mono<Void>> continuation) {
        replaceHeaders(exchange.getResponse().getHeaders(), responseHeaders);
        if (reject) {
            exchange.getResponse().setRawStatusCode(status);
            exchange.getResponse().getHeaders().setContentLength(body.length);
            if (!exchange.getResponse().getHeaders().containsKey(HttpHeaders.CONTENT_TYPE)) {
                exchange.getResponse().getHeaders().setContentType(new MediaType("text", "plain", StandardCharsets.UTF_8));
            }
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body)));
        }
        if (requestHeaders.isEmpty()) {
            return continuation.apply(exchange);
        }
        final HttpHeaders headers = new HttpHeaders();
        exchange.getRequest().getHeaders().forEach((name, values) -> headers.put(name, new ArrayList<>(values)));
        replaceHeaders(headers, requestHeaders);
        ServerHttpRequest request = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public HttpHeaders getHeaders() {
                return HttpHeaders.readOnlyHttpHeaders(headers);
            }
        };
        return continuation.apply(exchange.mutate().request(request).build());
    }

    private void replaceHeaders(final HttpHeaders target, final Map<String, List<String>> updates) {
        updates.forEach((name, values) -> {
            if (values.isEmpty()) {
                target.remove(name);
            } else {
                target.put(name, values);
            }
        });
    }
}
