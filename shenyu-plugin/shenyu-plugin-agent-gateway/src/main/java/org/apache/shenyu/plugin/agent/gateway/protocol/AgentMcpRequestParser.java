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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.StreamReadConstraints;
import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.http.HttpHeaders;

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Strict request validation for the stateless MCP tools subset.
 * HTTP body acquisition, authentication and authorization belong to the caller.
 */
public final class AgentMcpRequestParser {

    public static final String VERSION = "2026-07-28";

    private static final String META_PREFIX = "io.modelcontextprotocol/";

    private static final String ENCODED_PREFIX = "=?base64?";

    private static final String ENCODED_SUFFIX = "?=";

    private static final Set<String> METHODS = Set.of("server/discover", "tools/list", "tools/call");

    private final ObjectMapper mapper = new ObjectMapper(JsonFactory.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
            .streamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(64).build()).build())
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    /**
     * Parse one bounded UTF-8 request and validate its mirrored headers.
     * No client-supplied metadata is converted to trusted identity.
     *
     * @param body the already acquired request bytes
     * @param headers the request headers
     * @param maxRequestBytes the configured positive byte limit
     * @return an independent validated request
     */
    public AgentMcpRequest parse(final byte[] body, final HttpHeaders headers, final int maxRequestBytes) {
        Objects.requireNonNull(body, "body");
        Objects.requireNonNull(headers, "headers");
        if (maxRequestBytes <= 0) {
            throw new IllegalArgumentException("maxRequestBytes must be positive");
        }
        if (body.length > maxRequestBytes) {
            throw failure(413, -32600, "Request body exceeds the configured limit", null);
        }
        JsonNode request;
        try {
            request = mapper.readTree(decodeUtf8(body));
        } catch (JsonProcessingException | CharacterCodingException error) {
            throw failure(400, -32700, "Parse error", null);
        }
        if (Objects.isNull(request) || request.isMissingNode()) {
            throw failure(400, -32700, "Parse error", null);
        }
        if (!request.isObject() || !request.path("jsonrpc").isTextual() || !"2.0".equals(request.path("jsonrpc").textValue())
                || !request.path("method").isTextual() || !(request.path("id").isTextual() || request.path("id").isIntegralNumber())
                || request.has("result") || request.has("error")) {
            throw failure(400, -32600, "Expected a single JSON-RPC request with an ID", null);
        }
        JsonNode id = request.get("id");
        String method = request.get("method").textValue();
        JsonNode params = request.path("params");
        JsonNode meta = params.path("_meta");
        JsonNode version = meta.path(META_PREFIX + "protocolVersion");
        if (!params.isObject() || !meta.isObject() || !version.isTextual() || version.textValue().isEmpty()
                || !meta.path(META_PREFIX + "clientCapabilities").isObject()) {
            throw failure(400, -32602, "Missing or invalid request metadata", id);
        }
        validateHeader(headers, "MCP-Protocol-Version", version.textValue(), false, id);
        validateHeader(headers, "Mcp-Method", method, false, id);
        if (!VERSION.equals(version.textValue())) {
            ObjectNode data = mapper.createObjectNode();
            data.put("requested", version.textValue());
            data.putArray("supported").add(VERSION);
            throw new AgentMcpProtocolException(400, -32022, "Unsupported protocol version", id, data);
        }
        if (!METHODS.contains(method)) {
            throw failure(404, -32601, "Method not found", id);
        }
        validateParams(method, params, id);
        if ("tools/call".equals(method)) {
            validateHeader(headers, "Mcp-Name", params.get("name").textValue(), true, id);
        }
        return new AgentMcpRequest(id, method, (ObjectNode) params);
    }

    private void validateParams(final String method, final JsonNode params, final JsonNode id) {
        if ("tools/call".equals(method) && (!params.path("name").isTextual() || params.path("name").textValue().isEmpty()
                || params.has("arguments") && !params.get("arguments").isObject())) {
            throw failure(400, -32602, "Invalid tool call parameters", id);
        }
        if ("tools/list".equals(method) && params.has("cursor") && !params.get("cursor").isTextual()) {
            throw failure(400, -32602, "Invalid tools cursor", id);
        }
    }

    private void validateHeader(final HttpHeaders headers, final String name, final String expected, final boolean encoded, final JsonNode id) {
        List<String> values = headers.get(name);
        if (Objects.isNull(values) || values.size() != 1 || !isSafeHeader(values.get(0))) {
            throw failure(400, -32020, "Missing or invalid mirrored header", id);
        }
        String actual = values.get(0);
        if (encoded && actual.startsWith(ENCODED_PREFIX) && actual.endsWith(ENCODED_SUFFIX)) {
            String base64 = actual.substring(ENCODED_PREFIX.length(), actual.length() - ENCODED_SUFFIX.length());
            try {
                byte[] decoded = Base64.getDecoder().decode(base64);
                if (!Base64.getEncoder().encodeToString(decoded).equals(base64)) {
                    throw failure(400, -32020, "Invalid mirrored header encoding", id);
                }
                actual = decodeUtf8(decoded);
            } catch (IllegalArgumentException | CharacterCodingException error) {
                throw failure(400, -32020, "Invalid mirrored header encoding", id);
            }
        }
        if (!expected.equals(actual)) {
            throw failure(400, -32020, "Mirrored header does not match the request", id);
        }
    }

    private boolean isSafeHeader(final String value) {
        if (Objects.isNull(value) || value.isEmpty() || value.charAt(0) <= ' ' || value.charAt(value.length() - 1) <= ' ') {
            return false;
        }
        for (int index = 0; index < value.length(); index++) {
            char character = value.charAt(index);
            if (character > '~' || character < ' ' && character != '\t') {
                return false;
            }
        }
        return true;
    }

    private String decodeUtf8(final byte[] bytes) throws CharacterCodingException {
        return StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString();
    }

    private AgentMcpProtocolException failure(final int status, final int code, final String message, final JsonNode id) {
        return new AgentMcpProtocolException(status, code, message, id, null);
    }
}
