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

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpHeaders;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpRequestParserTest {

    private final AgentMcpRequestParser parser = new AgentMcpRequestParser();

    @ParameterizedTest
    @ValueSource(strings = {"server/discover", "tools/list", "tools/call"})
    void shouldAcceptSupportedMethods(final String method) {
        AgentMcpRequest request = parse(body(method, "1", "2026-07-28"), headers(method));
        assertEquals(method, request.getMethod());
        assertEquals(1, request.getId().intValue());
        assertEquals("order_status", request.getParams().get("name").textValue());
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"1\"", "1", "-1", "0", "92233720368547758081234"})
    void shouldPreserveIdTypesAndLargeIntegers(final String id) {
        assertEquals(id, parse(body("tools/call", id, "2026-07-28"), headers("tools/call")).getId().toString());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "{", "{'jsonrpc':'2.0'}", "{jsonrpc:2}", "/* secret */ {}", "{} {}", "{\"id\":1,\"id\":2}"})
    void shouldRejectMalformedOrAmbiguousJson(final String body) {
        assertFailure(body, headers("tools/call"), 400, -32700, "null");
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "1", "{}", "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\"}",
            "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}"})
    void shouldRejectBatchesNotificationsAndResponses(final String body) {
        assertFailure(body, headers("tools/list"), 400, -32600, "null");
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "true", "1.5", "{}", "[]"})
    void shouldRejectInvalidIds(final String id) {
        assertFailure(body("tools/call", id, "2026-07-28"), headers("tools/call"), 400, -32600, "null");
    }

    @Test
    void shouldRejectRequestResponseHybrids() {
        String body = body("tools/list", "1", "2026-07-28");
        assertFailure(body.replace("\"jsonrpc\"", "\"error\":{},\"jsonrpc\""), headers("tools/list"), 400, -32600, "null");
    }

    @Test
    void shouldRequireTypedMetadata() {
        String body = body("tools/list", "\"private-id\"", "2026-07-28");
        assertFailure(body.replace("\"io.modelcontextprotocol/clientCapabilities\":{}", "\"io.modelcontextprotocol/clientCapabilities\":[]"),
                headers("tools/list"), 400, -32602, "\"private-id\"");
        assertFailure(body.replace("\"params\"", "\"ignored\""), headers("tools/list"), 400, -32602, "\"private-id\"");
    }

    @ParameterizedTest
    @ValueSource(strings = {"MCP-Protocol-Version", "Mcp-Method", "Mcp-Name"})
    void shouldRejectMissingDuplicateOrMismatchedHeaders(final String name) {
        HttpHeaders headers = headers("tools/call");
        String body = body("tools/call", "1", "2026-07-28");
        headers.remove(name);
        assertFailure(body, headers, 400, -32020, "1");
        headers.add(name, "wrong");
        assertFailure(body, headers, 400, -32020, "1");
        headers.add(name, "wrong");
        assertFailure(body, headers, 400, -32020, "1");
    }

    @ParameterizedTest
    @ValueSource(strings = {" order_status", "order_status ", "order\nstatus", "世界", "=?base64?%%%?=", "=?base64?/w==?=", "=?base64?YQ?="})
    void shouldRejectUnsafeOrMalformedNames(final String name) {
        HttpHeaders headers = headers("tools/call");
        headers.set("Mcp-Name", name);
        assertFailure(body("tools/call", "1", "2026-07-28"), headers, 400, -32020, "1");
    }

    @ParameterizedTest
    @ValueSource(strings = {"世界", " padded ", "line1\nline2", "=?base64?literal?="})
    void shouldDecodeNamesExactlyOnce(final String name) {
        HttpHeaders headers = headers("tools/call");
        headers.set("Mcp-Name", "=?base64?" + Base64.getEncoder().encodeToString(name.getBytes(StandardCharsets.UTF_8)) + "?=");
        ObjectNode params = parse(body("tools/call", "1", "2026-07-28"), headers("tools/call")).getParams();
        params.put("name", name);
        String body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":" + params + "}";
        assertEquals(name, parse(body, headers).getParams().get("name").textValue());
    }

    @Test
    void shouldUseCaseInsensitiveHeaderNamesButCaseSensitiveValues() {
        HttpHeaders headers = new HttpHeaders();
        headers.set("mcp-protocol-version", "2026-07-28");
        headers.set("mcp-method", "tools/list");
        parse(body("tools/list", "1", "2026-07-28"), headers);
        headers.set("mcp-method", "Tools/List");
        assertFailure(body("tools/list", "1", "2026-07-28"), headers, 400, -32020, "1");
    }

    @Test
    void shouldAdvertiseSupportedVersionsWithoutExposingRawRequest() {
        HttpHeaders headers = headers("tools/list");
        headers.set("MCP-Protocol-Version", "2025-11-25");
        AgentMcpProtocolException error = assertFailure(body("tools/list", "\"id\"", "2025-11-25"), headers, 400, -32022, "\"id\"");
        assertEquals("2026-07-28", error.toResponse().path("error").path("data").path("supported").get(0).textValue());
        error.toResponse().putObject("error").put("code", 0);
        assertEquals(-32022, error.toResponse().path("error").path("code").intValue());
        assertFalse(error.toResponse().toString().contains("order_status"));
    }

    @Test
    void shouldRejectUnknownMethodsWithoutInventingCapabilities() {
        assertFailure(body("resources/list", "1", "2026-07-28"), headers("resources/list"), 404, -32601, "1");
    }

    @Test
    void shouldValidateToolArgumentsAndCursorTypes() {
        String body = body("tools/call", "1", "2026-07-28");
        assertFailure(body.replace("\"arguments\":{}", "\"arguments\":[]"), headers("tools/call"), 400, -32602, "1");
        assertFailure(body.replace("\"name\":\"order_status\"", "\"name\":1"), headers("tools/call"), 400, -32602, "1");
        parse(body.replace("\"arguments\":{},", ""), headers("tools/call"));
        assertFailure(body("tools/list", "1", "2026-07-28").replace("\"arguments\":{}", "\"cursor\":1"),
                headers("tools/list"), 400, -32602, "1");
    }

    @Test
    void shouldBoundBytesRejectInvalidUtf8AndLimitNesting() {
        byte[] body = body("tools/list", "1", "2026-07-28").getBytes(StandardCharsets.UTF_8);
        parser.parse(body, headers("tools/list"), body.length);
        AgentMcpProtocolException limit = assertThrows(AgentMcpProtocolException.class, () -> parser.parse(body, headers("tools/list"), body.length - 1));
        assertEquals(413, limit.getHttpStatus());
        AgentMcpProtocolException utf8 = assertThrows(AgentMcpProtocolException.class, () -> parser.parse(new byte[]{(byte) 0xff}, headers("tools/list"), 1024));
        assertEquals(-32700, utf8.getCode());
        assertFailure("[".repeat(65) + "0" + "]".repeat(65), headers("tools/list"), 400, -32700, "null");
        assertThrows(IllegalArgumentException.class, () -> parser.parse(body, headers("tools/list"), 0));
    }

    @Test
    void shouldKeepRequestSnapshotsAndNotTreatMetadataAsIdentity() {
        AgentMcpRequest request = parse(body("tools/call", "1", "2026-07-28"), headers("tools/call"));
        request.getParams().put("name", "mutated");
        request.getParams().putObject("_meta").put("subject", "forged");
        assertEquals("order_status", request.getParams().get("name").textValue());
        assertFalse(request.getParams().get("_meta").has("subject"));
    }

    @Test
    void shouldSnapshotCapabilitiesWithoutLosingNestedTypesOrLargeIntegers() {
        String capabilities = "{\"sampling\":{},\"flag\":true,\"large\":92233720368547758081234,\"values\":[null,\"text\",false,0.5]}";
        AgentMcpRequest request = parse(body("tools/call", "1", "2026-07-28")
                .replace("\"io.modelcontextprotocol/clientCapabilities\":{}", "\"io.modelcontextprotocol/clientCapabilities\":" + capabilities), headers("tools/call"));
        JsonObject copy = request.getClientCapabilities();
        assertTrue(copy.get("sampling").isJsonObject());
        assertTrue(copy.get("flag").getAsBoolean());
        assertEquals("92233720368547758081234", copy.get("large").getAsString());
        assertTrue(copy.getAsJsonArray("values").get(0).isJsonNull());
        assertEquals("text", copy.getAsJsonArray("values").get(1).getAsString());
        assertFalse(copy.getAsJsonArray("values").get(2).getAsBoolean());
        assertEquals(0.5, copy.getAsJsonArray("values").get(3).getAsDouble());
        copy.remove("sampling");
        copy.getAsJsonArray("values").set(0, new com.google.gson.JsonPrimitive("changed"));
        assertTrue(request.getClientCapabilities().has("sampling"));
        assertTrue(request.getClientCapabilities().getAsJsonArray("values").get(0).isJsonNull());
    }

    @Test
    void shouldKeepConcurrentRequestsWithIdenticalIdsIndependent() {
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> Mono.fromCallable(() -> {
            String body = body("tools/call", "1", "2026-07-28").replace("\"arguments\":{}", "\"arguments\":{\"owner\":" + index + "}");
            AgentMcpRequest request = parse(body, headers("tools/call"));
            assertEquals(index, request.getParams().path("arguments").path("owner").intValue());
            assertTrue(request.getId().isIntegralNumber());
            return request;
        }).subscribeOn(Schedulers.parallel()), 8)).expectNextCount(32).verifyComplete();
    }

    private AgentMcpRequest parse(final String body, final HttpHeaders headers) {
        return parser.parse(body.getBytes(StandardCharsets.UTF_8), headers, 262144);
    }

    private AgentMcpProtocolException assertFailure(final String body, final HttpHeaders headers, final int status, final int code, final String id) {
        AgentMcpProtocolException error = assertThrows(AgentMcpProtocolException.class, () -> parse(body, headers));
        assertEquals(status, error.getHttpStatus());
        assertEquals(code, error.getCode());
        assertEquals(id, error.toResponse().get("id").toString());
        assertEquals("2.0", error.toResponse().get("jsonrpc").textValue());
        return error;
    }

    private String body(final String method, final String id, final String version) {
        return "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"method\":\"" + method + "\",\"params\":{\"name\":\"order_status\",\"arguments\":{},\"_meta\":{"
                + "\"io.modelcontextprotocol/protocolVersion\":\"" + version + "\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}";
    }

    private HttpHeaders headers(final String method) {
        HttpHeaders headers = new HttpHeaders();
        headers.set("MCP-Protocol-Version", "2026-07-28");
        headers.set("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            headers.set("Mcp-Name", "order_status");
        }
        return headers;
    }
}
