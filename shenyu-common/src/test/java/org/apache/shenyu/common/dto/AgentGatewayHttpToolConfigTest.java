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

package org.apache.shenyu.common.dto;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Shared plan validation, fixed targets, typed arguments and immutable configuration.
 */
class AgentGatewayHttpToolConfigTest {

    @Test
    void emptyPlansPreserveExistingRules() {
        assertEquals(0, AgentGatewayHttpToolConfig.parse(null, Set.of()).getTools().size());
        assertEquals(0, AgentGatewayMcpConfig.parse(JsonParser.parseString("{}"), true).getHttpTools().getTools().size());
    }

    @Test
    void capturesConfigurationAndReturnsPrivateSchemas() {
        JsonObject source = plans("POST", "/orders/status");
        AgentGatewayHttpToolConfig.Tool tool = parse(source);
        source.getAsJsonObject("lookup").getAsJsonObject("requestConfig").getAsJsonObject("requestTemplate").addProperty("url", "/changed");
        tool.getInputSchema().addProperty("type", "array");
        assertEquals("/orders/status", tool.requestPath(arguments("ABC")));
        assertEquals("object", tool.getInputSchema().get("type").getAsString());
        assertEquals("ABC", tool.requestBody(arguments("ABC")).get("id").getAsString());
    }

    @Test
    void delegationIsOptionalAndCapturesAnImmutablePolicySnapshot() {
        JsonObject source = plans("GET", "/orders/status");
        assertTrue(parse(source).getIdentityDelegation().isEmpty());
        JsonObject policy = delegation();
        source.getAsJsonObject("lookup").add("identityDelegation", policy);
        AgentGatewayHttpToolConfig.Tool tool = parse(source);
        policy.getAsJsonArray("scopes").add("admin");
        policy.addProperty("audience", "other-api");

        var binding = tool.getIdentityDelegation().orElseThrow();
        assertEquals("/orders/status", tool.getPath());
        assertEquals("https://orders-api/v1", binding.getAudience());
        assertEquals("orders_key-1", binding.getCredentialRef());
        assertEquals("v1.0", binding.getCredentialVersion());
        assertEquals(List.of("orders:read", "orders/item"), binding.getScopes());
        assertThrows(UnsupportedOperationException.class, () -> binding.getScopes().clear());
        assertEquals(30, binding.getTtlSeconds());
        assertTrue(binding.isRequireTenant());
    }

    @Test
    void acceptsExplicitPolicyLimitsAndOptionalTenant() {
        JsonObject policy = delegation();
        JsonArray scopes = new JsonArray();
        for (int i = 0; i < 16; i++) {
            scopes.add("s".repeat(63) + (char) ('a' + i));
        }
        policy.add("scopes", scopes);
        policy.addProperty("audience", "a".repeat(256));
        policy.addProperty("credentialRef", "_".repeat(64));
        policy.addProperty("credentialVersion", "-".repeat(64));
        policy.addProperty("requireTenant", false);
        policy.addProperty("ttlSeconds", 1);
        var binding = parseDelegation(policy);
        assertEquals(1, binding.getTtlSeconds());
        assertFalse(binding.isRequireTenant());
        assertEquals(16, binding.getScopes().size());
        policy.addProperty("ttlSeconds", 300);
        assertEquals(300, parseDelegation(policy).getTtlSeconds());
        scopes.add("one-too-many");
        assertThrows(IllegalArgumentException.class, () -> parseDelegation(policy));
    }

    @ParameterizedTest
    @ValueSource(strings = {"audience", "scopes", "credentialRef", "credentialVersion"})
    void requiresAnExplicitKeyAndTarget(final String field) {
        JsonObject policy = delegation();
        policy.remove(field);
        assertThrows(IllegalArgumentException.class, () -> parseDelegation(policy));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "audience|null", "audience|true", "audience|\"\"", "audience|\"orders *\"", "audience|\"orders*\"",
        "scopes|[]", "scopes|null", "scopes|\"read\"", "scopes|[true]", "scopes|[\"read\",\"read\"]",
        "scopes|[\"*\"]", "scopes|[\"add role\"]", "scopes|[\"\"]",
        "credentialRef|\"../key\"", "credentialRef|\"key.json\"", "credentialRef|42",
        "credentialVersion|\"v1/sub\"", "credentialVersion|null", "credentialVersion|\"\"",
        "ttlSeconds|0", "ttlSeconds|301", "ttlSeconds|null", "ttlSeconds|\"3\"", "ttlSeconds|3.1", "ttlSeconds|4294967297",
        "requireTenant|\"true\"", "requireTenant|0", "requireTenant|null", "header|\"Authorization\"" })
    void rejectsInvalidDelegationAtAdminAndGatewayBoundaries(final String field, final String value) {
        JsonObject policy = delegation();
        policy.add(field, JsonParser.parseString(value));
        JsonObject source = plans("GET", "/orders/status");
        source.getAsJsonObject("lookup").add("identityDelegation", policy);
        JsonObject mcp = JsonParser.parseString("{\"allowedTools\":[\"lookup\"]}").getAsJsonObject();
        mcp.add("httpTools", source);
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayMcpConfig.parse(mcp, true));
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayMcpConfig.parse(mcp, false));
    }

    @Test
    void rejectsOversizedBindingsAndExplicitNullDelegation() {
        JsonObject policy = delegation();
        for (String field : List.of("audience", "credentialRef", "credentialVersion")) {
            JsonObject oversized = policy.deepCopy();
            oversized.addProperty(field, "x".repeat("audience".equals(field) ? 257 : 65));
            assertThrows(IllegalArgumentException.class, () -> parseDelegation(oversized));
        }
        policy.getAsJsonArray("scopes").add("s".repeat(65));
        assertThrows(IllegalArgumentException.class, () -> parseDelegation(policy));
        assertThrows(IllegalArgumentException.class, () -> parseDelegation(null));
    }

    private JsonObject delegation() {
        return JsonParser.parseString("""
                {"audience":"https://orders-api/v1","scopes":["orders:read","orders/item"],"credentialRef":"orders_key-1","credentialVersion":"v1.0"}
                """).getAsJsonObject();
    }

    private AgentGatewayHttpToolConfig.IdentityDelegation parseDelegation(final JsonObject policy) {
        JsonObject source = plans("GET", "/orders/status");
        source.getAsJsonObject("lookup").add("identityDelegation", policy);
        return parse(source).getIdentityDelegation().orElseThrow();
    }

    @Test
    void escapesQueryValuesWithoutChangingTheFixedRoute() {
        AgentGatewayHttpToolConfig.Tool tool = parse(plans("GET", "/orders/status"));
        assertEquals("/orders/status?id=https%3A%2F%2Fevil.example%2F%3Fx%3D1%26y%3D2", tool.requestPath(arguments("https://evil.example/?x=1&y=2")));
    }

    @ParameterizedTest
    @ValueSource(strings = {"https://evil.example", "//evil", "/a/../b", "/a%2fb", "/a?target=x", "/a#x", "/a;session=x", "/a\\b", "/"})
    void rejectsUncontrolledTargets(final String path) {
        assertThrows(IllegalArgumentException.class, () -> parse(plans("GET", path)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"get", "DELETE", "CONNECT", "TRACE", "PUT"})
    void rejectsUnsupportedMethods(final String method) {
        assertThrows(IllegalArgumentException.class, () -> parse(plans(method, "/orders/status")));
    }

    @Test
    void rejectsUnmappedArgumentsAndWrongTypes() {
        AgentGatewayHttpToolConfig.Tool tool = parse(plans("GET", "/orders/status"));
        assertThrows(IllegalArgumentException.class, () -> tool.validate(new JsonObject()));
        JsonObject bad = arguments("1");
        bad.addProperty("extra", "injection");
        assertThrows(IllegalArgumentException.class, () -> tool.validate(bad));
        bad.remove("extra");
        bad.addProperty("id", 1);
        assertThrows(IllegalArgumentException.class, () -> tool.validate(bad));
        assertThrows(IllegalArgumentException.class, () -> tool.validate(arguments("x".repeat(2049))));
    }

    @Test
    void rejectsForeignToolNamesAndUnknownExtensions() {
        JsonObject source = plans("GET", "/orders/status");
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayHttpToolConfig.parse(source, Set.of("different")));
        source.getAsJsonObject("lookup").addProperty("credentials", "not-permitted");
        assertThrows(IllegalArgumentException.class, () -> parse(source));
    }

    @Test
    void rejectsHeadersSchemasAndBodyMappingOnGet() {
        JsonObject source = plans("GET", "/orders/status");
        source.getAsJsonObject("lookup").getAsJsonObject("requestConfig").getAsJsonObject("argsPosition").addProperty("id", "body");
        assertThrows(IllegalArgumentException.class, () -> parse(source));
        final JsonObject headerPlan = plans("POST", "/orders/status");
        headerPlan.getAsJsonObject("lookup").getAsJsonObject("requestConfig").getAsJsonObject("requestTemplate").add("headers", new JsonObject());
        assertThrows(IllegalArgumentException.class, () -> parse(headerPlan));
    }

    @Test
    void validatesIntegerNumberAndBooleanWithoutCoercion() {
        JsonObject source = plans("POST", "/orders/status");
        source.getAsJsonObject("lookup").getAsJsonObject("inputSchema").getAsJsonObject("properties").getAsJsonObject("id").addProperty("type", "integer");
        AgentGatewayHttpToolConfig.Tool tool = parse(source);
        JsonObject value = new JsonObject();
        value.addProperty("id", 12);
        tool.validate(value);
        value.addProperty("id", 1.5);
        assertThrows(IllegalArgumentException.class, () -> tool.validate(value));
    }

    private AgentGatewayHttpToolConfig.Tool parse(final JsonObject source) {
        return AgentGatewayHttpToolConfig.parse(source, Set.of("lookup")).getTools().get("lookup");
    }

    private JsonObject arguments(final String value) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("id", value);
        return arguments;
    }

    private JsonObject plans(final String method, final String path) {
        JsonObject source = JsonParser.parseString("""
                {"lookup":{"description":"Look up a record",
                "inputSchema":{"type":"object","properties":{"id":{"type":"string"}},"required":["id"]},
                "requestConfig":{"requestTemplate":{"method":"GET","url":"/orders/status"},"argsPosition":{"id":"query"}}}}
                """).getAsJsonObject();
        JsonObject config = source.getAsJsonObject("lookup").getAsJsonObject("requestConfig");
        config.getAsJsonObject("requestTemplate").addProperty("method", method);
        config.getAsJsonObject("requestTemplate").addProperty("url", path);
        config.getAsJsonObject("argsPosition").addProperty("id", "POST".equals(method) ? "body" : "query");
        return source;
    }
}
