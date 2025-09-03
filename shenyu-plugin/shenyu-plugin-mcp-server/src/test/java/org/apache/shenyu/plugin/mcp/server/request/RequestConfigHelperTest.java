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

package org.apache.shenyu.plugin.mcp.server.request;

import com.google.gson.JsonObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link RequestConfigHelper}.
 */
class RequestConfigHelperTest {

    @Test
    void testBasicGetRequest() {
        String configStr = "{\"requestTemplate\":{\"url\":\"/test\",\"method\":\"GET\"},\"argsPosition\":{}}";
        RequestConfigHelper helper = new RequestConfigHelper(configStr);
        
        assertEquals("/test", helper.getUrlTemplate());
        assertEquals("GET", helper.getMethod());
        assertFalse(helper.isArgsToJsonBody());
        assertNotNull(helper.getRequestTemplate());
        assertNotNull(helper.getArgsPosition());
    }

    @Test
    void testPostRequestWithJsonBody() {
        String configStr = "{\"requestTemplate\":{\"url\":\"/api/users\","
                + "\"method\":\"POST\",\"headers\":[{\"key\":\"Content-Type\","
                + "\"value\":\"application/json\"}],\"argsToJsonBody\":true},\"argsPosition\":{\"name\":\"body\",\"email\":\"body\"}}";
        RequestConfigHelper helper = new RequestConfigHelper(configStr);
        
        assertEquals("/api/users", helper.getUrlTemplate());
        assertEquals("POST", helper.getMethod());
        assertTrue(helper.isArgsToJsonBody());
        
        JsonObject requestTemplate = helper.getRequestTemplate();
        assertTrue(requestTemplate.has("headers"));
        
        JsonObject argsPosition = helper.getArgsPosition();
        assertEquals("body", argsPosition.get("name").getAsString());
        assertEquals("body", argsPosition.get("email").getAsString());
    }

    @Test
    void testPathParameterBuilding() {
        JsonObject argsPosition = new JsonObject();
        argsPosition.addProperty("id", "path");
        
        JsonObject inputJson = new JsonObject();
        inputJson.addProperty("id", "123");
        
        String result = RequestConfigHelper.buildPath("/users/{{.id}}", argsPosition, inputJson);
        assertEquals("/users/123", result);
    }

    @Test
    void testQueryParameterBuilding() {
        JsonObject argsPosition = new JsonObject();
        argsPosition.addProperty("page", "query");
        argsPosition.addProperty("size", "query");
        
        JsonObject inputJson = new JsonObject();
        inputJson.addProperty("page", "1");
        inputJson.addProperty("size", "10");
        
        String result = RequestConfigHelper.buildPath("/users", argsPosition, inputJson);
        assertTrue(result.contains("page=1"));
        assertTrue(result.contains("size=10"));
        assertTrue(result.contains("?"));
        assertTrue(result.contains("&"));
    }

    @Test
    void testMixedPathAndQueryParameters() {
        JsonObject argsPosition = new JsonObject();
        argsPosition.addProperty("userId", "path");
        argsPosition.addProperty("include", "query");
        
        JsonObject inputJson = new JsonObject();
        inputJson.addProperty("userId", "456");
        inputJson.addProperty("include", "profile");
        
        String result = RequestConfigHelper.buildPath("/users/{{.userId}}/details", argsPosition, inputJson);
        assertTrue(result.startsWith("/users/456/details"));
        assertTrue(result.contains("include=profile"));
    }

    @Test
    void testInvalidJsonConfig() {
        assertThrows(Exception.class, () -> {
            new RequestConfigHelper("invalid json");
        });
    }

    @Test
    void testMissingRequestTemplate() {
        RequestConfigHelper helper = new RequestConfigHelper("{\"argsPosition\":{}}");
        // This will fail because getRequestTemplate() returns null
        assertThrows(Exception.class, () -> {
            helper.getUrlTemplate();
        });
    }

    @Test
    void testMissingUrlInTemplate() {
        RequestConfigHelper helper = new RequestConfigHelper("{\"requestTemplate\":{\"method\":\"GET\"}}");
        assertThrows(Exception.class, () -> {
            helper.getUrlTemplate();
        });
    }

    @Test
    void testMissingMethodInTemplate() {
        RequestConfigHelper helper = new RequestConfigHelper("{\"requestTemplate\":{\"url\":\"/test\"}}");
        // getMethod() has a default value "GET", so it won't throw exception
        // Instead test that default method is returned
        assertEquals("GET", helper.getMethod());
    }

    @Test
    void testDefaultArgsToJsonBody() {
        String configStr = "{\"requestTemplate\":{\"url\":\"/test\",\"method\":\"GET\"},\"argsPosition\":{}}";
        RequestConfigHelper helper = new RequestConfigHelper(configStr);
        
        assertFalse(helper.isArgsToJsonBody());
    }

    @Test
    void testResponseTemplateExtraction() {
        String configStr = "{\"requestTemplate\":{\"url\":\"/test\",\"method\":\"GET\"},\"argsPosition\":{},\"responseTemplate\":{\"body\":\"{{.}}\"}}";
        RequestConfigHelper helper = new RequestConfigHelper(configStr);
        
        JsonObject responseTemplate = helper.getResponseTemplate();
        assertNotNull(responseTemplate);
        assertTrue(responseTemplate.has("body"));
        assertEquals("{{.}}", responseTemplate.get("body").getAsString());
    }

    @Test
    void testComplexPathTemplate() {
        JsonObject argsPosition = new JsonObject();
        argsPosition.addProperty("orgId", "path");
        argsPosition.addProperty("projectId", "path");
        argsPosition.addProperty("version", "query");
        
        JsonObject inputJson = new JsonObject();
        inputJson.addProperty("orgId", "apache");
        inputJson.addProperty("projectId", "shenyu");
        inputJson.addProperty("version", "2.7.0");
        
        String result = RequestConfigHelper.buildPath("/orgs/{{.orgId}}/projects/{{.projectId}}", argsPosition, inputJson);
        assertTrue(result.startsWith("/orgs/apache/projects/shenyu"));
        assertTrue(result.contains("version=2.7.0"));
    }

    @Test
    void testEmptyPathTemplate() {
        JsonObject argsPosition = new JsonObject();
        JsonObject inputJson = new JsonObject();
        
        String result = RequestConfigHelper.buildPath("/simple/path", argsPosition, inputJson);
        assertEquals("/simple/path", result);
    }

    @Test
    void testSpecialCharactersInParameters() {
        JsonObject argsPosition = new JsonObject();
        argsPosition.addProperty("query", "query");
        
        JsonObject inputJson = new JsonObject();
        inputJson.addProperty("query", "hello world & special chars");
        
        String result = RequestConfigHelper.buildPath("/search", argsPosition, inputJson);
        // The implementation doesn't URL encode, so check for raw string
        assertTrue(result.contains("query=hello world & special chars"));
    }
}
