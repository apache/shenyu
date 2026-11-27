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

package org.apache.shenyu.admin.service.manager.impl;

import com.google.gson.JsonParser;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocParameter;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SwaggerDocRequestBodyTest {

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void documentsJsonObjectRequestBody(final boolean referenced) {
        String schema = """
                {"type":"object","required":["product"],"properties":{
                    "product":{"type":"string","description":"Product code"},
                    "quantity":{"type":"integer"}
                }}
                """;
        String document = """
                {
                  "openapi":"3.0.3",
                  "info":{"title":"Orders","version":"1"},
                  "paths":{"/orders":{"post":{
                    "requestBody":{"required":true,"content":{"application/json":{"schema":%s}}},
                    "responses":{"201":{"description":"Created"}}
                  }}},
                  "components":{"schemas":{"Order":%s}}
                }
                """.formatted(referenced ? "{\"$ref\":\"#/components/schemas/Order\"}" : schema, schema);
        DocItem item = parseDocItem(document);
        List<DocParameter> parameters = item.getRequestParameters().get(0).getRefs();
        assertEquals(List.of("product", "quantity"), parameters.stream().map(DocParameter::getName).toList());
        assertEquals("Product code", parameters.get(0).getDescription());
        assertTrue(parameters.get(0).isRequired());
        assertEquals("integer", parameters.get(1).getType());
        assertFalse(parameters.get(1).isRequired());
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {
        "\"requestBody\":{\"content\":{\"text/plain\":{\"schema\":{\"type\":\"object\",\"properties\":{\"body\":{\"type\":\"string\"}}}}}},",
        "\"requestBody\":{\"content\":{\"application/json\":{\"schema\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}}},",
        "\"requestBody\":{\"content\":{\"application/json\":{\"schema\":{\"$ref\":\"#/components/schemas/\"}}}},"
    })
    void retainsOperationParametersWithoutSupportedBody(final String requestBody) {
        assertEquals(List.of("code"), existingAndBody(requestBody).stream().map(DocParameter::getName).toList());
    }

    @Test
    void keepsOperationParameterWhenABodyFieldHasTheSameName() {
        List<DocParameter> parameters = existingAndBody("""
                "requestBody":{"content":{"application/json":{"schema":{"properties":{
                    "code":{"type":"integer"},"quantity":{"type":"integer"}
                }}}}},
                """);
        assertEquals(List.of("code", "Request body"), parameters.stream().map(DocParameter::getName).toList());
        assertEquals("string", parameters.get(0).getType());
        assertEquals(List.of("code", "quantity"), parameters.get(1).getRefs().stream().map(DocParameter::getName).toList());
        assertEquals("integer", parameters.get(1).getRefs().get(0).getType());
    }

    @Test
    void keepsOperationWhenTheRequestBodySchemaIsBoolean() {
        assertEquals(List.of("code"), existingAndBody("""
                "requestBody":{"content":{"application/json":{"schema":true}}},
                """).stream().map(DocParameter::getName).toList());
    }

    @Test
    void keepsSupportedFieldsBesideBooleanAndUnionTypedRequestFields() {
        List<DocParameter> parameters = existingAndBody("""
                "requestBody":{"content":{"application/json":{"schema":{"type":"object","properties":{
                    "freeForm":true,"possiblyNull":{"type":["string","null"]},"quantity":{"type":"integer"}
                }}}}},
                """);
        assertEquals(List.of("code", "Request body"), parameters.stream().map(DocParameter::getName).toList());
        assertEquals(List.of("quantity"), parameters.get(1).getRefs().stream().map(DocParameter::getName).toList());
    }

    @Test
    void excludesReadOnlyFieldsFromARequiredRequestSchemaButKeepsThemInTheResponse() {
        DocItem item = parseDocItem("""
                {"openapi":"3.0.3","info":{"title":"Orders","version":"1"},
                 "paths":{"/orders":{"post":{
                   "requestBody":{"content":{"application/json":{"schema":{"$ref":"#/components/schemas/Order"}}}},
                   "responses":{"200":{"description":"OK","content":{"application/json":{"schema":{"$ref":"#/components/schemas/Order"}}}}}
                 }}},
                 "components":{"schemas":{"Order":{"type":"object","required":["id","quantity"],"properties":{
                   "id":{"type":"string","readOnly":true},"quantity":{"type":"integer"},"note":{"type":"string"}
                 }}}}}
                """);
        DocParameter body = item.getRequestParameters().get(0);
        assertEquals(List.of("quantity", "note"), body.getRefs().stream().map(DocParameter::getName).toList());
        assertFalse(body.isRequired());
        assertTrue(body.getRefs().get(0).isRequired());
        assertFalse(body.getRefs().get(1).isRequired());
        assertEquals(List.of("id", "quantity", "note"), item.getResponseParameters().stream().map(DocParameter::getName).toList());
    }

    @ParameterizedTest
    @CsvSource({"false,false", "true,true", ",false"})
    void preservesBodyAndFieldRequirementsInTheStoredDocument(final String required, final boolean expectedBodyRequired) {
        String bodyRequired = Objects.isNull(required) ? "" : "\"required\":" + required + ',';
        List<DocParameter> parameters = existingAndBody("""
                "requestBody":{%s"content":{"application/json":{"schema":{"type":"object","required":["id"],"properties":{
                    "id":{"type":"string"},"note":{"type":"string","description":"Optional note"}
                }}}}},
                """.formatted(bodyRequired));

        DocItem document = new DocItem();
        document.setRequestParameters(parameters);
        List<DocParameter> stored = JsonUtils.jsonToObject(JsonUtils.toJson(document), DocItem.class).getRequestParameters();
        assertEquals(List.of("code", "Request body"), stored.stream().map(DocParameter::getName).toList());
        DocParameter body = stored.get(1);
        assertEquals("object", body.getType());
        assertEquals(expectedBodyRequired, body.isRequired());
        assertEquals(List.of("id", "note"), body.getRefs().stream().map(DocParameter::getName).toList());
        assertTrue(body.getRefs().get(0).isRequired());
        assertFalse(body.getRefs().get(1).isRequired());
    }

    @Test
    void documentsARequiredBodyWithOnlyOptionalFields() {
        DocParameter body = existingAndBody("""
                "requestBody":{"required":true,"content":{"application/json":{"schema":{"type":"object","properties":{
                    "note":{"type":"string"}
                }}}}},
                """).get(1);
        assertEquals("Request body", body.getName());
        assertTrue(body.isRequired());
        assertFalse(body.getRefs().get(0).isRequired());
    }

    @Test
    void documentsARequiredEmptyObjectBody() {
        DocParameter body = existingAndBody("""
                "requestBody":{"required":true,"content":{"application/json":{"schema":{"type":"object"}}}},
                """).get(1);
        assertEquals("Request body", body.getName());
        assertTrue(body.isRequired());
        assertTrue(body.getRefs().isEmpty());
    }

    private List<DocParameter> existingAndBody(final String requestBody) {
        String document = """
                {"openapi":"3.1.0","info":{"title":"Orders","version":"1"},
                 "paths":{"/orders":{"post":{
                    "parameters":[{"name":"filter","in":"query","schema":{"$ref":"#/components/schemas/Filter"}}],
                    %s"responses":{"201":{"description":"Created"}}
                 }}},
                 "components":{"schemas":{"Filter":{"type":"object","properties":{"code":{"type":"string"}}}}}}
                """.formatted(Objects.isNull(requestBody) ? "" : requestBody);
        return parseDocItem(document).getRequestParameters();
    }

    private DocItem parseDocItem(final String document) {
        return new SwaggerDocParser().parseJson(JsonParser.parseString(document).getAsJsonObject())
                .getDocModuleList().get(0).getDocItems().get(0);
    }
}
