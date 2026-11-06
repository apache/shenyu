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

package org.apache.shenyu.admin.service.openapi;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Mapping;

import java.util.List;

/** Shared source for generator, import and real gateway-chain tests. */
final class OpenApiImportFixture {

    private OpenApiImportFixture() {
    }

    static JsonObject document() {
        return JsonParser.parseString("""
                {"openapi":"3.0.3","info":{"title":"Orders","version":"1"},"paths":{"/backend/orders":{
                  "parameters":[{"name":"id","in":"query","required":true,"schema":{"type":"string"}},
                                {"name":"verbose","in":"query","schema":{"type":"boolean"}}],
                  "get":{"operationId":"lookup","summary":"Look up an order","responses":{"200":{"description":"OK"}}},
                  "post":{"operationId":"create","summary":"Create an order","requestBody":{"required":true,"content":{
                    "application/json":{"schema":{"type":"object","properties":{"name":{"type":"string"},"count":{"type":"integer"}},"required":["name"]}}
                  }},"responses":{"200":{"description":"OK"}}}
                }}}
                """).getAsJsonObject();
    }

    static Mapping mapping(final String method) {
        return new Mapping(method, "/backend/orders", "/orders/status", "route", null);
    }

    static AgentGatewayOpenApiRequest request() {
        return request(document(), List.of(mapping("GET"), mapping("POST")));
    }

    static AgentGatewayOpenApiRequest request(final JsonObject source, final List<Mapping> mappings) {
        return new AgentGatewayOpenApiRequest("ns", "agent", "orders-v1", source.toString(), mappings);
    }
}
