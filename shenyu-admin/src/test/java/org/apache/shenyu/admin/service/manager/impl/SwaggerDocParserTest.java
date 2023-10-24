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

import com.google.gson.JsonObject;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SwaggerDocParserTest {

    public static final String DOC_INFO_JSON = "{\n"
        + "    \"swagger\":\"2.0\",\n"
        + "    \"info\":{\n"
        + "        \"description\":\"shenyu-examples-http-swagger2 API\",\n"
        + "        \"version\":\"2.3.0\",\n"
        + "        \"title\":\"shenyu-examples-http-swagger2 API\",\n"
        + "        \"contact\":{\n"
        + "            \"name\":\"ShenYu\",\n"
        + "            \"url\":\"https://github.com/apache/shenyu\",\n"
        + "            \"email\":\"dev@shenyu.apache.org\"\n"
        + "        }\n"
        + "    },\n"
        + "    \"host\":\"127.0.0.1:8190\",\n"
        + "    \"basePath\":\"/\",\n"
        + "    \"tags\":[\n"
        + "        {\n"
        + "            \"name\":\"Order API\",\n"
        + "            \"description\":\"Order Controller\"\n"
        + "        },\n"
        + "        {\n"
        + "            \"name\":\"http-test-controller\",\n"
        + "            \"description\":\"Http Test Controller\"\n"
        + "        }\n"
        + "    ],\n"
        + "    \"paths\":{\n"
        + "        \"/order/path/{id}/{name}\":{\n"
        + "            \"get\":{\n"
        + "                \"tags\":[\n"
        + "                    \"Order API\"\n"
        + "                ],\n"
        + "                \"summary\":\"getPathVariable\",\n"
        + "                \"description\":\"get path variable.\",\n"
        + "                \"operationId\":\"getPathVariableUsingGET_1\",\n"
        + "                \"produces\":[\n"
        + "                    \"*/*\"\n"
        + "                ],\n"
        + "                \"parameters\":[\n"
        + "                    {\n"
        + "                        \"name\":\"X-Access-Token\",\n"
        + "                        \"in\":\"header\",\n"
        + "                        \"description\":\"user auth\",\n"
        + "                        \"required\":false,\n"
        + "                        \"type\":\"string\"\n"
        + "                    },\n"
        + "                    {\n"
        + "                        \"name\":\"id\",\n"
        + "                        \"in\":\"path\",\n"
        + "                        \"description\":\"id\",\n"
        + "                        \"required\":true,\n"
        + "                        \"type\":\"string\"\n"
        + "                    },\n"
        + "                    {\n"
        + "                        \"name\":\"name\",\n"
        + "                        \"in\":\"path\",\n"
        + "                        \"description\":\"name\",\n"
        + "                        \"required\":true,\n"
        + "                        \"type\":\"string\"\n"
        + "                    }\n"
        + "                ],\n"
        + "                \"responses\":{\n"
        + "                    \"200\":{\n"
        + "                        \"description\":\"OK\",\n"
        + "                        \"schema\":{\n"
        + "                            \"$ref\":\"#/definitions/OrderDTO\"\n"
        + "                        }\n"
        + "                    },\n"
        + "                \"deprecated\":false\n"
        + "             }\n"
        + "           }\n"
        + "        },\n"
        + "        \"/test/payment\":{\n"
        + "            \"post\":{\n"
        + "                \"tags\":[\n"
        + "                    \"http-test-controller\"\n"
        + "                ],\n"
        + "                \"summary\":\"payment\",\n"
        + "                \"description\":\"The user pays the order.\",\n"
        + "                \"operationId\":\"postUsingPOST\",\n"
        + "                \"consumes\":[\n"
        + "                    \"application/json\"\n"
        + "                ],\n"
        + "                \"produces\":[\n"
        + "                    \"*/*\"\n"
        + "                ],\n"
        + "                \"parameters\":[\n"
        + "                    {\n"
        + "                        \"name\":\"X-Access-Token\",\n"
        + "                        \"in\":\"header\",\n"
        + "                        \"description\":\"user auth\",\n"
        + "                        \"required\":false,\n"
        + "                        \"type\":\"string\"\n"
        + "                    },\n"
        + "                    {\n"
        + "                        \"in\":\"body\",\n"
        + "                        \"name\":\"userDTO\",\n"
        + "                        \"description\":\"userDTO\",\n"
        + "                        \"required\":true,\n"
        + "                        \"schema\":{\n"
        + "                            \"$ref\":\"#/definitions/UserDTO\"\n"
        + "                        }\n"
        + "                    }\n"
        + "                ],\n"
        + "                \"responses\":{\n"
        + "                    \"200\":{\n"
        + "                        \"description\":\"OK\",\n"
        + "                        \"schema\":{\n"
        + "                            \"$ref\":\"#/definitions/UserDTO\"\n"
        + "                        }\n"
        + "                    }\n"
        + "                },\n"
        + "                \"deprecated\":false\n"
        + "            }\n"
        + "        }\n"
        + "    },\n"
        + "    \"definitions\":{\n"
        + "        \"Mono«string»\":{\n"
        + "            \"type\":\"object\",\n"
        + "            \"title\":\"Mono«string»\"\n"
        + "        },\n"
        + "        \"OrderDTO\":{\n"
        + "            \"type\":\"object\",\n"
        + "            \"required\":[\n"
        + "                \"id\",\n"
        + "                \"name\"\n"
        + "            ],\n"
        + "            \"properties\":{\n"
        + "                \"id\":{\n"
        + "                    \"type\":\"string\",\n"
        + "                    \"example\":100000,\n"
        + "                    \"description\":\"id\"\n"
        + "                },\n"
        + "                \"name\":{\n"
        + "                    \"type\":\"string\",\n"
        + "                    \"example\":\"jack\",\n"
        + "                    \"description\":\"name\"\n"
        + "                }\n"
        + "            },\n"
        + "            \"title\":\"OrderDTO\"\n"
        + "        },\n"
        + "        \"UserDTO\":{\n"
        + "            \"type\":\"object\",\n"
        + "            \"required\":[\n"
        + "                \"userId\"\n"
        + "            ],\n"
        + "            \"properties\":{\n"
        + "                \"userId\":{\n"
        + "                    \"type\":\"string\",\n"
        + "                    \"example\":100000,\n"
        + "                    \"description\":\"user id\"\n"
        + "                },\n"
        + "                \"userName\":{\n"
        + "                    \"type\":\"string\",\n"
        + "                    \"example\":\"shenyu\",\n"
        + "                    \"description\":\"user name\"\n"
        + "                }\n"
        + "            },\n"
        + "            \"title\":\"UserDTO\"\n"
        + "        }\n"
        + "    }\n"
        + "}";

    @InjectMocks
    private SwaggerDocParser swaggerDocParser;

    @Test
    public void testParseJson() {
        JsonObject docRoot = GsonUtils.getInstance().fromJson(DOC_INFO_JSON, JsonObject.class);
        docRoot.addProperty("basePath", "/" + "testClusterName");
        DocInfo docInfo = swaggerDocParser.parseJson(docRoot);
        assert docInfo.getDocModuleList().get(0).getModule().equals("Order API");
    }
}
