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
            + "  \"openapi\": \"3.0.1\",\n"
            + "  \"info\": {\n"
            + "    \"title\": \"shenyu-examples-http-swagger3 API\",\n"
            + "    \"description\": \"shenyu-examples-http-swagger3 API\",\n"
            + "    \"version\": \"3.0.1\",\n"
            + "    \"contact\": {\n"
            + "      \"name\": \"ShenYu\",\n"
            + "      \"url\": \"https://github.com/apache/shenyu\",\n"
            + "      \"email\": \"dev@shenyu.apache.org\"\n"
            + "    }\n"
            + "  },\n"
            + "  \"servers\": [\n"
            + "    {\n"
            + "      \"url\": \"http://127.0.0.1:8190\",\n"
            + "      \"description\": \"Local server\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"tags\": [\n"
            + "    {\n"
            + "      \"name\": \"Order API\",\n"
            + "      \"description\": \"Order Controller\"\n"
            + "    }\n"
            + "  ],\n"
            + "  \"paths\": {\n"
            + "    \"/order/path/{id}/{name}\": {\n"
            + "      \"get\": {\n"
            + "        \"tags\": [\"Order API\"],\n"
            + "        \"summary\": \"Get path variable\",\n"
            + "        \"parameters\": [\n"
            + "          {\n"
            + "            \"name\": \"id\",\n"
            + "            \"in\": \"path\",\n"
            + "            \"required\": true,\n"
            + "            \"schema\": {\n"
            + "              \"type\": \"string\"\n"
            + "            }\n"
            + "          },\n"
            + "          {\n"
            + "            \"name\": \"name\",\n"
            + "            \"in\": \"path\",\n"
            + "            \"required\": true,\n"
            + "            \"schema\": {\n"
            + "              \"type\": \"string\"\n"
            + "            }\n"
            + "          }\n"
            + "        ],\n"
            + "        \"responses\": {\n"
            + "          \"200\": {\n"
            + "            \"description\": \"Successful response\",\n"
            + "            \"content\": {\n"
            + "              \"application/json\": {\n"
            + "                \"schema\": {\n"
            + "                  \"$ref\": \"#/components/schemas/OrderDTO\"\n"
            + "                }\n"
            + "              }\n"
            + "            }\n"
            + "          }\n"
            + "        }\n"
            + "      }\n"
            + "    }\n"
            + "  },\n"
            + "  \"components\": {\n"
            + "    \"schemas\": {\n"
            + "      \"OrderDTO\": {\n"
            + "        \"type\": \"object\",\n"
            + "        \"required\": [\"id\", \"name\"],\n"
            + "        \"properties\": {\n"
            + "          \"id\": {\n"
            + "            \"type\": \"string\",\n"
            + "            \"example\": \"100000\"\n"
            + "          },\n"
            + "          \"name\": {\n"
            + "            \"type\": \"string\",\n"
            + "            \"example\": \"jack\"\n"
            + "          }\n"
            + "        }\n"
            + "      }\n"
            + "    }\n"
            + "  }\n"
            + "}\n";

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
