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

package org.apache.shenyu.integrated.test.admin;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class AdminPluginUploadTest extends AbstractPluginDataInit {

    // jar build in dockerfile
    public static final String JAR_PATH = "/opt/shenyu-integrated-test-upload-plugin-case/shenyu-custom-plugin.jar";

    private String jarTxt;

    @BeforeAll
    public void setup() throws IOException {
        Path path = Paths.get(JAR_PATH);
        byte[] jarData = Files.readAllBytes(path);
        jarTxt = Base64.getEncoder().encodeToString(jarData);
    }

    @Test
    public void testUploadPlugin() throws IOException {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setName("CustomPlugin");
        pluginData.setRole("Test");
        pluginData.setId("1");
        pluginData.setPluginJar(jarTxt);
        HttpHelper.INSTANCE.postGateway("/shenyu/plugin/saveOrUpdate", pluginData, String.class);
        SelectorData selectorData = new SelectorData();
        selectorData.setPluginId("1");
        selectorData.setId("2");
        selectorData.setPluginId(pluginData.getId());
        selectorData.setName("/all");
        selectorData.setPluginName("CustomPlugin");
        selectorData.setType(0);
        selectorData.setContinued(false);
        selectorData.setLogged(true);
        selectorData.setEnabled(true);
        selectorData.setMatchRestful(false);
        selectorData.setSort(1);
        ConditionData selectorConditionData = new ConditionData();
        selectorConditionData.setParamName("uri");
        selectorConditionData.setOperator("pathPattern");
        selectorConditionData.setParamName("/");
        selectorConditionData.setParamValue("");
        selectorData.setConditionList(Collections.singletonList(selectorConditionData));
        HttpHelper.INSTANCE.postGateway("/shenyu/plugin/selector/saveOrUpdate", selectorData, String.class);
    }

    @Test
    public void testPluginEnableByUpload() throws IOException {
        String responseStr = HttpHelper.INSTANCE.getHttpService("http://localhost:9195/http/test", null, String.class);
        assertEquals("CustomPlugin-version::1", responseStr);
    }

}
