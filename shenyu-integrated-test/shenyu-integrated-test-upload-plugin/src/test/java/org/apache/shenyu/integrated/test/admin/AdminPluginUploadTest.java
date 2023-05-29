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

import com.google.common.io.CharStreams;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class AdminPluginUploadTest extends AbstractPluginDataInit {

    private PluginData pluginData;

    private String jarTxt16;

    private String jarTxt17;

    @BeforeAll
    public void setup() throws IOException {
        InputStream is = AdminPluginUploadTest.class.getResourceAsStream("/CustomPlugin-V1.6.txt");
        assert is != null;
        jarTxt16 = CharStreams.toString(new InputStreamReader(is, StandardCharsets.UTF_8));
        is = AdminPluginUploadTest.class.getResourceAsStream("/CustomPlugin-V1.7.txt");
        assert is != null;
        jarTxt17 = CharStreams.toString(new InputStreamReader(is, StandardCharsets.UTF_8));
        pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setName("CustomPlugin");
        pluginData.setRole("Test");
        pluginData.setId("1");
        pluginData.setPluginJar(jarTxt16);
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

    @Test
    public void testPluginHotLoad() throws IOException {
        pluginData.setPluginJar(jarTxt17);
        HttpHelper.INSTANCE.postGateway("/shenyu/plugin/saveOrUpdate", pluginData, String.class);
        String responseStr = HttpHelper.INSTANCE.getHttpService("http://localhost:9195/http/test", null, String.class);
        assertEquals("CustomPlugin-version::2", responseStr);
    }

}
