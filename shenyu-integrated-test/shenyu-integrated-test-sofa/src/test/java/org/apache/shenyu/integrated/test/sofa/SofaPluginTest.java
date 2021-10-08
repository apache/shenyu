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

package org.apache.shenyu.integrated.test.sofa;

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integrated.test.sofa.dto.SofaTestData;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.hamcrest.Matchers;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class SofaPluginTest extends AbstractPluginDataInit {

    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.SOFA.getName(), "{\"protocol\":\"zookeeper\",\"register\":\"shenyu-zk:2181\"}");
        assertThat(pluginResult, Matchers.is("success"));
    }

    @Test
    public void testHelloWorld() throws IOException {
        AdminResponse<SofaTestData> response = HttpHelper.INSTANCE.getFromGateway("/sofa/findById?id=1001", new TypeToken<AdminResponse<SofaTestData>>() { }.getType());
        assertThat(response.getData().getName(), is("hello world shenyu Sofa, findById"));
        assertThat(response.getData().getId(), is("1001"));
    }
}
