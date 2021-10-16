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

package org.apache.shenyu.integratedtest.alibaba.dubbo;

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.AdminResponse;
import org.apache.shenyu.integratedtest.common.dto.DubboTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class AlibabaDubboPluginTest extends AbstractPluginDataInit {
    
    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.DUBBO.getName(), "{\"register\":\"zookeeper://shenyu-zk:2181\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testFindById() throws IOException {
        AdminResponse<DubboTest> dubboTest = HttpHelper.INSTANCE.getFromGateway("/dubbo/findById?id=1", new TypeToken<AdminResponse<DubboTest>>() {
        }.getType());
        assertEquals("hello world shenyu Alibaba Dubbo, findById", dubboTest.getData().getName());
    }
}
