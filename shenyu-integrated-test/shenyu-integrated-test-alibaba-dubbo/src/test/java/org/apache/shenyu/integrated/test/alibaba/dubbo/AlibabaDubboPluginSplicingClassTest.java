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

package org.apache.shenyu.integrated.test.alibaba.dubbo;

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.DubboTest;
import org.apache.shenyu.integratedtest.common.dto.ListResp;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class AlibabaDubboPluginSplicingClassTest extends AbstractPluginDataInit {

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.DUBBO.getName(), "{\"register\":\"zookeeper://shenyu-zk:2181\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testFindById() throws IOException {
        DubboTest dubboTest = HttpHelper.INSTANCE.getFromGateway("/dubbo/demo/findById?id=1", new TypeToken<DubboTest>() {
        }.getType());
        assertEquals("hello world shenyu Alibaba Dubbo, findById", dubboTest.getName());
    }

    @Test
    public void testFindAll() throws IOException {
        DubboTest dubboTest = HttpHelper.INSTANCE.getFromGateway("/dubbo/demo/findAll", DubboTest.class);
        assertEquals("hello world shenyu Alibaba Dubbo , findAll", dubboTest.getName());
    }

    @Test
    public void testInsert() throws IOException {
        DubboTest req = new DubboTest("1", "insertName");
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/demo/insert", req, DubboTest.class);
        assertEquals("hello world shenyu Alibaba Dubbo: insertName", dubboTest.getName());
        assertEquals("1", dubboTest.getId());
    }

    @Test
    public void testFindList() throws IOException {
        ListResp listResp = HttpHelper.INSTANCE.getFromGateway("/dubbo/demo/findList", ListResp.class);
        List<DubboTest> users = listResp.getUsers();
        assertEquals(listResp.getTotal().intValue(), users.size());
        assertEquals("test", users.get(0).getName());
        assertEquals("1", users.get(0).getId());
    }
}
