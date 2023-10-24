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

package org.apache.shenyu.integrated.test.brpc;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.BigObject;
import org.apache.shenyu.integratedtest.common.dto.BrpcTest;
import org.apache.shenyu.integratedtest.common.dto.ComplexObjects;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class BrpcPluginTest extends AbstractPluginDataInit {

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.BRPC.getName(),
                "{\"corethreads\":0, \"threads\":2147483647, \"queues\":0, \"threadpool\":\"shared\"}");
        assertThat(pluginResult, Matchers.is("success"));
    }

    @Test
    public void testGetArray() throws IOException {
        String[] data = new String[]{"foo", "bar", "yoo"};
        Map<String, String[]> param = Maps.newHashMap();
        param.put("param", data);
        String[] response = HttpHelper.INSTANCE.postGateway("/brpc/getArray", param, String[].class);
        assertThat(response.length, is(3));
    }

    @Test
    public void testGetUsersList0() throws IOException {
        BrpcTest body = new BrpcTest();
        body.setUserId(1L);
        body.setUserName("test");
        List<BrpcTest> list = new ArrayList<>();
        list.add(body);
        Map<String, List<BrpcTest>> param = Maps.newHashMap();
        param.put("inputUsers", list);
        List<BrpcTest> response = HttpHelper.INSTANCE.postGateway("/brpc/getUsersList0", param, List.class);
        assertThat(response.size(), is(1));
    }

    @Test
    public void testGetUsersList1() throws IOException {
        BrpcTest body = new BrpcTest();
        body.setUserId(1L);
        body.setUserName("test");
        List<BrpcTest> list = new ArrayList<>();
        list.add(body);
        Map<String, Object> param = Maps.newHashMap();
        param.put("inputUsers", list);
        param.put("param", new String[]{"foo", "bar", "yoo"});
        List<BrpcTest> response = HttpHelper.INSTANCE.postGateway("/brpc/getUsersList1", param, List.class);
        assertThat(response.size(), is(1));
    }

    @Test
    public void testGetUserByObj() throws IOException {
        BrpcTest body = new BrpcTest();
        body.setUserId(1L);
        body.setUserName("test");
        Map<String, BrpcTest> param = Maps.newHashMap();
        param.put("inputUser", body);
        BrpcTest brpcTest = HttpHelper.INSTANCE.postGateway("/brpc/getUserByObj", param, BrpcTest.class);
        assertThat(brpcTest.getUserId(), is(1L));
        assertThat(brpcTest.getUserName(), is("test"));
    }

    @Test
    public void testBigObject0() throws IOException {
        BigObject response = HttpHelper.INSTANCE.postGateway("/brpc/bigObject0", BigObject.class);
        assertNotNull(response);
    }

    @Test
    public void testComplexObjects() throws IOException {
        BrpcTest body = new BrpcTest();
        body.setUserId(1L);
        body.setUserName("test");
        Map<String, BrpcTest> param = Maps.newHashMap();
        param.put("user", body);
        ComplexObjects complexObjects = HttpHelper.INSTANCE.postGateway("/brpc/complexObjects", param, ComplexObjects.class);
        assertThat(complexObjects.getUser().getUserId(), is(1L));
        assertThat(complexObjects.getUser().getUserName(), is("new user"));
    }
}
