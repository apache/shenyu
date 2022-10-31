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

package org.apache.shenyu.integrated.test.combination;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.DubboTest;
import org.apache.shenyu.integratedtest.common.dto.MotanDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The integrated test for combination plugins about shared thread pool.
 */
public class SharedThreadPoolTest extends AbstractPluginDataInit {
    
    private static final Logger LOG = LoggerFactory.getLogger(SharedThreadPoolTest.class);
    
    @BeforeAll
    public static void setup() throws IOException {
        // for apache dubbo
        String pluginResult = initPlugin(PluginEnum.DUBBO.getName(), "{\"register\":\"zookeeper://shenyu-zk:2181\",\"threadpool\": \"shared\"}");
        assertThat(pluginResult, is("success"));
        // for grpc
        pluginResult = initPlugin(PluginEnum.GRPC.getName(), "{\"register\":\"zookeeper://shenyu-zk:2181\",\"threadpool\": \"shared\"}");
        assertThat(pluginResult, is("success"));
        // for motan
        pluginResult = initPlugin(PluginEnum.MOTAN.getName(), "{\"register\":\"shenyu-zk:2181\",\"threadpool\": \"shared\"}");
        assertThat(pluginResult, is("success"));
        // for sofa
        pluginResult = initPlugin(PluginEnum.SOFA.getName(), "{\"protocol\":\"zookeeper\",\"register\":\"shenyu-zk:2181\",\"threadpool\": \"shared\"}");
        assertThat(pluginResult, Matchers.is("success"));
    }
    
    @Test
    public void testApacheDubbo() throws IOException {
        DubboTest dubboTest = HttpHelper.INSTANCE.getFromGateway("/dubbo/findById?id=1", DubboTest.class);
        assertEquals("hello world shenyu Apache, findById", dubboTest.getName());
        assertEquals("1", dubboTest.getId());
    }
    
    @Test
    public void testGrpc() throws Exception {
        JsonArray jsonArray = new JsonArray();
        JsonObject child = new JsonObject();
        child.addProperty("message", "hello rpc");
        jsonArray.add(child);
        JsonObject request = new JsonObject();
        request.add("data", jsonArray);
        JsonArray response = HttpHelper.INSTANCE.postGateway("/grpc/echo", request, JsonArray.class);
        Map<String, Object> result = GsonUtils.getInstance().toObjectMap(response.get(0).toString(), Object.class);
        assertEquals("ReceivedHELLO", result.get("message"));
    }
    
    @Test
    public void testMotan() throws Exception {
        MotanDTO request = new MotanDTO("shenyu");
        Type returnType = new TypeToken<String>() {
        }.getType();
        String response = HttpHelper.INSTANCE.postGateway("/motan/demo/hello", request, returnType);
        assertEquals("hello shenyu", response);
    }
    
    @AfterAll
    public static void testIsOneThreadPool() throws IOException {
        String spring = HttpHelper.INSTANCE.getFromGateway("/shenyu/getFromSpring", String.class);
        String dubbo = HttpHelper.INSTANCE.getFromGateway("/shenyu/getFromDubbo", String.class);
        assertEquals(spring, dubbo);
        String grpc = HttpHelper.INSTANCE.getFromGateway("/shenyu/getFromGrpc", String.class);
        assertEquals(spring, grpc);
        String motan = HttpHelper.INSTANCE.getFromGateway("/shenyu/getFromMotan", String.class);
        assertEquals(spring, motan);
        // TODO test sofa
    }
}
