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

package org.apache.shenyu.integrated.test.grpc;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integrated.test.grpc.dto.GrpcResponse;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class GrpcPluginTest extends AbstractPluginDataInit {
    
    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.GRPC.getName(), "");
        assertThat(pluginResult, is("success"));
    }
    
    @Test
    public void testHelloWorld() throws Exception {
        JsonObject request = buildGrpcRequest();
        GrpcResponse response = HttpHelper.INSTANCE.postGateway("/grpc/echo", request, GrpcResponse.class);
        assertEquals("200", response.getCode());
    }

    private JsonObject buildGrpcRequest() {
        JsonArray jsonArray = new JsonArray();
        JsonObject child = new JsonObject();
        child.addProperty("message", "hello rpc");
        jsonArray.add(child);
        JsonObject jsonObject = new JsonObject();
        jsonObject.add("data", jsonArray);
        return jsonObject;
    }
}
