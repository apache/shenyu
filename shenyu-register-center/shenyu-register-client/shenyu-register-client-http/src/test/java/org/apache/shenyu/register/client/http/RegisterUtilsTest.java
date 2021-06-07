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

package org.apache.shenyu.register.client.http;

import com.google.gson.Gson;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import lombok.SneakyThrows;
import org.apache.shenyu.register.client.http.utils.OkHttpTools;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.enums.RegisterTypeEnum;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link RegisterUtils}.
 */
public final class RegisterUtilsTest {
    
    private Gson gson = new Gson();
    
    private OkHttpTools okHttpTools;
    
    private String json;
    
    private String url;
    
    @Before
    public void setUp() {
        okHttpTools = mock(OkHttpTools.class);
        Map<String, Object> jsonMap = new HashMap<String, Object>() {
            {
                put("appName", "dubbo");
                put("contextPath", "/dubbo");
                put("path", "/dubbo/findByArrayIdsAndName");
                put("pathDesc", "");
                put("serviceName", "org.apache.shenyu.examples.dubbo.api.service.DubboMultiParamService");
                put("ruleName", "/dubbo/findByArrayIdsAndName");
                put("parameterTypes", "[Ljava.lang.Integer;,java.lang.String");
                put("rpcExt", "{\"group\":\"\",\"version\":\"\",\"loadbalance\":\"random\",\"retries\":2,\"timeout\":10000,\"url\":\"\"}");
                put("enabled", true);
            }
        };
        
        json = gson.toJson(jsonMap);
        url = "http://localhost:9095/shenyu-client/dubbo-register";
    }
    
    @SneakyThrows
    @Test
    public void testDoRegisterWhenSuccess() {
        when(okHttpTools.post(url, json)).thenReturn("SUCCESS");
        
        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName());
            verify(okHttpTools, times(1)).post(eq(url), eq(json));
        }
    }
    
    @SneakyThrows
    @Test
    public void testDoRegisterWhenError() {
        when(okHttpTools.post(url, json)).thenReturn("Error parameter！");
        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName());
            verify(okHttpTools, times(1)).post(eq(url), eq(json));
        }
    }
    
    @SneakyThrows
    @Test(expected = IOException.class)
    public void testDoRegisterWhenThrowException() {
        when(okHttpTools.post(url, json)).thenThrow(IOException.class);
        try (MockedStatic<OkHttpTools> okHttpToolsMockedStatic = mockStatic(OkHttpTools.class)) {
            okHttpToolsMockedStatic.when(OkHttpTools::getInstance).thenReturn(okHttpTools);
            RegisterUtils.doRegister(json, url, RegisterTypeEnum.DUBBO.getName());
            verify(okHttpTools, times(1)).post(eq(url), eq(json));
        }
    }
}
