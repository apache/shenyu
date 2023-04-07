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

package org.apache.shenyu.plugin.brpc.cache;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link ApplicationConfigCache}.
 */
public final class ApplicationConfigCacheTest {

    private ApplicationConfigCache applicationConfigCacheUnderTest;

    @BeforeEach
    public void setUp() {
        applicationConfigCacheUnderTest = ApplicationConfigCache.getInstance();
    }

    @Test
    public void testGetClassMethodKey() {
        assertEquals("className_methodName", ApplicationConfigCache.getClassMethodKey("className", "methodName"));
    }

    @Test
    public void testGetInstance() {
        assertNotNull(this.applicationConfigCacheUnderTest);
    }

    @Test
    public void testMethodInfo() {
        List<Pair<String, String>> params = new ArrayList<>();
        Pair<String, String> pair = Pair.of("left", "right");
        params.add(pair);
        ApplicationConfigCache.MethodInfo methodInfo = new ApplicationConfigCache.MethodInfo();
        methodInfo.setParamTypes(params);
        Assertions.assertEquals(methodInfo.getParamTypes().get(0).getLeft(), "left");
    }

    @Test
    public void testBrpcParamInfo() {
        ApplicationConfigCache.BrpcParamInfo paramInfo = new ApplicationConfigCache.BrpcParamInfo(null, null);
        paramInfo.setParamNames(new String[]{"test"});
        paramInfo.setParamTypes(new Class<?>[]{ApplicationConfigCache.class});
        Assertions.assertEquals(paramInfo.getParamNames()[0], "test");
        Assertions.assertEquals(paramInfo.getParamTypes()[0], ApplicationConfigCache.class);
    }

    @Test
    public void testBrpcParamExtInfo() {
        ApplicationConfigCache.BrpcParamExtInfo paramExtInfo = new ApplicationConfigCache.BrpcParamExtInfo();
        ApplicationConfigCache.MethodInfo methodInfo = new ApplicationConfigCache.MethodInfo();
        methodInfo.setMethodName("methodName");
        List<ApplicationConfigCache.MethodInfo> list = new ArrayList<>();
        list.add(methodInfo);
        paramExtInfo.setMethodInfo(list);
        Assertions.assertEquals(paramExtInfo.getMethodInfo().get(0).getMethodName(), "methodName");
    }
}
