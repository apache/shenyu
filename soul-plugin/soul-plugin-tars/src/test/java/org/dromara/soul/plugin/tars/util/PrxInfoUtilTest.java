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

package org.dromara.soul.plugin.tars.util;

import org.dromara.soul.common.dto.MetaData;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Test case for {@link PrxInfoUtil}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
public class PrxInfoUtilTest {

    @Test
    public void testGetParamClass() throws Exception {
        assertEquals(int.class, PrxInfoUtil.getParamClass("int"));
        assertEquals(long.class, PrxInfoUtil.getParamClass("long"));
        assertEquals(short.class, PrxInfoUtil.getParamClass("short"));
        assertEquals(byte.class, PrxInfoUtil.getParamClass("byte"));
        assertEquals(boolean.class, PrxInfoUtil.getParamClass("boolean"));
        assertEquals(char.class, PrxInfoUtil.getParamClass("char"));
        assertEquals(float.class, PrxInfoUtil.getParamClass("float"));
        assertEquals(Integer.class, PrxInfoUtil.getParamClass("java.lang.Integer"));
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @Test(expected = ClassNotFoundException.class)
    public void testGetParamClassThrowsClassNotFoundException() throws Exception {
        PrxInfoUtil.getParamClass("className");
    }

    @Test
    public void testGetPrxName() {
        final MetaData metaData = new MetaData("id", "appName", "contextPath", "/path",
                "rpcType", "serviceName", "methodName", "parameterTypes",
                "rpcExt", false);
        final String result = PrxInfoUtil.getPrxName(metaData);
        assertEquals("pathmethodNamePrx", result);
    }

    @Test
    public void testGetMethodName() {
        assertEquals("promise_methodName", PrxInfoUtil.getMethodName("methodName"));
    }

    @Test
    public void testGetObjectName() {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path", "rpcType", "serviceName", "methodName",
                "parameterTypes", "rpcExt", false);
        final String result = PrxInfoUtil.getObjectName(metaData);
        assertEquals("serviceName@tcp -h 127.0.0.1 -p 8080", result);
    }

    @Test
    public void testGetParamArray() {
        assertArrayEquals(new Object[]{Integer.valueOf(1), "1"},
                PrxInfoUtil.getParamArray(new Class<?>[]{int.class, Integer.class}, new String[]{"param1", "param2"},
                        "{\"param1\":\"1\",\"param2\":\"1\"}"));
    }
}
