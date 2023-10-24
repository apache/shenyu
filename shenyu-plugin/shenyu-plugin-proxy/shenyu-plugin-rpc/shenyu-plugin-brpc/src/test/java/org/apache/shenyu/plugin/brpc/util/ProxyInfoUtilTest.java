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

package org.apache.shenyu.plugin.brpc.util;

import org.apache.shenyu.common.dto.MetaData;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link ProxyInfoUtil}.
 */
@ExtendWith(MockitoExtension.class)
public class ProxyInfoUtilTest {

    @Test
    public void testGetParamClass() throws Exception {
        assertEquals(int.class, ProxyInfoUtil.getParamClass("int"));
        assertEquals(long.class, ProxyInfoUtil.getParamClass("long"));
        assertEquals(short.class, ProxyInfoUtil.getParamClass("short"));
        assertEquals(byte.class, ProxyInfoUtil.getParamClass("byte"));
        assertEquals(boolean.class, ProxyInfoUtil.getParamClass("boolean"));
        assertEquals(char.class, ProxyInfoUtil.getParamClass("char"));
        assertEquals(float.class, ProxyInfoUtil.getParamClass("float"));
        assertEquals(Integer.class, ProxyInfoUtil.getParamClass("java.lang.Integer"));
    }

    @Test
    public void testGetParamClassThrowsClassNotFoundException() throws Exception {
        assertThrows(ClassNotFoundException.class, () -> {
            ProxyInfoUtil.getParamClass("className");
        });
    }

    @Test
    public void testGetPrxName() {
        final MetaData metaData = new MetaData("id", "appName", "contextPath", "/path",
                "rpcType", "serviceName", "methodName", "parameterTypes",
                "rpcExt", false);
        final String result = ProxyInfoUtil.getProxyName(metaData);
        assertEquals("pathmethodNameProxy", result);
    }

    @Test
    public void testGetMethodName() {
        assertEquals("methodName", ProxyInfoUtil.getMethodName("methodName"));
    }

    @Test
    public void testGetObjectName() {
        final String result = ProxyInfoUtil.getObjectName("127.0.0.1:8005", "serviceName");
        assertEquals("serviceName@tcp -h 127.0.0.1 -p 8005", result);
    }

    @Test
    public void testGetParamArray() {
        assertArrayEquals(new Object[]{11, Double.valueOf("1.321321312"), Long.valueOf("131231312"), Short.valueOf("11"), Byte.valueOf("0"), false, 'a', 1.321321312F},
                ProxyInfoUtil.getParamArray(new Class<?>[]{int.class, double.class, long.class, short.class, byte.class, boolean.class, char.class, float.class},
                        new String[]{"int", "double", "long", "short", "byte", "boolean", "char", "float"},
                        "{\"int\":11,\"double\":1.321321312,\"long\":131231312,\"short\":11,\"byte\":0,\"boolean\":false,\"char\":'a',\"float\":1.321321312}"));
    }
}
