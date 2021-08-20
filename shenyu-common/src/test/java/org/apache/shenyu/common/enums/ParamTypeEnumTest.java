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

package org.apache.shenyu.common.enums;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.Test;

/** Test Cases for ParamTypeEnum. */
public final class ParamTypeEnumTest {

    @Test
    public void testGetName() {
        assertEquals("cookie", ParamTypeEnum.COOKIE.getName());
        assertEquals("req_method", ParamTypeEnum.REQUEST_METHOD.getName());
        assertEquals("header", ParamTypeEnum.HEADER.getName());
        assertEquals("ip", ParamTypeEnum.IP.getName());
        assertEquals("host", ParamTypeEnum.HOST.getName());
        assertEquals("query", ParamTypeEnum.QUERY.getName());
        assertEquals("uri", ParamTypeEnum.URI.getName());
        assertEquals("post", ParamTypeEnum.POST.getName());
        assertEquals("form-data", ParamTypeEnum.FORM_DATA.getName());
        assertEquals("path", ParamTypeEnum.PATH.getName());
    }

    @Test
    public void testSupport() {
        assertTrue(ParamTypeEnum.COOKIE.getSupport());
        assertTrue(ParamTypeEnum.REQUEST_METHOD.getSupport());
        assertTrue(ParamTypeEnum.HEADER.getSupport());
        assertTrue(ParamTypeEnum.IP.getSupport());
        assertTrue(ParamTypeEnum.HOST.getSupport());
        assertTrue(ParamTypeEnum.QUERY.getSupport());
        assertTrue(ParamTypeEnum.URI.getSupport());
        assertTrue(ParamTypeEnum.POST.getSupport());
        assertTrue(ParamTypeEnum.FORM_DATA.getSupport());
    }

    @Test
    public void testAcquireSupport() {
        List<ParamTypeEnum> supportParamTypeList =
                Arrays.stream(ParamTypeEnum.values())
                        .filter(ParamTypeEnum::getSupport)
                        .collect(Collectors.toList());
        assertEquals(ParamTypeEnum.acquireSupport(), supportParamTypeList);
    }

    @Test
    public void testGetParamTypeEnumByNameValid() {
        Arrays.stream(ParamTypeEnum.values())
                .filter(ParamTypeEnum::getSupport)
                .forEach(
                    paramTypeEnum ->
                            assertEquals(
                                    paramTypeEnum, ParamTypeEnum.getParamTypeEnumByName(paramTypeEnum.getName())));
    }

    @Test(expected = ShenyuException.class)
    public void testGetParamTypeEnumByNameInvalid() {
        ParamTypeEnum.getParamTypeEnumByName("InvalidName");
    }
}
