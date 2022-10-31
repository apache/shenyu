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

package org.apache.shenyu.admin.model.result;

import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test case for {@link ShenyuAdminResult}.
 */
public final class ShenyuAdminResultTest {

    private ShenyuAdminResult shenyuAdminResultUnderTest;

    @BeforeEach
    public void setUp() {
        shenyuAdminResultUnderTest = new ShenyuAdminResult(0, "message", "data");
    }

    @Test
    public void testEquals() {
        assertEquals(shenyuAdminResultUnderTest.getCode().intValue(), 0);
        assertEquals(shenyuAdminResultUnderTest.getMessage(), "message");
    }

    @Test
    public void testHashCode() {
        shenyuAdminResultUnderTest = new ShenyuAdminResult();
        shenyuAdminResultUnderTest.setCode(0);
        shenyuAdminResultUnderTest.setMessage("message");
        shenyuAdminResultUnderTest.setData("data");
        final int result = shenyuAdminResultUnderTest.hashCode();
        assertEquals(-458988318, result);
    }

    @Test
    public void testToString() {
        final String result = shenyuAdminResultUnderTest.toString();
        assertEquals("ShenyuAdminResult{code=0, message='message', data=data}", result);
    }

    @Test
    public void testSuccess() {
        final ShenyuAdminResult result = ShenyuAdminResult.success();
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals("", result.getMessage());
        assertNull(result.getData());
        assertEquals(221991, result.hashCode());
        assertEquals("ShenyuAdminResult{code=200, message='', data=null}", result.toString());
    }

    @Test
    public void testSuccessWithMsg() {
        final ShenyuAdminResult result = ShenyuAdminResult.success("msg");
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(3582918, result.hashCode());
        assertEquals("ShenyuAdminResult{code=200, message='msg', data=null}", result.toString());
    }

    @Test
    public void testSuccessWithData() {
        final ShenyuAdminResult result = ShenyuAdminResult.success(Collections.singletonList("data"));
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertNull(result.getMessage());
        assertNotNull(result.getData());
        assertEquals("ShenyuAdminResult{code=200, message='null', data=[data]}", result.toString());
    }

    @Test
    public void testSuccessWithMsgAndData() {
        final ShenyuAdminResult result = ShenyuAdminResult.success("msg", "data");
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertEquals("data", result.getData());
        assertEquals(6658928, result.hashCode());
        assertEquals("ShenyuAdminResult{code=200, message='msg', data=data}", result.toString());
    }

    @Test
    public void testError() {
        final ShenyuAdminResult result = ShenyuAdminResult.error("msg");
        assertEquals(CommonErrorCode.ERROR, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(3871218, result.hashCode());
        assertEquals("ShenyuAdminResult{code=500, message='msg', data=null}", result.toString());
    }

    @Test
    public void testErrorWithMsg() {
        final ShenyuAdminResult result = ShenyuAdminResult.error(0, "msg");
        assertEquals(0, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(3390718, result.hashCode());
        assertEquals("ShenyuAdminResult{code=0, message='msg', data=null}", result.toString());
    }

    @Test
    public void testTimeout() {
        final ShenyuAdminResult result = ShenyuAdminResult.timeout("msg");
        assertEquals(HttpStatus.REQUEST_TIMEOUT.value(), result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(3782806, result.hashCode());
        assertEquals("ShenyuAdminResult{code=408, message='msg', data=null}", result.toString());
    }
}
