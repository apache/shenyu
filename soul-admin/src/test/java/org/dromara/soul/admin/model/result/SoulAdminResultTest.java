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

package org.dromara.soul.admin.model.result;

import org.dromara.soul.common.exception.CommonErrorCode;
import org.junit.Before;
import org.junit.Test;
import org.springframework.http.HttpStatus;

import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Test case for {@link SoulAdminResult}.
 *
 * @author HoldDie
 */
public final class SoulAdminResultTest {

    private SoulAdminResult soulAdminResultUnderTest;

    @Before
    public void setUp() {
        soulAdminResultUnderTest = new SoulAdminResult(0, "message", "data");
    }

    @Test
    public void testEquals() {
        assertEquals(soulAdminResultUnderTest.getCode().intValue(), 0);
        assertEquals(soulAdminResultUnderTest.getMessage(), "message");
    }

    @Test
    public void testHashCode() {
        soulAdminResultUnderTest = new SoulAdminResult();
        soulAdminResultUnderTest.setCode(0);
        soulAdminResultUnderTest.setMessage("message");
        soulAdminResultUnderTest.setData("data");
        final int result = soulAdminResultUnderTest.hashCode();
        assertEquals(509285258, result);
    }

    @Test
    public void testToString() {
        final String result = soulAdminResultUnderTest.toString();
        assertEquals("SoulAdminResult(code=0, message=message, data=data)", result);
    }

    @Test
    public void testSuccess() {
        final SoulAdminResult result = SoulAdminResult.success();
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals("", result.getMessage());
        assertNull(result.getData());
        assertEquals(901622, result.hashCode());
        assertEquals("SoulAdminResult(code=200, message=, data=null)", result.toString());
    }

    @Test
    public void testSuccessWithMsg() {
        final SoulAdminResult result = SoulAdminResult.success("msg");
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(7298225, result.hashCode());
        assertEquals("SoulAdminResult(code=200, message=msg, data=null)", result.toString());
    }

    @Test
    public void testSuccessWithData() {
        final SoulAdminResult result = SoulAdminResult.success(Collections.singletonList("data"));
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertNull(result.getMessage());
        assertNotNull(result.getData());
        assertEquals("SoulAdminResult(code=200, message=null, data=[data])", result.toString());
    }

    @Test
    public void testSuccessWithMsgAndData() {
        final SoulAdminResult result = SoulAdminResult.success("msg", "data");
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertEquals("data", result.getData());
        assertEquals(10374192, result.hashCode());
        assertEquals("SoulAdminResult(code=200, message=msg, data=data)", result.toString());
    }

    @Test
    public void testError() {
        final SoulAdminResult result = SoulAdminResult.error("msg");
        assertEquals(CommonErrorCode.ERROR, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(8342525, result.hashCode());
        assertEquals("SoulAdminResult(code=500, message=msg, data=null)", result.toString());
    }

    @Test
    public void testErrorWithMsg() {
        final SoulAdminResult result = SoulAdminResult.error(0, "msg");
        assertEquals(0, result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(6602025, result.hashCode());
        assertEquals("SoulAdminResult(code=0, message=msg, data=null)", result.toString());
    }

    @Test
    public void testTimeout() {
        final SoulAdminResult result = SoulAdminResult.timeout("msg");
        assertEquals(HttpStatus.REQUEST_TIMEOUT.value(), result.getCode().intValue());
        assertEquals("msg", result.getMessage());
        assertNull(result.getData());
        assertEquals(8022273, result.hashCode());
        assertEquals("SoulAdminResult(code=408, message=msg, data=null)", result.toString());
    }
}
