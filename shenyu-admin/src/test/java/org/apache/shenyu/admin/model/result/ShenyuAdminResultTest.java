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

import org.apache.shenyu.admin.spring.ShenyuMessageSourceAware;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.http.HttpStatus;

import java.util.Collections;
import java.util.Locale;

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
        shenyuAdminResultUnderTest = new ShenyuAdminResult(0, ShenyuResultMessage.SUCCESS, "data");
        ShenyuMessageSourceAware shenyuMessageSourceAware = new ShenyuMessageSourceAware();
        ResourceBundleMessageSource resourceBundleMessageSourcenew = new ResourceBundleMessageSource();
        resourceBundleMessageSourcenew.setBasename("i18n.messages");
        resourceBundleMessageSourcenew.setDefaultEncoding("UTF-8");
        shenyuMessageSourceAware.setMessageSource(resourceBundleMessageSourcenew);
        LocaleContextHolder.setLocale(Locale.CHINESE);
    }

    @Test
    public void testEquals() {
        assertEquals(shenyuAdminResultUnderTest.getCode().intValue(), 0);
        assertEquals(shenyuAdminResultUnderTest.getMessage(), ShenyuResultMessage.SUCCESS);
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
        assertEquals("ShenyuAdminResult{code=0, message='成功', data=data}", result);
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
        final ShenyuAdminResult result = ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS);
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals(ShenyuResultMessage.SUCCESS, result.getMessage());
        assertNull(result.getData());
        assertEquals(-1264839772, result.hashCode());
        assertEquals("ShenyuAdminResult{code=200, message='成功', data=null}", result.toString());
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
        final ShenyuAdminResult result = ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, "data");
        assertEquals(CommonErrorCode.SUCCESSFUL, result.getCode().intValue());
        assertEquals(ShenyuResultMessage.SUCCESS, result.getMessage());
        assertEquals("data", result.getData());
        assertEquals(-1261763762, result.hashCode());
        assertEquals("ShenyuAdminResult{code=200, message='成功', data=data}", result.toString());
    }

    @Test
    public void testError() {
        final ShenyuAdminResult result = ShenyuAdminResult.error(ShenyuResultMessage.SUCCESS);
        assertEquals(CommonErrorCode.ERROR, result.getCode().intValue());
        assertEquals(ShenyuResultMessage.SUCCESS, result.getMessage());
        assertNull(result.getData());
        assertEquals(-1264551472, result.hashCode());
        assertEquals("ShenyuAdminResult{code=500, message='成功', data=null}", result.toString());
    }

    @Test
    public void testErrorWithMsg() {
        final ShenyuAdminResult result = ShenyuAdminResult.error(0, ShenyuResultMessage.SUCCESS);
        assertEquals(0, result.getCode().intValue());
        assertEquals(ShenyuResultMessage.SUCCESS, result.getMessage());
        assertNull(result.getData());
        assertEquals(-1265031972, result.hashCode());
        assertEquals("ShenyuAdminResult{code=0, message='成功', data=null}", result.toString());
    }

    @Test
    public void testTimeout() {
        final ShenyuAdminResult result = ShenyuAdminResult.timeout(ShenyuResultMessage.SUCCESS);
        assertEquals(HttpStatus.REQUEST_TIMEOUT.value(), result.getCode().intValue());
        assertEquals(ShenyuResultMessage.SUCCESS, result.getMessage());
        assertNull(result.getData());
        assertEquals(-1264639884, result.hashCode());
        assertEquals("ShenyuAdminResult{code=408, message='成功', data=null}", result.toString());
    }
}
