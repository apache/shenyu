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

package org.apache.shenyu.admin.listener.http;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The TestCase for {@link HttpLongPollingDataChangedListener}.
 */
@ExtendWith(MockitoExtension.class)
public final class HttpLongPollingDataChangedListenerTest {

    private static final String X_REAL_IP = "X-Real-IP";

    private static final String X_FORWARDED_FOR = "X-Forwarded-For";

    private MockHttpServletRequest httpServletRequest;

    private MockHttpServletResponse httpServletResponse;

    @BeforeEach
    public void setUp() {
        this.httpServletRequest = new MockHttpServletRequest();
        this.httpServletResponse = new MockHttpServletResponse();
    }

    /**
     * test DoLongPolling Process.
     *
     * @throws UnsupportedEncodingException throw not support encoding
     */
    @Test
    public void testDoLongPolling() throws UnsupportedEncodingException {
        testCompareChangedGroup();
        testGetRemoteIp();
        testGenerateResponse();
    }

    /**
     *  test GenerateResponse.
     *
     * @throws UnsupportedEncodingException throw not support encoding
     */
    @Test
    public void testGenerateResponse() throws UnsupportedEncodingException {
        List<ConfigGroupEnum> changedGroups = new ArrayList<>();
        changedGroups.add(ConfigGroupEnum.PLUGIN);
        this.httpServletResponse.setHeader("Pragma", "no-cache");
        this.httpServletResponse.setDateHeader("Expires", 0);
        this.httpServletResponse.setHeader("Cache-Control", "no-cache,no-store");
        this.httpServletResponse.setContentType(MediaType.APPLICATION_JSON_VALUE);
        this.httpServletResponse.setStatus(MockHttpServletResponse.SC_OK);
        this.httpServletResponse.getWriter().println(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, changedGroups)));
    }

    /**
     * test getRemoteIp.
     */
    @Test
    public void testGetRemoteIp() {
        this.httpServletRequest.addHeader(X_FORWARDED_FOR, "x-forwarded-for,test");
        this.httpServletRequest.addHeader(X_REAL_IP, "127.0.0.1");
        String xForwardedFor = httpServletRequest.getHeader(X_FORWARDED_FOR);
        assertNotNull(xForwardedFor);
        assertTrue(StringUtils.isNotBlank(xForwardedFor));
        assertEquals("x-forwarded-for,test", xForwardedFor);
        assertEquals("127.0.0.1", httpServletRequest.getHeader(X_REAL_IP));
    }

    /**
     * test CompareChangedGroup.
     */
    @Test
    public void testCompareChangedGroup() {
        this.httpServletRequest.setParameter(ConfigGroupEnum.RULE.name(), "E10ADC3949BA59ABBE56E057F20F883E,1607068125");
        this.httpServletRequest.setParameter(ConfigGroupEnum.PLUGIN.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D63,1607068126");
        this.httpServletRequest.setParameter(ConfigGroupEnum.APP_AUTH.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D62,1607068124");
        this.httpServletRequest.setParameter(ConfigGroupEnum.SELECTOR.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D61,1607068123");
        this.httpServletRequest.setParameter(ConfigGroupEnum.META_DATA.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D60,1607068122");
        assertEquals("E10ADC3949BA59ABBE56E057F20F883E,1607068125", this.httpServletRequest.getParameter(ConfigGroupEnum.RULE.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D63,1607068126", this.httpServletRequest.getParameter(ConfigGroupEnum.PLUGIN.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D62,1607068124", this.httpServletRequest.getParameter(ConfigGroupEnum.APP_AUTH.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D61,1607068123", this.httpServletRequest.getParameter(ConfigGroupEnum.SELECTOR.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D60,1607068122", this.httpServletRequest.getParameter(ConfigGroupEnum.META_DATA.name()));
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            String[] params = Objects.requireNonNull(this.httpServletRequest.getParameter(group.name())).split(",");
            assertNotNull(params);
            assertEquals(2, params.length);
        }
    }
}
