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

package org.apache.shenyu.admin.shiro.bean;

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpMethod;

import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link StatelessAuthFilter}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class StatelessAuthFilterTest {
    
    private static final String HEAD_TOKEN = "X-Access-Token";
    
    @InjectMocks
    private StatelessAuthFilter statelessAuthFilter;
    
    @Mock
    private HttpServletRequest httpServletRequest;
    
    @Mock
    private HttpServletResponse httpServletResponse;
    
    @Test
    public void testIsAccessAllowed() {
        Object obj = mock(Object.class);
        assertEquals(false, statelessAuthFilter.isAccessAllowed(httpServletRequest, httpServletResponse, obj));
    }
    
    @Test
    public void testOnAccessDenied() throws Exception {
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.OPTIONS.name());
        assertEquals(true, statelessAuthFilter.onAccessDenied(httpServletRequest, httpServletResponse));
    }
    
    @Test
    public void testUnionFailResponse() {
        PrintWriter printWriter = mock(PrintWriter.class);
        try {
            when(httpServletResponse.getWriter()).thenReturn(printWriter);
            doNothing().when(printWriter).println();
            Method testMethod = statelessAuthFilter.getClass().getDeclaredMethod("unionFailResponse", ServletResponse.class);
            testMethod.setAccessible(true);
            testMethod.invoke(statelessAuthFilter, httpServletResponse);
            verify(httpServletResponse).setContentType("application/json;charset=utf-8");
            verify(httpServletResponse).setCharacterEncoding("utf-8");
            verify(httpServletResponse).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }
    
    @Test
    public void testWrapCorsResponse() {
        try {
            Method testMethod = statelessAuthFilter.getClass().getDeclaredMethod("wrapCorsResponse", HttpServletResponse.class);
            testMethod.setAccessible(true);
            testMethod.invoke(statelessAuthFilter, httpServletResponse);
            verify(httpServletResponse).addHeader("Access-Control-Allow-Origin", "*");
            verify(httpServletResponse).addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
            verify(httpServletResponse).addHeader("Access-Control-Allow-Headers", "Content-Type");
            verify(httpServletResponse).addHeader("Access-Control-Max-Age", "1800");
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }
}
