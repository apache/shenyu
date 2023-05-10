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

package org.apache.shenyu.admin.aspect;

import org.apache.shenyu.admin.aspect.controller.PrintLogControllerMethodAdviceImpl;
import org.apache.shenyu.admin.aspect.controller.RestControllerAspect;
import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ListUtil;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PrintLogControllerMethodAdviceImpl}.
 */
public class PrintApiLogAspectTest {
    
    private RestControllerAspect printApiLogAspect;
    
    private DashboardProperties dashboardProperties;
    
    private UserInfo visitor;
    
    @BeforeEach
    public void setUp() {
        dashboardProperties = mock(DashboardProperties.class);
        PrintLogControllerMethodAdviceImpl logControllerMethodAdvice = new PrintLogControllerMethodAdviceImpl(dashboardProperties);
        printApiLogAspect = new RestControllerAspect(ListUtil.of(logControllerMethodAdvice));
        visitor = UserInfo.builder()
                .userId("1")
                .userName("admin")
                .build();
    }
    
    @Test
    public void testAround() throws ShenyuException, NoSuchMethodException {
        addVisitor();
        final Method method = DataPermissionAspectTest.class.getMethod("nullMethod");
        ProceedingJoinPoint point = mock(ProceedingJoinPoint.class);
        final MethodSignature signature = mock(MethodSignature.class);
        when(point.getSignature()).thenReturn(signature);
        when(signature.getMethod()).thenReturn(method);
        assertDoesNotThrow(() -> printApiLogAspect.logAround(point));
        assertNotEquals(SessionUtil.visitor(), visitor);
        
        addVisitor();
        when(dashboardProperties.getEnablePrintApiLog()).thenReturn(true);
        when(point.getTarget()).thenReturn(mock(PrintApiLogAspectTest.class));
        when(point.getSignature()).thenReturn(signature);
        assertDoesNotThrow(() -> printApiLogAspect.logAround(point));
        assertNotEquals(SessionUtil.visitor(), visitor);
        try {
            addVisitor();
            doThrow(ShenyuException.class).when(point).proceed();
            assertThrows(ShenyuException.class, () -> printApiLogAspect.logAround(point));
            assertNotEquals(SessionUtil.visitor(), visitor);
        } catch (Throwable e) {
            throw new ShenyuException(e);
        }
    }
    
    @Test
    public void pointCutTest() {
        assertDoesNotThrow(() -> printApiLogAspect.controller());
    }
    
    private void addVisitor() {
        SessionUtil.setLocalVisitor(visitor);
    }
    
    /**
     * nullMethod.
     */
    public void nullMethod() {
    }
}
