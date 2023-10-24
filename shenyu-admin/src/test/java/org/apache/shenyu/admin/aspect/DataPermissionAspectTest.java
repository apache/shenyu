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

import org.apache.shenyu.admin.aspect.annotation.DataPermission;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.query.FilterQuery;
import org.apache.shenyu.admin.service.DataPermissionService;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import static org.apache.shenyu.common.constant.AdminConstants.DATA_PERMISSION_RULE;
import static org.apache.shenyu.common.constant.AdminConstants.DATA_PERMISSION_SELECTOR;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link DataPermissionAspect}.
 */
public class DataPermissionAspectTest {

    private DataPermissionAspect dataPermissionAspect;

    private DataPermissionService dataPermissionService;

    @BeforeEach
    public void setUp() {
        dataPermissionService = mock(DataPermissionService.class);
        dataPermissionAspect = new DataPermissionAspect(dataPermissionService);
    }

    @Test
    public void testAround() {
        ProceedingJoinPoint point = mock(ProceedingJoinPoint.class);
        boolean thrown = false;
        try {
            dataPermissionAspect.around(point);
        } catch (ShenyuException e) {
            thrown = true;
        }
        assertTrue(thrown);
    }

    @Test
    public void aroundTest() throws NoSuchMethodException {
        ProceedingJoinPoint point = mock(ProceedingJoinPoint.class);
        final MethodSignature signature = mock(MethodSignature.class);
        when(point.getSignature()).thenReturn(signature);
        final Method method = DataPermissionAspectTest.class.getMethod("nullMethod");
        when(signature.getMethod()).thenReturn(method);
        assertDoesNotThrow(() -> dataPermissionAspect.around(point));
        when(point.getArgs()).thenReturn(null);
        assertDoesNotThrow(() -> dataPermissionAspect.around(point));
        try (MockedStatic<JwtUtils> jwtUtilsMockedStatic = mockStatic(JwtUtils.class)) {
            when(signature.getMethod()).thenReturn(DataPermissionAspectTest.class.getMethod("selectorMethod"));
            final Object[] objects = new Object[2];
            objects[0] = new FilterQuery();
            when(point.getArgs()).thenReturn(objects);
            jwtUtilsMockedStatic.when(JwtUtils::getUserInfo).thenReturn(new UserInfo());
            when(dataPermissionService.getDataPermission(any())).thenReturn(new ArrayList<>());
            assertDoesNotThrow(() -> dataPermissionAspect.around(point));
            List<String> stringList = new ArrayList<>();
            stringList.add("permission");
            when(dataPermissionService.getDataPermission(any())).thenReturn(stringList);
            assertDoesNotThrow(() -> dataPermissionAspect.around(point));
            when(signature.getMethod()).thenReturn(DataPermissionAspectTest.class.getMethod("ruleMethod"));
            assertDoesNotThrow(() -> dataPermissionAspect.around(point));
            when(signature.getMethod()).thenReturn(DataPermissionAspectTest.class.getMethod("otherMethod"));
            assertDoesNotThrow(() -> dataPermissionAspect.around(point));
        }
    }

    @Test
    public void dataPermissionCutTest() {
        assertDoesNotThrow(() -> dataPermissionAspect.dataPermissionCut());
    }

    /**
     * selectorMethod.
     */
    @DataPermission(dataType = DATA_PERMISSION_SELECTOR)
    public void selectorMethod() {
    }

    /**
     * nullMethod.
     */
    public void nullMethod() {
    }

    /**
     * ruleMethod.
     */
    @DataPermission(dataType = DATA_PERMISSION_RULE)
    public void ruleMethod() {
    }

    /**
    * otherMethod.
    */
    @DataPermission(dataType = "other")
    public void otherMethod() {
    }
}
