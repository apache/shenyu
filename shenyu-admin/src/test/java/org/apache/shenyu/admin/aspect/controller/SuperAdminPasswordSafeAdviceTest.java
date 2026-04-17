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

package org.apache.shenyu.admin.aspect.controller;

import com.google.common.base.Stopwatch;
import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.annotation.AnnotatedElementUtils;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link SuperAdminPasswordSafeAdvice}.
 */
@ExtendWith(MockitoExtension.class)
public class SuperAdminPasswordSafeAdviceTest {

    @Mock
    private DashboardProperties properties;

    @Mock
    private DashboardUserService userService;

    @InjectMocks
    private SuperAdminPasswordSafeAdvice advice;

    private Object bean;

    private Method method;

    private Stopwatch stopwatch;

    @BeforeEach
    void setUp() throws Exception {
        bean = new Object();
        method = Object.class.getMethod("toString");
        stopwatch = Stopwatch.createUnstarted();
    }

    @Test
    void testDoPreProcessWhenEnableOnlySuperAdminPermissionIsFalseShouldSkip() {
        when(properties.getEnableOnlySuperAdminPermission()).thenReturn(false);

        advice.doPreProcess(bean, method, stopwatch);

        verify(properties, never()).getEnableSuperAdminPasswordSafe();
        verify(userService, never()).checkUserPassword(anyString());
    }

    @Test
    void testDoPreProcessWhenEnableSuperAdminPasswordSafeIsFalseShouldSkip() {
        when(properties.getEnableOnlySuperAdminPermission()).thenReturn(true);
        when(properties.getEnableSuperAdminPasswordSafe()).thenReturn(false);

        advice.doPreProcess(bean, method, stopwatch);

        verify(userService, never()).checkUserPassword(anyString());
    }

    @Test
    void testDoPreProcessWhenNotAdminShouldSkip() {
        when(properties.getEnableOnlySuperAdminPermission()).thenReturn(true);
        when(properties.getEnableSuperAdminPasswordSafe()).thenReturn(true);

        try (MockedStatic<SessionUtil> sessionUtilMock = mockStatic(SessionUtil.class)) {
            sessionUtilMock.when(SessionUtil::isAdmin).thenReturn(false);

            advice.doPreProcess(bean, method, stopwatch);

            verify(userService, never()).checkUserPassword(anyString());
        }
    }

    @Test
    void testDoPreProcessWhenNoRequiresPermissionsShouldSkip() {
        when(properties.getEnableOnlySuperAdminPermission()).thenReturn(true);
        when(properties.getEnableSuperAdminPasswordSafe()).thenReturn(true);

        try (MockedStatic<SessionUtil> sessionUtilMock = mockStatic(SessionUtil.class)) {
            sessionUtilMock.when(SessionUtil::isAdmin).thenReturn(true);

            advice.doPreProcess(bean, method, stopwatch);

            verify(userService, never()).checkUserPassword(anyString());
        }
    }

    @Test
    void testDoPreProcessWhenPermissionNotInListShouldSkip() throws Exception {
        when(properties.getEnableOnlySuperAdminPermission()).thenReturn(true);
        when(properties.getEnableSuperAdminPasswordSafe()).thenReturn(true);

        Method methodWithAnnotation = TestController.class.getMethod("testMethod");
        UserInfo user = mock(UserInfo.class);

        try (MockedStatic<SessionUtil> sessionUtilMock = mockStatic(SessionUtil.class);
             MockedStatic<AnnotatedElementUtils> annotatedElementUtilsMock = mockStatic(AnnotatedElementUtils.class)) {
            sessionUtilMock.when(SessionUtil::isAdmin).thenReturn(true);
            sessionUtilMock.when(SessionUtil::visitor).thenReturn(user);
            annotatedElementUtilsMock.when(() -> AnnotatedElementUtils.findMergedAnnotation(methodWithAnnotation, org.apache.shiro.authz.annotation.RequiresPermissions.class))
                    .thenReturn(mock(org.apache.shiro.authz.annotation.RequiresPermissions.class));

            advice.doPreProcess(bean, methodWithAnnotation, stopwatch);

            verify(userService, never()).checkUserPassword(anyString());
        }
    }

    @Test
    void testDoPreProcessWhenPermissionInListShouldCheckPassword() throws Exception {
        when(properties.getEnableOnlySuperAdminPermission()).thenReturn(true);
        when(properties.getEnableSuperAdminPasswordSafe()).thenReturn(true);
        List<String> permissions = Arrays.asList("admin:permission", "other:permission");
        when(properties.getOnlySuperAdminPermission()).thenReturn(permissions);

        Method methodWithAnnotation = TestController.class.getMethod("testMethod");
        org.apache.shiro.authz.annotation.RequiresPermissions requiresPermissions = mock(org.apache.shiro.authz.annotation.RequiresPermissions.class);
        when(requiresPermissions.value()).thenReturn(new String[]{"admin:permission"});
        UserInfo user = mock(UserInfo.class);
        when(user.getUserId()).thenReturn("userId");

        try (MockedStatic<SessionUtil> sessionUtilMock = mockStatic(SessionUtil.class);
             MockedStatic<AnnotatedElementUtils> annotatedElementUtilsMock = mockStatic(AnnotatedElementUtils.class)) {
            sessionUtilMock.when(SessionUtil::isAdmin).thenReturn(true);
            sessionUtilMock.when(SessionUtil::visitor).thenReturn(user);
            annotatedElementUtilsMock.when(() -> AnnotatedElementUtils.findMergedAnnotation(methodWithAnnotation, org.apache.shiro.authz.annotation.RequiresPermissions.class))
                    .thenReturn(requiresPermissions);

            advice.doPreProcess(bean, methodWithAnnotation, stopwatch);

            verify(userService).checkUserPassword("userId");
        }
    }

    private static class TestController {
        @org.apache.shiro.authz.annotation.RequiresPermissions("admin:permission")
        public void testMethod() {
            throw new UnsupportedOperationException("testMethod() used for unit test.");
        }
    }
}
