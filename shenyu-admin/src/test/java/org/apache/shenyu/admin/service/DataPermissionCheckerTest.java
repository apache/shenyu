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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.service.impl.DataPermissionCheckerImpl;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shiro.authz.UnauthorizedException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Collections;

import static org.apache.shenyu.common.constant.AdminConstants.RULE_DATA_TYPE;
import static org.apache.shenyu.common.constant.AdminConstants.SELECTOR_DATA_TYPE;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link DataPermissionCheckerImpl}.
 */
public final class DataPermissionCheckerTest {

    private DataPermissionMapper dataPermissionMapper;

    private NamespaceUserService namespaceUserService;

    private DataPermissionChecker checker;

    @BeforeEach
    public void setUp() {
        dataPermissionMapper = mock(DataPermissionMapper.class);
        namespaceUserService = mock(NamespaceUserService.class);
        checker = new DataPermissionCheckerImpl(dataPermissionMapper, namespaceUserService);
    }

    @Test
    public void testAdministratorIsUnrestricted() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(true);
            assertDoesNotThrow(() -> checker.requireSelectorReadable("selector-1", "namespace-1"));
            assertDoesNotThrow(() -> checker.requireRuleReadable("rule-1", "namespace-1"));
            verifyNoInteractions(dataPermissionMapper, namespaceUserService);
        }
    }

    @Test
    public void testAnonymousCallerIsRejected() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("-1");
            assertThrows(UnauthorizedException.class, () -> checker.requireSelectorReadable("selector-1", "namespace-1"));
            verifyNoInteractions(dataPermissionMapper, namespaceUserService);
        }
    }

    @Test
    public void testResourceOutsideCallerNamespaceIsRejected() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("user-1");
            when(namespaceUserService.listNamespaceIdByUserId("user-1")).thenReturn(Collections.singletonList("namespace-2"));
            assertThrows(UnauthorizedException.class, () -> checker.requireSelectorReadable("selector-1", "namespace-1"));
            verify(dataPermissionMapper, never()).findOneByUniqueKey(anyString(), anyString(), anyInt());
        }
    }

    @Test
    public void testUngrantedResourceIsRejected() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("user-1");
            when(namespaceUserService.listNamespaceIdByUserId("user-1")).thenReturn(Collections.singletonList("namespace-1"));
            when(dataPermissionMapper.findOneByUniqueKey("selector-1", "user-1", SELECTOR_DATA_TYPE)).thenReturn(null);
            assertThrows(UnauthorizedException.class, () -> checker.requireSelectorReadable("selector-1", "namespace-1"));
        }
    }

    @Test
    public void testGrantedSelectorInNamespaceIsReadable() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("user-1");
            when(namespaceUserService.listNamespaceIdByUserId("user-1")).thenReturn(Collections.singletonList("namespace-1"));
            when(dataPermissionMapper.findOneByUniqueKey("selector-1", "user-1", SELECTOR_DATA_TYPE)).thenReturn(new DataPermissionDO());
            assertDoesNotThrow(() -> checker.requireSelectorReadable("selector-1", "namespace-1"));
        }
    }

    @Test
    public void testGrantedRuleInNamespaceIsReadable() {
        try (MockedStatic<SessionUtil> session = mockStatic(SessionUtil.class)) {
            session.when(SessionUtil::isAdmin).thenReturn(false);
            session.when(SessionUtil::visitorId).thenReturn("user-1");
            when(namespaceUserService.listNamespaceIdByUserId("user-1")).thenReturn(Collections.singletonList("namespace-1"));
            when(dataPermissionMapper.findOneByUniqueKey("rule-1", "user-1", RULE_DATA_TYPE)).thenReturn(new DataPermissionDO());
            assertDoesNotThrow(() -> checker.requireRuleReadable("rule-1", "namespace-1"));
        }
    }
}
