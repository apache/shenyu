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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.config.AppAuthLifecycleProperties;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verify the full deny matrix of AppAuthAccessPolicyImpl: default user, non-whitelisted, missing permission,
 * no Shiro context, no namespace relation, foreign caller/namespace, REVOKED and missing app_auth; only whitelist plus permission passes.
 */
class AppAuthAccessPolicyImplTest {

    private static final String SVC_USER = "10001";

    private AppAuthLifecycleProperties properties;

    private NamespaceUserService namespaceUserService;

    private AppAuthRequestMapper requestMapper;

    private AppAuthMapper appAuthMapper;

    private AppAuthAccessPolicyImpl policy;

    private Subject subject;

    @BeforeEach
    void setUp() {
        properties = new AppAuthLifecycleProperties();
        properties.setServiceUserIds(Collections.singletonList(SVC_USER));
        namespaceUserService = mock(NamespaceUserService.class);
        requestMapper = mock(AppAuthRequestMapper.class);
        appAuthMapper = mock(AppAuthMapper.class);
        policy = new AppAuthAccessPolicyImpl(properties, namespaceUserService, requestMapper, appAuthMapper);

        subject = mock(Subject.class);
        ThreadContext.bind(subject);
    }

    @AfterEach
    void tearDown() {
        ThreadContext.unbindSubject();
        SessionUtil.setLocalVisitor(null);
    }

    private void loginAs(final String userId) {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId(userId).userName("name-" + userId).build());
    }

    private void permit(final String permission, final boolean allowed) {
        when(subject.isPermitted(permission)).thenReturn(allowed);
    }

    @Test
    void defaultVisitorIsRejected() {
        SessionUtil.setLocalVisitor(null);
        assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
    }

    @Test
    void nonWhitelistedUserIsRejectedEvenWithPermission() {
        loginAs("20002");
        permit("system:authen:lifecycleApply", true);
        assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
    }

    @Test
    void missingActionPermissionIsRejected() {
        loginAs(SVC_USER);
        // plain permissions like system:authen:list must not pass the lifecycle action check
        permit("system:authen:lifecycleResult", false);
        assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleResult"));
    }

    @Test
    void missingShiroContextIsRejected() {
        loginAs(SVC_USER);
        ThreadContext.unbindSubject();
        assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
    }

    @Test
    void whitelistedPermittedCallerPasses() {
        loginAs(SVC_USER);
        permit("system:authen:lifecycleApply", true);
        assertEquals(SVC_USER, policy.requireServiceCaller("system:authen:lifecycleApply"));
    }

    @Test
    void emptyWhitelistRejectsEveryone() {
        properties.setServiceUserIds(Collections.emptyList());
        loginAs(SVC_USER);
        permit("system:authen:lifecycleApply", true);
        assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
    }

    @Test
    void namespaceRelationIsRequired() {
        when(namespaceUserService.listNamespaceIdByUserId(SVC_USER))
                .thenReturn(Collections.singletonList("ns-owned"));
        policy.requireNamespace(SVC_USER, "ns-owned");
        assertThrows(ShenyuAdminException.class, () -> policy.requireNamespace(SVC_USER, "ns-other"));
    }

    @Test
    void ownedAuthRequiresMatchedRecordStateAndAppAuth() {
        final AppAuthRequestDO record = new AppAuthRequestDO();
        record.setCallerKey(SVC_USER);
        record.setNamespaceId("ns-1");
        record.setState("CREATED");
        when(requestMapper.selectByAuthId("auth-1")).thenReturn(record);
        final AppAuthDO auth = new AppAuthDO();
        auth.setId("auth-1");
        auth.setNamespaceId("ns-1");
        when(appAuthMapper.selectById("auth-1")).thenReturn(auth);

        assertEquals(record, policy.requireOwnedAuth(SVC_USER, "ns-1", "auth-1"));

        // foreign caller / foreign namespace / REVOKED / missing record / missing app_auth -> all rejected
        assertThrows(ShenyuAdminException.class, () -> policy.requireOwnedAuth("other-svc", "ns-1", "auth-1"));
        assertThrows(ShenyuAdminException.class, () -> policy.requireOwnedAuth(SVC_USER, "ns-2", "auth-1"));
        record.setState("REVOKED");
        assertThrows(ShenyuAdminException.class, () -> policy.requireOwnedAuth(SVC_USER, "ns-1", "auth-1"));
        record.setState("CREATED");
        when(appAuthMapper.selectById("auth-1")).thenReturn(null);
        assertThrows(ShenyuAdminException.class, () -> policy.requireOwnedAuth(SVC_USER, "ns-1", "auth-1"));
        when(requestMapper.selectByAuthId("auth-1")).thenReturn(null);
        assertThrows(ShenyuAdminException.class, () -> policy.requireOwnedAuth(SVC_USER, "ns-1", "auth-1"));
    }
}
