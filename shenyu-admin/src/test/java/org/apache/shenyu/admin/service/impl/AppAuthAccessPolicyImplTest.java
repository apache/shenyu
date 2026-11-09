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

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.config.AppAuthLifecycleProperties;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
        SecurityAuditLog.resetRateLimiter();
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
    void nullSubjectIsRejected() {
        loginAs(SVC_USER);
        ThreadContext.unbindSubject();
        // a mock SecurityManager left bound in the JVM (as PermissionServiceTest does) makes
        // SecurityUtils.getSubject() build a subject via the mock and receive null back
        ThreadContext.bind(mock(org.apache.shiro.mgt.SecurityManager.class));
        try {
            assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
        } finally {
            ThreadContext.unbindSecurityManager();
        }
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

    @Test
    void serviceIdentityDenialsEmitFixedReasonAudit() {
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            SessionUtil.setLocalVisitor(null);
            assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
            loginAs("20002");
            permit("system:authen:lifecycleApply", true);
            assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleApply"));
            loginAs(SVC_USER);
            permit("system:authen:lifecycleResult", false);
            assertThrows(ShenyuAdminException.class, () -> policy.requireServiceCaller("system:authen:lifecycleResult"));
        });
        assertEquals(3, events.size());
        String first = events.get(0).getFormattedMessage();
        assertTrue(first.contains("eventType=app_auth_access"), first);
        assertTrue(first.contains("outcome=denied"), first);
        assertTrue(first.contains("reason=service_identity_unknown"), first);
        assertTrue(first.contains("actor=unknown"), first);
        String second = events.get(1).getFormattedMessage();
        assertTrue(second.contains("reason=service_identity_not_whitelisted"), second);
        assertTrue(second.contains("actor=name-20002"), second);
        assertTrue(second.contains("resource=system:authen:lifecycleApply"), second);
        String third = events.get(2).getFormattedMessage();
        assertTrue(third.contains("reason=service_identity_no_permission"), third);
        assertTrue(third.contains("resource=system:authen:lifecycleResult"), third);
    }

    @Test
    void namespaceDenialEmitsAuditWithContext() {
        when(namespaceUserService.listNamespaceIdByUserId(SVC_USER))
                .thenReturn(Collections.singletonList("ns-owned"));
        loginAs(SVC_USER);
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertThrows(ShenyuAdminException.class, () -> policy.requireNamespace(SVC_USER, "ns-other")));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("reason=namespace_ownership_denied"), message);
        assertTrue(message.contains("namespace=ns-other"), message);
        assertTrue(message.contains("actor=name-" + SVC_USER), message);
    }

    @Test
    void appAuthOwnershipDenialEmitsAuditWithContext() {
        loginAs(SVC_USER);
        when(requestMapper.selectByAuthId("auth-1")).thenReturn(null);
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertThrows(ShenyuAdminException.class, () -> policy.requireOwnedAuth(SVC_USER, "ns-1", "auth-1")));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("reason=app_auth_ownership_denied"), message);
        assertTrue(message.contains("namespace=ns-1"), message);
        assertTrue(message.contains("resource=auth-1"), message);
    }

    @Test
    void passingChecksEmitNoAudit() {
        loginAs(SVC_USER);
        permit("system:authen:lifecycleApply", true);
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertEquals(SVC_USER, policy.requireServiceCaller("system:authen:lifecycleApply")));
        assertTrue(events.isEmpty(), "passing checks must not emit denial audit events");
    }

    @Test
    void denialIsNotAuditedAgainByGlobalExceptionHandler() throws Exception {
        loginAs(SVC_USER);
        when(requestMapper.selectByAuthId("auth-1")).thenReturn(null);
        final ShenyuAdminException thrown = assertThrows(ShenyuAdminException.class,
                () -> policy.requireOwnedAuth(SVC_USER, "ns-1", "auth-1"));
        assertEquals("App auth ownership check failed", thrown.getMessage());
        Method handler = ExceptionHandlers.class.getDeclaredMethod("handleShenyuException", org.apache.shenyu.common.exception.ShenyuException.class);
        handler.setAccessible(true);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            try {
                handler.invoke(new ExceptionHandlers(null), thrown);
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        });
        assertTrue(events.isEmpty(), "the global handler must not duplicate the business denial audit event");
    }

    /**
     * capture the logback events emitted on the security audit logger while running the given action.
     *
     * @param runnable action to run
     * @return captured logging events
     */
    private List<ILoggingEvent> captureAuditLogs(final Runnable runnable) {
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(SecurityAuditLog.LOGGER_NAME);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        logger.addAppender(appender);
        try {
            runnable.run();
        } finally {
            logger.detachAppender(appender);
        }
        return appender.list;
    }
}
