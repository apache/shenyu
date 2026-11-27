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

package org.apache.shenyu.admin.utils;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.RecordLogDataChangedAdapterListener;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.OperationRecordLogMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.event.user.UserUpdatedEvent;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.service.impl.DataPermissionCheckerImpl;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.slf4j.LoggerFactory;
import org.springframework.context.MessageSource;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.servlet.HandlerMapping;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Existing permission decisions and operation records share body-free request correlation.
 */
class AdminSecurityAuditIntegrationTest {

    private final Logger logger = (Logger) LoggerFactory.getLogger("org.apache.shenyu.admin.security.audit");

    private final ListAppender<ILoggingEvent> events = new ListAppender<>();

    private MockHttpServletRequest request;

    @BeforeEach
    void setup() {
        events.start();
        logger.addAppender(events);
        request = new MockHttpServletRequest("GET", "/rules/secret-path");
        request.setAttribute(HandlerMapping.BEST_MATCHING_PATTERN_ATTRIBUTE, "/rules/{id}");
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
        Subject subject = mock(Subject.class);
        when(subject.isAuthenticated()).thenReturn(true);
        UserInfo user = UserInfo.builder().userId("trusted-user-id").userName("editor").build();
        when(subject.getPrincipal()).thenReturn(user);
        ThreadContext.bind(subject);
        SessionUtil.setLocalVisitor(user);
    }

    @AfterEach
    void cleanup() {
        RequestContextHolder.resetRequestAttributes();
        ThreadContext.unbindSubject();
        SessionUtil.clean();
        logger.detachAppender(events);
        events.stop();
    }

    @Test
    void namespaceDenialCarriesKnownIdentityAndIsNotDuplicatedByExceptionHandler() {
        NamespaceUserService namespaces = mock(NamespaceUserService.class);
        when(namespaces.listNamespaceIdByUserId("trusted-user-id")).thenReturn(Collections.emptyList());
        DataPermissionCheckerImpl checker = new DataPermissionCheckerImpl(mock(DataPermissionMapper.class), namespaces);
        UnauthorizedException denial = assertThrows(UnauthorizedException.class, () -> checker.requireRuleReadable("rule-1", "ns-1"));
        ReflectionTestUtils.invokeMethod(new ExceptionHandlers(mock(MessageSource.class)), "handleUnauthorizedException", denial);
        assertEquals(1, events.list.size());
        JsonObject event = JsonParser.parseString(events.list.get(0).getFormattedMessage()).getAsJsonObject();
        assertEquals("trusted-user-id", event.get("actor_id").getAsString());
        assertEquals("NAMESPACE_DENIED", event.get("reason").getAsString());
        assertEquals("ns-1", event.get("namespace_id").getAsString());
        assertEquals("rule-1", event.get("resource_id").getAsString());
    }

    @Test
    void generalActionDenialUsesRouteTemplateWithoutExceptionOrRawPath() {
        UnauthorizedException denial = new UnauthorizedException("secret-token-in-exception");
        ReflectionTestUtils.invokeMethod(new ExceptionHandlers(mock(MessageSource.class)), "handleUnauthorizedException", denial);
        assertEquals(1, events.list.size());
        String json = events.list.get(0).getFormattedMessage();
        assertFalse(json.contains("secret"));
        assertEquals("/rules/{id}", JsonParser.parseString(json).getAsJsonObject().get("action").getAsString());
    }

    @Test
    void typedServiceDenialDoesNotFallThroughToUnboundedExceptionLogging() {
        Logger exceptionLogger = (Logger) LoggerFactory.getLogger(ExceptionHandlers.class);
        exceptionLogger.addAppender(events);
        try {
            ShenyuAdminException denial = AdminSecurityAudit.denied(new ShenyuAdminException("secret diagnostic"),
                    AdminSecurityAudit.Reason.SERVICE_IDENTITY_DENIED, "apply", null, null);
            ExceptionHandlers handlers = new ExceptionHandlers(mock(MessageSource.class));
            ReflectionTestUtils.invokeMethod(handlers, "handleShenyuException", denial);
            ReflectionTestUtils.invokeMethod(handlers, "handleExceptionHandler", denial);
            assertEquals(1, events.list.size());
            assertFalse(events.list.get(0).getFormattedMessage().contains("secret"));
            ReflectionTestUtils.invokeMethod(handlers, "handleExceptionHandler", new IllegalStateException("independent failure"));
            assertEquals(2, events.list.size());
            assertEquals("independent failure", events.list.get(1).getFormattedMessage());
        } finally {
            exceptionLogger.detachAppender(events);
        }
    }

    @Test
    void resourceDenialIsDistinctFromNamespaceDenial() {
        NamespaceUserService namespaces = mock(NamespaceUserService.class);
        when(namespaces.listNamespaceIdByUserId("trusted-user-id")).thenReturn(Collections.singletonList("ns-1"));
        DataPermissionCheckerImpl checker = new DataPermissionCheckerImpl(mock(DataPermissionMapper.class), namespaces);
        assertThrows(UnauthorizedException.class, () -> checker.requireRuleReadable("rule-1", "ns-1"));
        assertEquals("RESOURCE_DENIED", JsonParser.parseString(events.list.get(0).getFormattedMessage()).getAsJsonObject().get("reason").getAsString());
    }

    @Test
    void existingOperationRecordRetainsCorrelationAfterRequestContextIsReleased() {
        final String id = AdminSecurityAudit.requestId(request);
        DashboardUserDO before = DashboardUserDO.builder().id("target-id").userName("target").tokenVersion(0L).build();
        DashboardUserDO after = DashboardUserDO.builder().id("target-id").userName("target").tokenVersion(1L).build();
        UserUpdatedEvent event = new UserUpdatedEvent(after, before, "editor");
        RequestContextHolder.resetRequestAttributes();
        OperationRecordLogMapper mapper = mock(OperationRecordLogMapper.class);
        new RecordLogDataChangedAdapterListener(mapper).onApplicationEvent(event);
        ArgumentCaptor<OperationRecordLog> record = ArgumentCaptor.forClass(OperationRecordLog.class);
        verify(mapper).insert(record.capture());
        assertEquals(id, event.getRequestId());
        assertTrue(record.getValue().getContext().startsWith("[request_id=" + id + "] "));
        assertTrue(record.getValue().getContext().contains("token version[0 => 1]"));
        assertEquals("editor", record.getValue().getOperator());
    }
}
