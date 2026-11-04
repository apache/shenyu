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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.service.AppAuthAccessPolicy;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.dao.DeadlockLoserDataAccessException;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Verify the lifecycle success audit: exactly one event per successful apply/revoke/replacePaths,
 * emitted only after the transaction bean returned (i.e. after its commit), never on rollback,
 * and never carrying credentials.
 */
@ExtendWith(MockitoExtension.class)
class AppAuthLifecycleServiceImplTest {

    private static final String CALLER = "10001";

    @Mock
    private AppAuthAccessPolicy accessPolicy;

    @Mock
    private AppAuthTransactionService transactionService;

    @Mock
    private AppAuthRequestMapper requestMapper;

    private AppAuthLifecycleServiceImpl service;

    @BeforeEach
    void setUp() {
        service = new AppAuthLifecycleServiceImpl(accessPolicy, transactionService, requestMapper);
        MDC.clear();
    }

    @AfterEach
    void tearDown() {
        MDC.clear();
        SecurityAuditLog.resetRateLimiter();
    }

    @Test
    void applySuccessEmitsOneAuditWithoutCredentials() {
        final AppAuthApplyRequestDTO request = buildApplyRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("CREATED").namespaceId("ns-1").requestId("req-1").id("auth-1")
                .appKey("appKey-8179-credential").appSecret("appSecret-8179-credential")
                .enabled(false).open(true).pathList(List.of("/a")).build();
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleApply")).thenReturn(CALLER);
        when(transactionService.apply(eq(CALLER), eq(request), anyString())).thenReturn(resultVO);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.apply(request);
            assertSame(resultVO, result.getData());
        });
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertEquals(Level.INFO, events.get(0).getLevel());
        assertTrue(message.contains("eventType=app_auth_lifecycle"), message);
        assertTrue(message.contains("outcome=success"), message);
        assertTrue(message.contains("action=apply"), message);
        assertTrue(message.contains("actor=" + CALLER), message);
        assertTrue(message.contains("namespace=ns-1"), message);
        assertTrue(message.contains("resource=auth-1"), message);
        assertFalse(message.contains("appKey-8179-credential"), "appKey must not appear in audit logs");
        assertFalse(message.contains("appSecret-8179-credential"), "appSecret must not appear in audit logs");
    }

    @Test
    void applyRollbackEmitsNoSuccessAudit() {
        final AppAuthApplyRequestDTO request = buildApplyRequest();
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleApply")).thenReturn(CALLER);
        when(transactionService.apply(eq(CALLER), eq(request), anyString()))
                .thenThrow(new ShenyuAdminException("simulated database failure, transaction rolled back"));
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertThrows(ShenyuAdminException.class, () -> service.apply(request)));
        assertTrue(events.isEmpty(), "a rolled-back transaction must not produce a success audit event");
    }

    @Test
    void applyEmitsSingleAuditAfterDeadlockRetry() {
        final AppAuthApplyRequestDTO request = buildApplyRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("CREATED").namespaceId("ns-1").requestId("req-1").id("auth-1")
                .appKey(null).appSecret(null).enabled(false).open(true).pathList(List.of("/a")).build();
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleApply")).thenReturn(CALLER);
        when(transactionService.apply(eq(CALLER), eq(request), anyString()))
                .thenThrow(new DeadlockLoserDataAccessException("lock wait", null))
                .thenReturn(resultVO);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.apply(request);
            assertSame(resultVO, result.getData());
        });
        long successEvents = events.stream()
                .filter(event -> event.getFormattedMessage().contains("eventType=app_auth_lifecycle"))
                .count();
        assertEquals(1, successEvents, "retried-then-committed apply must emit exactly one success event");
    }

    @Test
    void revokeSuccessEmitsAudit() {
        final AppAuthRevokeRequestDTO request = buildRevokeRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("REVOKED").namespaceId("ns-1").requestId("req-9").build();
        final AppAuthRequestDO tombstone = new AppAuthRequestDO();
        tombstone.setAuthId("auth-9");
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke")).thenReturn(CALLER);
        when(transactionService.revoke(CALLER, request)).thenReturn(resultVO);
        when(requestMapper.selectByKey(CALLER, "ns-1", "req-9")).thenReturn(tombstone);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.revoke(request);
            assertSame(resultVO, result.getData());
        });
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("action=revoke"), message);
        assertTrue(message.contains("outcome=success"), message);
        assertTrue(message.contains("actor=" + CALLER), message);
        assertTrue(message.contains("namespace=ns-1"), message);
        assertTrue(message.contains("resource=auth-9"), message);
        assertFalse(message.contains("resource=req-9"), message);
        verify(requestMapper).selectByKey(CALLER, "ns-1", "req-9");
    }

    @Test
    void revokeFallsBackToRequestIdWhenTombstoneMissing() {
        final AppAuthRevokeRequestDTO request = buildRevokeRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("REVOKED").namespaceId("ns-1").requestId("req-9").build();
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke")).thenReturn(CALLER);
        when(transactionService.revoke(CALLER, request)).thenReturn(resultVO);
        when(requestMapper.selectByKey(CALLER, "ns-1", "req-9")).thenReturn(null);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.revoke(request);
            assertSame(resultVO, result.getData());
        });
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("action=revoke"), message);
        assertTrue(message.contains("resource=req-9"), message);
        verify(requestMapper).selectByKey(CALLER, "ns-1", "req-9");
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"", "   "})
    void revokeFallsBackToRequestIdWhenAuthIdBlank(final String authId) {
        final AppAuthRevokeRequestDTO request = buildRevokeRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("REVOKED").namespaceId("ns-1").requestId("req-9").build();
        final AppAuthRequestDO tombstone = new AppAuthRequestDO();
        tombstone.setAuthId(authId);
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke")).thenReturn(CALLER);
        when(transactionService.revoke(CALLER, request)).thenReturn(resultVO);
        when(requestMapper.selectByKey(CALLER, "ns-1", "req-9")).thenReturn(tombstone);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.revoke(request);
            assertSame(resultVO, result.getData());
        });
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("action=revoke"), message);
        assertTrue(message.contains("resource=req-9"), message);
        verify(requestMapper).selectByKey(CALLER, "ns-1", "req-9");
    }

    @Test
    void revokeFallsBackToRequestIdWhenLookupFails() {
        final AppAuthRevokeRequestDTO request = buildRevokeRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("REVOKED").namespaceId("ns-1").requestId("req-9").build();
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke")).thenReturn(CALLER);
        when(transactionService.revoke(CALLER, request)).thenReturn(resultVO);
        when(requestMapper.selectByKey(CALLER, "ns-1", "req-9")).thenThrow(new RuntimeException("simulated lookup failure"));
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            // the lookup failure must not propagate and must not change the committed revoke response
            ShenyuAdminResult result = service.revoke(request);
            assertSame(resultVO, result.getData());
        });
        assertEquals(1, events.size(), "the committed revoke must still emit exactly one success audit event");
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("action=revoke"), message);
        assertTrue(message.contains("outcome=success"), message);
        assertTrue(message.contains("resource=req-9"), message);
        verify(requestMapper).selectByKey(CALLER, "ns-1", "req-9");
    }

    @Test
    void revokeQueriesAuthIdOnlyOnceAfterFinalCommit() {
        final AppAuthRevokeRequestDTO request = buildRevokeRequest();
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("REVOKED").namespaceId("ns-1").requestId("req-9").build();
        final AppAuthRequestDO tombstone = new AppAuthRequestDO();
        tombstone.setAuthId("auth-9");
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke")).thenReturn(CALLER);
        when(transactionService.revoke(CALLER, request))
                .thenThrow(new DeadlockLoserDataAccessException("lock wait", null))
                .thenReturn(resultVO);
        when(requestMapper.selectByKey(CALLER, "ns-1", "req-9")).thenReturn(tombstone);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.revoke(request);
            assertSame(resultVO, result.getData());
        });
        long successEvents = events.stream()
                .filter(event -> event.getFormattedMessage().contains("eventType=app_auth_lifecycle"))
                .count();
        assertEquals(1, successEvents, "retried-then-committed revoke must emit exactly one success event");
        assertTrue(events.get(0).getFormattedMessage().contains("resource=auth-9"), events.get(0).getFormattedMessage());
        verify(requestMapper, times(1)).selectByKey(CALLER, "ns-1", "req-9");
    }

    @Test
    void revokeRollbackEmitsNoSuccessAudit() {
        final AppAuthRevokeRequestDTO request = new AppAuthRevokeRequestDTO();
        request.setNamespaceId("ns-1");
        request.setRequestId("req-9");
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleRevoke")).thenReturn(CALLER);
        when(transactionService.revoke(CALLER, request))
                .thenThrow(new ShenyuAdminException("simulated database failure, transaction rolled back"));
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertThrows(ShenyuAdminException.class, () -> service.revoke(request)));
        assertTrue(events.isEmpty(), "a rolled-back transaction must not produce a success audit event");
        verify(requestMapper, never()).selectByKey(anyString(), anyString(), anyString());
    }

    @Test
    void replacePathsSuccessEmitsAudit() {
        final AppAuthReplacePathsRequestDTO request = new AppAuthReplacePathsRequestDTO();
        request.setNamespaceId("ns-1");
        request.setAuthId("auth-7");
        request.setPathList(List.of("/x"));
        final AppAuthResultVO resultVO = AppAuthResultVO.builder()
                .state("CREATED").namespaceId("ns-1").requestId("req-7").id("auth-7")
                .appKey(null).appSecret(null).enabled(false).open(true).pathList(List.of("/x")).build();
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleReplacePaths")).thenReturn(CALLER);
        when(transactionService.replacePaths(CALLER, request)).thenReturn(resultVO);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            ShenyuAdminResult result = service.replacePaths(request);
            assertSame(resultVO, result.getData());
        });
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("action=replace_paths"), message);
        assertTrue(message.contains("outcome=success"), message);
        assertTrue(message.contains("resource=auth-7"), message);
        assertTrue(message.contains("namespace=ns-1"), message);
    }

    @Test
    void replacePathsRollbackEmitsNoSuccessAudit() {
        final AppAuthReplacePathsRequestDTO request = new AppAuthReplacePathsRequestDTO();
        request.setNamespaceId("ns-1");
        request.setAuthId("auth-7");
        request.setPathList(List.of("/x"));
        when(accessPolicy.requireServiceCaller("system:authen:lifecycleReplacePaths")).thenReturn(CALLER);
        when(transactionService.replacePaths(CALLER, request))
                .thenThrow(new ShenyuAdminException("simulated database failure, transaction rolled back"));
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertThrows(ShenyuAdminException.class, () -> service.replacePaths(request)));
        assertTrue(events.isEmpty(), "a rolled-back transaction must not produce a success audit event");
    }

    @Test
    void accessDenialEmitsNoLifecycleAudit() {
        when(accessPolicy.requireServiceCaller(anyString()))
                .thenThrow(new ShenyuAdminException("Service identity check failed"));
        List<ILoggingEvent> events = captureAuditLogs(() ->
                assertThrows(ShenyuAdminException.class, () -> service.apply(buildApplyRequest())));
        assertTrue(events.isEmpty(), "an access denial must not produce a lifecycle success event");
    }

    private AppAuthApplyRequestDTO buildApplyRequest() {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId("ns-1");
        request.setRequestId("req-1");
        request.setUserId("user-1");
        request.setAppName("app8179");
        request.setPathList(List.of("/a"));
        return request;
    }

    private AppAuthRevokeRequestDTO buildRevokeRequest() {
        final AppAuthRevokeRequestDTO request = new AppAuthRevokeRequestDTO();
        request.setNamespaceId("ns-1");
        request.setRequestId("req-9");
        request.setReason("cleanup");
        return request;
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
