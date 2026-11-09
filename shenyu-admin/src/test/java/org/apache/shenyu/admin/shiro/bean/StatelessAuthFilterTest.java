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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.BearerToken;
import org.apache.shiro.authc.LockedAccountException;
import org.apache.shiro.subject.Subject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.http.HttpMethod;

import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
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

    @AfterEach
    public void tearDown() {
        MDC.clear();
        SecurityAuditLog.resetRateLimiter();
    }

    @Test
    public void testIsAccessAllowed() {
        Object obj = mock(Object.class);
        assertFalse(statelessAuthFilter.isAccessAllowed(httpServletRequest, httpServletResponse, obj));
    }
    
    @Test
    public void testOnAccessDenied() throws Exception {
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.OPTIONS.name());
        assertTrue(statelessAuthFilter.onAccessDenied(httpServletRequest, httpServletResponse));
    }

    @Test
    public void testOnAccessDeniedWithMissingToken() throws Exception {
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.GET.name());
        when(httpServletRequest.getRequestURI()).thenReturn("/shenyu8179/probe");
        when(httpServletRequest.getRemoteAddr()).thenReturn("127.0.0.1");
        StringWriter body = new StringWriter();
        when(httpServletResponse.getWriter()).thenReturn(new PrintWriter(body));
        List<ILoggingEvent> events = captureFilterLogs(() -> {
            try {
                assertFalse(statelessAuthFilter.onAccessDenied(httpServletRequest, httpServletResponse));
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        });
        verifyUnionFailResponse(body);
        assertFalse(events.isEmpty());
        assertTrue(events.stream().allMatch(event -> Level.WARN.equals(event.getLevel())));
        String allLogs = renderAll(events);
        assertTrue(allLogs.contains("missing_token"), allLogs);
        assertTrue(allLogs.contains("/shenyu8179/probe"), allLogs);
        assertTrue(allLogs.contains("127.0.0.1"), allLogs);
    }

    @Test
    public void testOnAccessDeniedWithInvalidTokenDoesNotLogRawToken() throws Exception {
        final String rawToken = "shenyu8179header.shenyu8179payload.shenyu8179signatureDoNotLogMe";
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.GET.name());
        when(httpServletRequest.getHeader(HEAD_TOKEN)).thenReturn(rawToken);
        when(httpServletRequest.getRequestURI()).thenReturn("/shenyu8179/secret");
        when(httpServletRequest.getRemoteAddr()).thenReturn("192.168.1.100");
        StringWriter body = new StringWriter();
        when(httpServletResponse.getWriter()).thenReturn(new PrintWriter(body));
        Subject subject = mock(Subject.class);
        doThrow(new AuthenticationException("sensitive realm detail, token=" + rawToken)).when(subject).login(any(BearerToken.class));
        StatelessAuthFilter filter = new StubSubjectStatelessAuthFilter(subject);
        List<ILoggingEvent> events = captureFilterLogs(() -> {
            try {
                assertFalse(filter.onAccessDenied(httpServletRequest, httpServletResponse));
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        });
        verifyUnionFailResponse(body);
        assertFalse(events.isEmpty());
        String allLogs = renderAll(events);
        assertNoTokenLeak(allLogs, rawToken);
        assertFalse(allLogs.contains("sensitive realm detail"), allLogs);
        assertTrue(events.stream().anyMatch(event -> Level.WARN.equals(event.getLevel())
                && event.getFormattedMessage().contains("authentication_failed")), allLogs);
        assertTrue(allLogs.contains("/shenyu8179/secret"), allLogs);
        assertTrue(allLogs.contains("192.168.1.100"), allLogs);
    }

    @Test
    public void testOnAccessDeniedWithAuthenticationSubclassLogsGenericReason() throws Exception {
        final String rawToken = "shenyu8179-subclass-header.shenyu8179-subclass-payload.shenyu8179-subclass-signature";
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.GET.name());
        when(httpServletRequest.getHeader(HEAD_TOKEN)).thenReturn(rawToken);
        when(httpServletRequest.getRequestURI()).thenReturn("/shenyu8179/locked");
        when(httpServletRequest.getRemoteAddr()).thenReturn("10.0.0.1");
        StringWriter body = new StringWriter();
        when(httpServletResponse.getWriter()).thenReturn(new PrintWriter(body));
        Subject subject = mock(Subject.class);
        doThrow(new LockedAccountException("account [admin] is locked")).when(subject).login(any(BearerToken.class));
        StatelessAuthFilter filter = new StubSubjectStatelessAuthFilter(subject);
        List<ILoggingEvent> events = captureFilterLogs(() -> {
            try {
                assertFalse(filter.onAccessDenied(httpServletRequest, httpServletResponse));
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        });
        verifyUnionFailResponse(body);
        assertFalse(events.isEmpty());
        String allLogs = renderAll(events);
        assertNoTokenLeak(allLogs, rawToken);
        assertFalse(allLogs.contains("account [admin] is locked"), allLogs);
        assertTrue(events.stream().anyMatch(event -> Level.WARN.equals(event.getLevel())
                && event.getFormattedMessage().contains("authentication_failed")), allLogs);
    }

    @Test
    public void testOnAccessDeniedWithValidToken() {
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.GET.name());
        when(httpServletRequest.getHeader(HEAD_TOKEN)).thenReturn("shenyu8179-valid-token");
        Subject subject = mock(Subject.class);
        StatelessAuthFilter filter = new StubSubjectStatelessAuthFilter(subject);
        List<ILoggingEvent> events = captureFilterLogs(() -> {
            try {
                assertTrue(filter.onAccessDenied(httpServletRequest, httpServletResponse));
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        });
        assertTrue(events.isEmpty(), "successful authentication must not produce failure audit events");
        verify(subject).login(any(BearerToken.class));
        verify(httpServletResponse, never()).setStatus(anyInt());
        verify(httpServletResponse, never()).addHeader(any(String.class), any(String.class));
    }

    @Test
    public void testOnAccessDeniedUsesMdcRequestId() {
        final String rawToken = "shenyu8179mdc.header.shenyu8179mdc.signature";
        when(httpServletRequest.getMethod()).thenReturn(HttpMethod.GET.name());
        when(httpServletRequest.getHeader(HEAD_TOKEN)).thenReturn(rawToken);
        when(httpServletRequest.getRequestURI()).thenReturn("/shenyu8179/mdc");
        when(httpServletRequest.getRemoteAddr()).thenReturn("10.1.1.1");
        StringWriter body = new StringWriter();
        try {
            when(httpServletResponse.getWriter()).thenReturn(new PrintWriter(body));
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
        Subject subject = mock(Subject.class);
        try {
            doThrow(new AuthenticationException("realm detail")).when(subject).login(any(BearerToken.class));
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
        StatelessAuthFilter filter = new StubSubjectStatelessAuthFilter(subject);
        MDC.put(SecurityAuditLog.REQUEST_ID_MDC_KEY, "filter-req-8179");
        List<ILoggingEvent> events = captureFilterLogs(() -> {
            try {
                assertFalse(filter.onAccessDenied(httpServletRequest, httpServletResponse));
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        });
        String allLogs = renderAll(events);
        assertTrue(allLogs.contains("eventType=request_auth"), allLogs);
        assertTrue(allLogs.contains("requestId=filter-req-8179"), allLogs);
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

    /**
     * verify the unified failure response keeps its status, content type, CORS headers and body unchanged.
     *
     * @param body captured response body
     */
    private void verifyUnionFailResponse(final StringWriter body) {
        verify(httpServletResponse).setContentType("application/json;charset=utf-8");
        verify(httpServletResponse).setCharacterEncoding("utf-8");
        verify(httpServletResponse).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        verify(httpServletResponse).addHeader("Access-Control-Allow-Origin", "*");
        verify(httpServletResponse).addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
        verify(httpServletResponse).addHeader("Access-Control-Allow-Headers", "Content-Type");
        verify(httpServletResponse).addHeader("Access-Control-Max-Age", "1800");
        String expected = GsonUtils.getInstance().toJson(ShenyuAdminResult.error(CommonErrorCode.TOKEN_ERROR, ShenyuResultMessage.TOKEN_IS_ERROR));
        assertThat(body.toString().trim(), is(expected));
    }

    /**
     * assert the logs carry no reversible form of the token: neither the raw token, nor any of its
     * dot-separated segments, nor its Base64 encoding, nor the full SHA-256 digest.
     *
     * @param allLogs combined rendered log text
     * @param rawToken token that must stay out of the logs
     */
    private void assertNoTokenLeak(final String allLogs, final String rawToken) {
        assertFalse(allLogs.contains(rawToken), "raw token must not appear in audit logs");
        for (String segment : rawToken.split("\\.")) {
            assertFalse(allLogs.contains(segment), "JWT segment must not appear in audit logs");
        }
        assertFalse(allLogs.contains(Base64.getEncoder().encodeToString(rawToken.getBytes(StandardCharsets.UTF_8))),
                "Base64 form of token must not appear in audit logs");
        assertFalse(allLogs.contains(sha256Hex(rawToken)), "full SHA-256 digest of token must not appear in audit logs");
    }

    /**
     * capture the logback events emitted on the security audit logger while running the given action.
     *
     * @param runnable action to run
     * @return captured logging events
     */
    private List<ILoggingEvent> captureFilterLogs(final Runnable runnable) {
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

    /**
     * render one event as its formatted message plus any attached throwable detail,
     * so assertions also cover exceptions handed to the logger.
     *
     * @param event logging event
     * @return rendered log text
     */
    private String render(final ILoggingEvent event) {
        StringBuilder builder = new StringBuilder(event.getFormattedMessage());
        if (Objects.nonNull(event.getThrowableProxy())) {
            builder.append(' ').append(event.getThrowableProxy().getClassName())
                    .append(' ').append(event.getThrowableProxy().getMessage());
        }
        return builder.toString();
    }

    /**
     * render all events into a single text for contains-assertions.
     *
     * @param events logging events
     * @return combined log text
     */
    private String renderAll(final List<ILoggingEvent> events) {
        return events.stream().map(this::render).collect(Collectors.joining("\n"));
    }

    /**
     * full SHA-256 hex digest of the given value, used to prove the complete digest is never logged.
     *
     * @param value value to hash
     * @return full hex digest
     */
    private String sha256Hex(final String value) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
            StringBuilder hex = new StringBuilder();
            for (byte b : digest) {
                hex.append(String.format("%02x", b));
            }
            return hex.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * filter with a stubbed subject, so the login failure path can be exercised without a security manager.
     */
    private static final class StubSubjectStatelessAuthFilter extends StatelessAuthFilter {

        private final Subject subject;

        StubSubjectStatelessAuthFilter(final Subject subject) {
            this.subject = subject;
        }

        @Override
        protected Subject getSubject(final ServletRequest servletRequest, final ServletResponse servletResponse) {
            return subject;
        }
    }
}
