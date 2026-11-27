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

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import org.apache.shenyu.admin.utils.AdminSecurityAudit;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.subject.Subject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

/**
 * Exercise the servlet authentication boundary without ever logging test credentials.
 */
class AdminAuthenticationAuditTest {

    private final ListAppender<ILoggingEvent> events = new ListAppender<>();

    private final List<Logger> loggers = Arrays.asList(
            (Logger) LoggerFactory.getLogger(StatelessAuthFilter.class),
            (Logger) LoggerFactory.getLogger(JwtUtils.class),
            (Logger) LoggerFactory.getLogger("org.apache.shenyu.admin.security.audit"));

    @BeforeEach
    void captureLogs() {
        events.start();
        loggers.forEach(logger -> logger.addAppender(events));
    }

    @AfterEach
    void removeLogCapture() {
        loggers.forEach(logger -> logger.detachAppender(events));
        events.stop();
    }

    @Test
    void rejectedTokenAndExceptionContentsNeverReachLogs() throws Exception {
        String secret = "synthetic-token-must-not-be-logged";
        Subject subject = mock(Subject.class);
        doThrow(new AuthenticationException("untrusted claim " + secret)).when(subject).login(any(AuthenticationToken.class));
        StatelessAuthFilter filter = filter(subject);
        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/private");
        request.addHeader("X-Access-Token", secret);
        request.addHeader(AdminSecurityAudit.REQUEST_ID_HEADER, "spoofed-correlation");
        MockHttpServletResponse response = new MockHttpServletResponse();
        assertFalse(filter.onAccessDenied(request, response));
        assertEquals(401, response.getStatus());
        for (ILoggingEvent event : events.list) {
            assertFalse(event.getFormattedMessage().contains(secret));
            assertNull(event.getThrowableProxy());
        }
        assertEquals(1, events.list.size());
        JsonObject audit = JsonParser.parseString(events.list.get(0).getFormattedMessage()).getAsJsonObject();
        assertEquals("unknown", audit.get("actor_id").getAsString());
        assertEquals("REJECTED_TOKEN", audit.get("reason").getAsString());
        assertEquals(response.getHeader(AdminSecurityAudit.REQUEST_ID_HEADER), audit.get("request_id").getAsString());
        assertNotEquals("spoofed-correlation", audit.get("request_id").getAsString());
    }

    @Test
    void jwtVerificationDoesNotLogCredentialsOrDecodeExceptions() {
        String secret = "synthetic-invalid-jwt-secret";
        assertFalse(JwtUtils.verifyToken(secret, "key"));
        assertTrue(events.list.isEmpty());
    }

    @Test
    void missingTokenHasItsOwnCategoryAndKeepsUnauthorizedResponse() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest("GET", "/private");
        MockHttpServletResponse response = new MockHttpServletResponse();
        assertFalse(filter(mock(Subject.class)).onAccessDenied(request, response));
        assertEquals(401, response.getStatus());
        assertEquals("MISSING_TOKEN", JsonParser.parseString(events.list.get(0).getFormattedMessage())
                .getAsJsonObject().get("reason").getAsString());
    }

    @Test
    void successfulAuthenticationAndOptionsKeepExistingBehavior() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest("GET", "/private");
        request.addHeader("X-Access-Token", "valid-test-credential");
        MockHttpServletResponse response = new MockHttpServletResponse();
        StatelessAuthFilter filter = filter(mock(Subject.class));
        assertTrue(filter.onAccessDenied(request, response));
        request.setMethod("OPTIONS");
        assertTrue(filter.onAccessDenied(request, response));
        assertTrue(events.list.isEmpty());
    }

    private StatelessAuthFilter filter(final Subject subject) {
        return new StatelessAuthFilter() {
            @Override
            protected Subject getSubject(final ServletRequest request, final ServletResponse response) {
                return subject;
            }
        };
    }
}
