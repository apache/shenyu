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

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercise the node authentication filter, not a direct controller call.
 */
public final class ClusterEventAuthFilterTest {

    @AfterEach
    public void tearDown() {
        SecurityAuditLog.resetRateLimiter();
    }

    @Test
    public void testOrdinaryUserCannotPublishButNodeCan() throws Exception {
        ClusterProperties properties = new ClusterProperties();
        properties.setEnabled(true);
        properties.setEventSecret("dedicated-node-secret");
        ClusterEventAuthFilter filter = new ClusterEventAuthFilter(properties);
        filter.processPathConfig("/cluster/data-change-event", null);
        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/cluster/data-change-event");
        request.setServletPath("/cluster/data-change-event");
        request.addHeader("X-Access-Token", "ordinary-user-token");
        MockHttpServletResponse response = new MockHttpServletResponse();
        AtomicBoolean reachedController = new AtomicBoolean();
        filter.doFilter(request, response, (req, res) -> reachedController.set(true));
        assertEquals(403, response.getStatus());
        assertFalse(reachedController.get());
        request.addHeader(ClusterEventAuthFilter.HEADER, "wrong-secret");
        filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
        assertFalse(reachedController.get());
        request.removeHeader(ClusterEventAuthFilter.HEADER);
        request.addHeader(ClusterEventAuthFilter.HEADER, "dedicated-node-secret");
        filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
        assertTrue(reachedController.get());
        reachedController.set(false);
        properties.setEventSecret("");
        filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
        assertFalse(reachedController.get());
    }

    @Test
    public void testDeniedRequestsEmitFixedReasonAuditWithoutSecretLeak() throws Exception {
        ClusterProperties properties = new ClusterProperties();
        properties.setEnabled(false);
        properties.setEventSecret("expected-secret-8179");
        ClusterEventAuthFilter filter = new ClusterEventAuthFilter(properties);
        filter.processPathConfig("/cluster/data-change-event", null);
        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/cluster/data-change-event");
        request.setServletPath("/cluster/data-change-event");
        ch.qos.logback.classic.Logger auditLogger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(SecurityAuditLog.LOGGER_NAME);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        auditLogger.addAppender(appender);
        try {
            // 1. cluster auth disabled
            filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> { });
            // 2. enabled but the expected secret is not configured
            properties.setEnabled(true);
            properties.setEventSecret("");
            filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> { });
            // 3. secret configured but the request carries none
            properties.setEventSecret("expected-secret-8179");
            filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> { });
            // 4. wrong secret supplied: denied with 403, same as before
            request.addHeader(ClusterEventAuthFilter.HEADER, "supplied-secret-do-not-log-8179");
            MockHttpServletResponse deniedResponse = new MockHttpServletResponse();
            filter.doFilter(request, deniedResponse, (req, res) -> { });
            assertEquals(403, deniedResponse.getStatus());
            // 5. correct secret: passes and produces no audit event
            AtomicBoolean reachedController = new AtomicBoolean();
            request.removeHeader(ClusterEventAuthFilter.HEADER);
            request.addHeader(ClusterEventAuthFilter.HEADER, "expected-secret-8179");
            filter.doFilter(request, new MockHttpServletResponse(), (req, res) -> reachedController.set(true));
            assertTrue(reachedController.get());
        } finally {
            auditLogger.detachAppender(appender);
        }
        List<ILoggingEvent> events = appender.list;
        assertEquals(4, events.size());
        String allLogs = events.stream().map(ILoggingEvent::getFormattedMessage).collect(Collectors.joining("\n"));
        assertTrue(allLogs.contains("eventType=cluster_event_auth"), allLogs);
        assertTrue(allLogs.contains("outcome=denied"), allLogs);
        assertTrue(allLogs.contains("reason=cluster_auth_disabled"), allLogs);
        assertTrue(allLogs.contains("reason=secret_not_configured"), allLogs);
        assertTrue(allLogs.contains("reason=missing_secret"), allLogs);
        assertTrue(allLogs.contains("reason=invalid_secret"), allLogs);
        assertTrue(allLogs.contains("resource=/cluster/data-change-event"), allLogs);
        assertFalse(allLogs.contains("supplied-secret-do-not-log-8179"), "supplied secret must not appear in audit logs");
        assertFalse(allLogs.contains("expected-secret-8179"), "configured secret must not appear in audit logs");
    }
}
