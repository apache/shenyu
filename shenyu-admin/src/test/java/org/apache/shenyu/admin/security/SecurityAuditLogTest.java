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

package org.apache.shenyu.admin.security;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.utils.SecurityAuditUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link SecurityAuditLog}.
 */
public class SecurityAuditLogTest {

    private ch.qos.logback.classic.Logger auditLogger;

    private ListAppender<ILoggingEvent> appender;

    @BeforeEach
    public void setUp() {
        auditLogger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(SecurityAuditLog.LOGGER_NAME);
        appender = new ListAppender<>();
        appender.start();
        auditLogger.addAppender(appender);
        SecurityAuditLog.resetRateLimiter();
    }

    @AfterEach
    public void tearDown() {
        auditLogger.detachAppender(appender);
        MDC.clear();
        SecurityAuditLog.resetRateLimiter();
    }

    @Test
    public void testEventContainsAllFieldsInFixedOrder() {
        MDC.put(SecurityAuditLog.REQUEST_ID_MDC_KEY, "req-001");
        SecurityAuditLog.event(SecurityAuditEventType.LOGIN)
                .outcome(SecurityAuditLog.Outcome.FAILURE)
                .action("login")
                .reason("bad_credentials")
                .actor("unauthenticated")
                .submittedIdentity("alice")
                .resource("/platform/login")
                .remoteAddr("127.0.0.1")
                .emit();
        List<ILoggingEvent> events = appender.list;
        assertThat(events.size(), is(1));
        ILoggingEvent event = events.get(0);
        assertThat(event.getLevel(), is(Level.WARN));
        assertThat(event.getFormattedMessage(), is("SECURITY_AUDIT eventType=login outcome=failure action=login"
                + " reason=bad_credentials actor=unauthenticated submittedIdentity=alice namespace=n/a"
                + " resource=/platform/login remoteAddr=127.0.0.1 requestId=req-001 fingerprint=n/a"));
    }

    @Test
    public void testSuccessIsLoggedAtInfoAndFailuresAtWarn() {
        SecurityAuditLog.event(SecurityAuditEventType.LOGIN)
                .outcome(SecurityAuditLog.Outcome.SUCCESS)
                .action("login")
                .actor("admin")
                .emit();
        SecurityAuditLog.event(SecurityAuditEventType.PERMISSION_CHECK)
                .outcome(SecurityAuditLog.Outcome.DENIED)
                .emit();
        assertThat(appender.list.size(), is(2));
        assertThat(appender.list.get(0).getLevel(), is(Level.INFO));
        assertThat(appender.list.get(1).getLevel(), is(Level.WARN));
    }

    @Test
    public void testUnsetFieldsFallBackToSafePlaceholders() {
        SecurityAuditLog.event(SecurityAuditEventType.PERMISSION_CHECK)
                .outcome(SecurityAuditLog.Outcome.DENIED)
                .action("permission_check")
                .reason("permission_denied")
                .emit();
        String message = appender.list.get(0).getFormattedMessage();
        assertThat(message, is("SECURITY_AUDIT eventType=permission_check outcome=denied action=permission_check"
                + " reason=permission_denied actor=unknown submittedIdentity=n/a namespace=n/a"
                + " resource=n/a remoteAddr=n/a requestId=no-request fingerprint=n/a"));
    }

    @Test
    public void testUserControlledValuesAreSanitized() {
        String longResource = "x".repeat(300);
        SecurityAuditLog.event(SecurityAuditEventType.LOGIN)
                .outcome(SecurityAuditLog.Outcome.FAILURE)
                .submittedIdentity("al\r\nice")
                .actor("un\tknown")
                .resource(longResource)
                .emit();
        String message = appender.list.get(0).getFormattedMessage();
        assertThat(message.contains("submittedIdentity=alice"), is(true));
        assertThat(message.contains("actor=unknown"), is(true));
        assertThat(message.contains("resource=" + "x".repeat(128)), is(true));
        assertThat(message.contains("\r"), is(false));
        assertThat(message.contains("\n"), is(false));
        assertThat(message.contains("\t"), is(false));
    }

    @Test
    public void testFingerprintIsEmittedButRawSecretNeverAppears() {
        final String secret = "shenyu8179-do-not-log-me";
        SecurityAuditLog.event(SecurityAuditEventType.TOKEN_VERIFICATION)
                .outcome(SecurityAuditLog.Outcome.FAILURE)
                .reason("signature_invalid")
                .fingerprint(SecurityAuditUtils.fingerprint(secret))
                .emit();
        String message = appender.list.get(0).getFormattedMessage();
        assertThat(message.contains("fingerprint=" + SecurityAuditUtils.fingerprint(secret)), is(true));
        assertThat(message.contains(secret), is(false));
    }

    @Test
    public void testRateLimitingSuppressesFloodAndEmitsOneBoundedSummary() throws InterruptedException {
        SecurityAuditLog.setRateLimiter(new SecurityAuditRateLimiter(1_500L, 2));
        for (int i = 0; i < 50; i++) {
            SecurityAuditLog.event(SecurityAuditEventType.LOGIN).outcome(SecurityAuditLog.Outcome.FAILURE).emit();
        }
        Thread.sleep(1_600L);
        SecurityAuditLog.event(SecurityAuditEventType.LOGIN).outcome(SecurityAuditLog.Outcome.FAILURE).emit();
        long eventLines = appender.list.stream()
                .filter(event -> event.getFormattedMessage().contains("eventType=login"))
                .count();
        List<ILoggingEvent> summaries = appender.list.stream()
                .filter(event -> event.getFormattedMessage().contains("eventType=audit_suppressed"))
                .toList();
        assertThat(eventLines, is(3L));
        assertThat(summaries.size(), is(1));
        String summary = summaries.get(0).getFormattedMessage();
        assertThat(summary.contains("suppressedEventType=login"), is(true));
        assertThat(summary.contains("suppressedCount=48"), is(true));
        assertThat(summaries.get(0).getLevel(), is(Level.WARN));
    }
}
