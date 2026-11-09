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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shiro.subject.Subject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.context.ConfigurableApplicationContext;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link JwtUtils}.
 */
public class JwtUtilsTest {

    public static final String TOKEN = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1c2VyTmFtZSIsImlhdCI6MTYxMTU5MDUwOH0.yAuGpmg1DSYNryZQQA6d66HO87E8eWAFLJVhYscx8K8";

    private static final Long EXPIRED_SECONDS = 86400L;

    private static final String KEY = "jwt-token";

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        JwtProperties jwtProperties = mock(JwtProperties.class);
        when(jwtProperties.getExpiredSeconds()).thenReturn(EXPIRED_SECONDS);
        when(context.getBean(JwtProperties.class)).thenReturn(jwtProperties);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        MDC.clear();
    }

    @AfterEach
    public void tearDown() {
        MDC.clear();
        SecurityAuditLog.resetRateLimiter();
    }

    @Test
    public void testGetIssuer() {
        assertThat(JwtUtils.getIssuer(TOKEN), is(""));
    }

    @Test
    public void testGetClientId() {
        assertThat(JwtUtils.getClientId(TOKEN), is(""));
    }

    @Test
    public void testGenerateToken() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
    }

    @Test
    public void testGenerateTokenWithExpireSeconds() {
        Long expireSeconds = 3600L;
        // 1 hour
        String token = JwtUtils.generateToken("userName", KEY, "clientId", expireSeconds);
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
        // Verify token is valid
        assertThat(JwtUtils.verifyToken(token, KEY), is(true));
    }

    @Test
    public void testGenerateTokenWithNullExpireSeconds() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId", null);
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
    }

    @Test
    public void testGenerateTokenWithNullUserName() {
        String token = JwtUtils.generateToken(null, KEY, "clientId");
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is(""));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
    }

    @Test
    public void testGenerateTokenWithNullClientId() {
        String token = JwtUtils.generateToken("userName", KEY, null);
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is(""));
    }

    @Test
    public void testGenerateTokenWithEmptyKey() {
        String token = JwtUtils.generateToken("userName", "", "clientId");
        assertThat(token, is(""));
    }

    @Test
    public void testGenerateTokenWithNullKey() {
        String token = JwtUtils.generateToken("userName", null, "clientId");
        assertThat(token, is(""));
    }

    @Test
    public void testGenerateTokenFailureLogDoesNotLeakExceptionDetail() {
        ch.qos.logback.classic.Logger jwtLogger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(JwtUtils.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        jwtLogger.addAppender(appender);
        final String token;
        try {
            // a null secret makes Algorithm.HMAC256 throw IllegalArgumentException
            token = JwtUtils.generateToken("userName", null, "clientId");
        } finally {
            jwtLogger.detachAppender(appender);
        }
        // the generation contract is unchanged: an empty token is returned on failure
        assertThat(token, is(""));
        assertThat(appender.list.size(), is(1));
        ILoggingEvent event = appender.list.get(0);
        assertThat(event.getLevel(), is(Level.WARN));
        assertThat(event.getFormattedMessage(), is("JWTToken generate fail, type=IllegalArgumentException"));
        assertThat(render(event).contains("The Secret cannot be null"), is(false));
        assertThat(event.getThrowableProxy(), is(nullValue()));
    }

    @Test
    public void testVerifyTokenWithValidToken() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(JwtUtils.verifyToken(token, KEY), is(true));
    }

    @Test
    public void testVerifyTokenWithInvalidToken() {
        assertThat(JwtUtils.verifyToken("invalid.token.here", KEY), is(false));
    }

    @Test
    public void testVerifyTokenWithWrongKey() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(JwtUtils.verifyToken(token, "wrong-key"), is(false));
    }

    @Test
    public void testVerifyTokenWithNullToken() {
        assertThrows(NullPointerException.class, () -> JwtUtils.verifyToken(null, KEY));
    }

    @Test
    public void testVerifyTokenWithNullKey() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThrows(IllegalArgumentException.class, () -> JwtUtils.verifyToken(token, null));
    }

    @Test
    public void testVerifyTokenWithEmptyKey() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThrows(IllegalArgumentException.class, () -> JwtUtils.verifyToken(token, ""));
    }

    @Test
    public void testVerifyTokenWithExpiredToken() {
        // Generate token with negative expiration to get an already-expired token
        String token = JwtUtils.generateToken("userName", KEY, "clientId", -1000L);
        assertThat(JwtUtils.verifyToken(token, KEY), is(false));
    }

    @Test
    public void testVerifyTokenWithWrongKeyDoesNotLogRawToken() {
        final String secretToken = JwtUtils.generateToken("logLeakUser", KEY, "logLeakClient");
        List<ILoggingEvent> events = captureJwtLogs(() -> JwtUtils.verifyToken(secretToken, "wrong-key"));
        String allLogs = renderAll(events);
        assertThat(events.isEmpty(), is(false));
        assertNoTokenLeak(allLogs, secretToken);
        assertThat(allLogs.contains("The Token's Signature resulted invalid"), is(false));
        assertThat(allLogs.contains("fingerprint="), is(true));
        assertThat(events.stream().anyMatch(event -> Level.WARN.equals(event.getLevel())
                && event.getFormattedMessage().contains("signature_invalid")), is(true));
    }

    @Test
    public void testVerifyTokenWithMalformedTokenDoesNotLogRawToken() {
        final String malformedToken = "shenyu8179malformed.malformedTestToken.notAJwt";
        List<ILoggingEvent> events = captureJwtLogs(() -> JwtUtils.verifyToken(malformedToken, KEY));
        String allLogs = renderAll(events);
        assertThat(events.isEmpty(), is(false));
        assertNoTokenLeak(allLogs, malformedToken);
        assertThat(allLogs.contains("doesn't have a valid JSON format"), is(false));
        assertThat(events.stream().anyMatch(event -> Level.WARN.equals(event.getLevel())
                && event.getFormattedMessage().contains("malformed")), is(true));
    }

    @Test
    public void testVerifyTokenWithExpiredTokenDoesNotLogRawToken() {
        final String expiredToken = JwtUtils.generateToken("expiredUser", KEY, "expiredClient", -1000L);
        List<ILoggingEvent> events = captureJwtLogs(() -> JwtUtils.verifyToken(expiredToken, KEY));
        String allLogs = renderAll(events);
        assertThat(events.isEmpty(), is(false));
        assertNoTokenLeak(allLogs, expiredToken);
        assertThat(allLogs.contains("The Token has expired on"), is(false));
        assertThat(events.stream().anyMatch(event -> Level.WARN.equals(event.getLevel())
                && event.getFormattedMessage().contains("expired")), is(true));
    }

    @Test
    public void testVerifyTokenWithValidTokenProducesNoFailureLog() {
        final String token = JwtUtils.generateToken("userName", KEY, "clientId");
        List<ILoggingEvent> events = captureJwtLogs(() -> JwtUtils.verifyToken(token, KEY));
        assertThat(events.isEmpty(), is(true));
    }

    @Test
    public void testVerifyTokenFailureUsesMdcRequestId() {
        MDC.put(SecurityAuditLog.REQUEST_ID_MDC_KEY, "jwt-req-8179");
        List<ILoggingEvent> events = captureJwtLogs(() -> JwtUtils.verifyToken("invalid.token.here", KEY));
        String allLogs = renderAll(events);
        assertThat(allLogs.contains("eventType=token_verification"), is(true));
        assertThat(allLogs.contains("requestId=jwt-req-8179"), is(true));
    }

    @Test
    public void testVerifyTokenFailureWithoutRequestContextUsesPlaceholder() {
        List<ILoggingEvent> events = captureJwtLogs(() -> JwtUtils.verifyToken("invalid.token.here", KEY));
        String allLogs = renderAll(events);
        assertThat(allLogs.contains("eventType=token_verification"), is(true));
        assertThat(allLogs.contains("requestId=no-request"), is(true));
        assertThat(allLogs.contains("actor=unauthenticated"), is(true));
    }

    @Test
    public void testGetIssuerWithNullToken() {
        assertThrows(NullPointerException.class, () -> JwtUtils.getIssuer(null));
    }

    @Test
    public void testGetIssuerWithEmptyToken() {
        assertThrows(Exception.class, () -> JwtUtils.getIssuer(""));
    }

    @Test
    public void testGetIssuerWithInvalidToken() {
        assertThrows(Exception.class, () -> JwtUtils.getIssuer("invalid.token"));
    }

    @Test
    public void testGetClientIdWithNullToken() {
        assertThrows(NullPointerException.class, () -> JwtUtils.getClientId(null));
    }

    @Test
    public void testGetClientIdWithEmptyToken() {
        assertThrows(Exception.class, () -> JwtUtils.getClientId(""));
    }

    @Test
    public void testGetClientIdWithInvalidToken() {
        assertThrows(Exception.class, () -> JwtUtils.getClientId("invalid.token"));
    }

    @Test
    public void testGetUserInfo() {
        UserInfo mockUserInfo = mock(UserInfo.class);
        Subject mockSubject = mock(Subject.class);
        when(mockSubject.getPrincipal()).thenReturn(mockUserInfo);

        try (MockedStatic<org.apache.shiro.SecurityUtils> securityUtilsMocked = mockStatic(org.apache.shiro.SecurityUtils.class)) {
            securityUtilsMocked.when(org.apache.shiro.SecurityUtils::getSubject).thenReturn(mockSubject);

            UserInfo result = JwtUtils.getUserInfo();
            assertThat(result, is(mockUserInfo));
        }
    }

    @Test
    public void testGenerateTokenWithTokenVersion() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId", 3L, EXPIRED_SECONDS);
        assertThat(JwtUtils.getTokenVersion(token), is(3L));
        assertThat(JwtUtils.verifyToken(token, KEY), is(true));
    }

    @Test
    public void testGenerateTokenWithoutTokenVersionIsLegacy() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(JwtUtils.getTokenVersion(token), is(JwtUtils.LEGACY_TOKEN_VERSION));
    }

    @Test
    public void testGetTokenVersionWithInvalidToken() {
        assertThrows(Exception.class, () -> JwtUtils.getTokenVersion("invalid.token"));
    }

    @Test
    public void testGetUserInfoWithNullPrincipal() {
        Subject mockSubject = mock(Subject.class);
        when(mockSubject.getPrincipal()).thenReturn(null);

        try (MockedStatic<org.apache.shiro.SecurityUtils> securityUtilsMocked = mockStatic(org.apache.shiro.SecurityUtils.class)) {
            securityUtilsMocked.when(org.apache.shiro.SecurityUtils::getSubject).thenReturn(mockSubject);

            UserInfo result = JwtUtils.getUserInfo();
            assertThat(result, nullValue());
        }
    }

    /**
     * assert the logs carry no reversible form of the token: neither the raw token, nor any of its
     * dot-separated segments, nor its Base64 encoding, nor the full SHA-256 digest.
     *
     * @param allLogs combined rendered log text
     * @param token token that must stay out of the logs
     */
    private void assertNoTokenLeak(final String allLogs, final String token) {
        assertThat(allLogs.contains(token), is(false));
        for (String segment : token.split("\\.")) {
            assertThat("JWT segment must not appear in logs", allLogs.contains(segment), is(false));
        }
        assertThat(allLogs.contains(Base64.getEncoder().encodeToString(token.getBytes(StandardCharsets.UTF_8))), is(false));
        assertThat(allLogs.contains(sha256Hex(token)), is(false));
    }

    /**
     * capture the logback events emitted on the security audit logger while running the given action.
     *
     * @param runnable action to run
     * @return captured logging events
     */
    private List<ILoggingEvent> captureJwtLogs(final Runnable runnable) {
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
}
