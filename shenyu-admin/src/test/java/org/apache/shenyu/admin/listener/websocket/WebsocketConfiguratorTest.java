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

package org.apache.shenyu.admin.listener.websocket;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpSession;
import jakarta.websocket.HandshakeResponse;
import jakarta.websocket.server.HandshakeRequest;
import jakarta.websocket.server.ServerEndpointConfig;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.apache.tomcat.websocket.server.Constants.BINARY_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM;
import static org.apache.tomcat.websocket.server.Constants.TEXT_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The TestCase for WebsocketConfigurator.
 */
public class WebsocketConfiguratorTest {

    private WebsocketConfigurator websocketConfigurator;

    private WebsocketSyncProperties websocketSyncProperties;

    @BeforeEach
    void setUp() {
        websocketConfigurator = new WebsocketConfigurator();
        websocketSyncProperties = new WebsocketSyncProperties();
        ReflectionTestUtils.setField(websocketConfigurator, "websocketSyncProperties", websocketSyncProperties);

        ApplicationContext applicationContext = mock(ApplicationContext.class);
        when(applicationContext.getBean(WebsocketSyncProperties.class)).thenReturn(websocketSyncProperties);
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
    }

    @AfterEach
    void tearDown() {
        MDC.clear();
        SecurityAuditLog.resetRateLimiter();
    }

    @Test
    void testModifyHandshake() {
        websocketSyncProperties.setToken("websocket-sync-token");
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> userProperties = new HashMap<>();
        when(sec.getUserProperties()).thenReturn(userProperties);

        HandshakeRequest request = mock(HandshakeRequest.class);
        when(request.getHeaders()).thenReturn(Collections.singletonMap(Constants.X_SHENYU_SYNC_TOKEN, List.of("websocket-sync-token")));
        HttpSession httpSession = mock(HttpSession.class);
        when(request.getHttpSession()).thenReturn(httpSession);
        when(httpSession.getAttribute(WebsocketListener.CLIENT_IP_NAME)).thenReturn("192.168.1.1");
        when(httpSession.getAttribute(Constants.CLIENT_PORT_NAME)).thenReturn("8080");
        when(httpSession.getAttribute(Constants.SHENYU_NAMESPACE_ID)).thenReturn("ns-123");

        HandshakeResponse response = mock(HandshakeResponse.class);
        websocketConfigurator.modifyHandshake(sec, request, response);

        assertEquals("192.168.1.1", userProperties.get(WebsocketListener.CLIENT_IP_NAME));
        assertEquals("8080", userProperties.get(Constants.CLIENT_PORT_NAME));
        assertEquals("ns-123", userProperties.get(Constants.SHENYU_NAMESPACE_ID));
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"compact-v1", "compact-v2", "", "true", "compact-v1,compact-v2"})
    void testDiscoveryCapabilityRequiresExactKnownVersion(final String capability) {
        websocketSyncProperties.setToken("websocket-sync-token");
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> properties = new HashMap<>();
        properties.put(Constants.X_SHENYU_DISCOVERY_PAYLOAD, Boolean.TRUE);
        when(sec.getUserProperties()).thenReturn(properties);
        HandshakeRequest request = mock(HandshakeRequest.class);
        Map<String, List<String>> headers = new HashMap<>();
        headers.put(Constants.X_SHENYU_SYNC_TOKEN, List.of("websocket-sync-token"));
        if (Objects.nonNull(capability)) {
            headers.put("x-shenyu-discovery-payload", List.of(capability));
        }
        when(request.getHeaders()).thenReturn(headers);
        when(request.getHttpSession()).thenReturn(mock(HttpSession.class));

        websocketConfigurator.modifyHandshake(sec, request, mock(HandshakeResponse.class));

        assertEquals("compact-v1".equals(capability), properties.get(Constants.X_SHENYU_DISCOVERY_PAYLOAD));
    }

    @Test
    void testModifyHandshakePutsAllAttributes() {
        websocketSyncProperties.setToken("websocket-sync-token");
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> userProperties = new HashMap<>();
        when(sec.getUserProperties()).thenReturn(userProperties);

        HandshakeRequest request = mock(HandshakeRequest.class);
        when(request.getHeaders()).thenReturn(Collections.singletonMap(Constants.X_SHENYU_SYNC_TOKEN, List.of("websocket-sync-token")));
        HttpSession httpSession = mock(HttpSession.class);
        when(request.getHttpSession()).thenReturn(httpSession);

        HandshakeResponse response = mock(HandshakeResponse.class);
        websocketConfigurator.modifyHandshake(sec, request, response);

        verify(httpSession).getAttribute(WebsocketListener.CLIENT_IP_NAME);
        verify(httpSession).getAttribute(Constants.CLIENT_PORT_NAME);
        verify(httpSession).getAttribute(Constants.SHENYU_NAMESPACE_ID);
        assertTrue(userProperties.containsKey(WebsocketListener.CLIENT_IP_NAME));
        assertTrue(userProperties.containsKey(Constants.CLIENT_PORT_NAME));
        assertTrue(userProperties.containsKey(Constants.SHENYU_NAMESPACE_ID));
    }

    @Test
    void testModifyHandshakeUsesSpringBeanWhenConfiguratorIsNotAutowired() {
        websocketSyncProperties.setToken("websocket-sync-token");
        ReflectionTestUtils.setField(websocketConfigurator, "websocketSyncProperties", null);
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> userProperties = new HashMap<>();
        when(sec.getUserProperties()).thenReturn(userProperties);

        HandshakeRequest request = mock(HandshakeRequest.class);
        when(request.getHeaders()).thenReturn(Collections.singletonMap(Constants.X_SHENYU_SYNC_TOKEN, List.of("websocket-sync-token")));
        when(request.getHttpSession()).thenReturn(mock(HttpSession.class));

        HandshakeResponse response = mock(HandshakeResponse.class);
        websocketConfigurator.modifyHandshake(sec, request, response);

        assertTrue(userProperties.containsKey(WebsocketListener.CLIENT_IP_NAME));
        assertTrue(userProperties.containsKey(Constants.CLIENT_PORT_NAME));
        assertTrue(userProperties.containsKey(Constants.SHENYU_NAMESPACE_ID));
    }

    @Test
    void testModifyHandshakeRejectsMissingToken() {
        websocketSyncProperties.setToken("websocket-sync-token");
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        HandshakeRequest request = mock(HandshakeRequest.class);
        when(request.getHeaders()).thenReturn(Collections.emptyMap());
        HandshakeResponse response = mock(HandshakeResponse.class);

        assertThrows(ShenyuException.class, () -> websocketConfigurator.modifyHandshake(sec, request, response));
    }

    @Test
    void testModifyHandshakeRejectsInvalidToken() {
        websocketSyncProperties.setToken("websocket-sync-token");
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        HandshakeRequest request = mock(HandshakeRequest.class);
        when(request.getHeaders()).thenReturn(Collections.singletonMap(Constants.X_SHENYU_SYNC_TOKEN, List.of("invalid-token")));
        HandshakeResponse response = mock(HandshakeResponse.class);

        assertThrows(ShenyuException.class, () -> websocketConfigurator.modifyHandshake(sec, request, response));
    }

    @Test
    void testModifyHandshakeRejectsBlankConfiguredToken() {
        websocketSyncProperties.setToken("");
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        HandshakeRequest request = mock(HandshakeRequest.class);
        when(request.getHeaders()).thenReturn(Collections.singletonMap(Constants.X_SHENYU_SYNC_TOKEN, List.of("websocket-sync-token")));
        HandshakeResponse response = mock(HandshakeResponse.class);

        assertThrows(ShenyuException.class, () -> websocketConfigurator.modifyHandshake(sec, request, response));
    }

    @Test
    void testCheckOriginAllowedWhenAllowOriginsEmpty() {
        websocketSyncProperties.setAllowOrigins("");
        // empty allowOrigins → delegates to super (always true for any origin)
        assertTrue(websocketConfigurator.checkOrigin("http://any-origin.com"));
    }

    @Test
    void testCheckOriginAllowedWhenAllowOriginsNull() {
        websocketSyncProperties.setAllowOrigins(null);
        assertTrue(websocketConfigurator.checkOrigin("http://any-origin.com"));
    }

    @Test
    void testCheckOriginAllowedWhenOriginMatches() {
        websocketSyncProperties.setAllowOrigins("http://allowed.com;http://other.com");
        assertTrue(websocketConfigurator.checkOrigin("http://allowed.com"));
    }

    @Test
    void testCheckOriginAllowedWhenSecondOriginMatches() {
        websocketSyncProperties.setAllowOrigins("http://first.com;http://second.com");
        assertTrue(websocketConfigurator.checkOrigin("http://second.com"));
    }

    @Test
    void testCheckOriginForbiddenWhenNoMatch() {
        websocketSyncProperties.setAllowOrigins("http://allowed.com;http://other.com");
        assertFalse(websocketConfigurator.checkOrigin("http://forbidden.com"));
    }

    @Test
    void testOnStartupWithPositiveMessageMaxSize() {
        websocketSyncProperties.setMessageMaxSize(65536);
        ServletContext servletContext = mock(ServletContext.class);

        websocketConfigurator.onStartup(servletContext);

        verify(servletContext).setInitParameter(TEXT_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM, "65536");
        verify(servletContext).setInitParameter(BINARY_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM, "65536");
    }

    @Test
    void testOnStartupWithZeroMessageMaxSize() {
        websocketSyncProperties.setMessageMaxSize(0);
        ServletContext servletContext = mock(ServletContext.class);

        websocketConfigurator.onStartup(servletContext);

        // messageMaxSize <= 0 → no setInitParameter calls
        verify(servletContext, org.mockito.Mockito.never())
                .setInitParameter(org.mockito.Mockito.anyString(), org.mockito.Mockito.anyString());
    }

    @Test
    void testOnStartupWithNegativeMessageMaxSize() {
        websocketSyncProperties.setMessageMaxSize(-1);
        ServletContext servletContext = mock(ServletContext.class);

        websocketConfigurator.onStartup(servletContext);

        verify(servletContext, org.mockito.Mockito.never())
                .setInitParameter(org.mockito.Mockito.anyString(), org.mockito.Mockito.anyString());
    }

    @Test
    void testSyncTokenDenialsEmitFixedReasonAuditWithoutTokenLeak() {
        final String configuredToken = "configured-token-8179-do-not-log";
        final String suppliedToken = "supplied-token-8179-do-not-log";
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            websocketSyncProperties.setToken(" ");
            ShenyuException notConfigured = assertThrows(ShenyuException.class, () -> websocketConfigurator
                    .modifyHandshake(mock(ServerEndpointConfig.class), mockHandshakeRequest(null, null), mock(HandshakeResponse.class)));
            assertEquals("websocket sync token is not configured", notConfigured.getMessage());
            websocketSyncProperties.setToken(configuredToken);
            ShenyuException missing = assertThrows(ShenyuException.class, () -> websocketConfigurator
                    .modifyHandshake(mock(ServerEndpointConfig.class), mockHandshakeRequest(null, "10.0.0.9"), mock(HandshakeResponse.class)));
            assertEquals("websocket sync token is invalid", missing.getMessage());
            ShenyuException invalid = assertThrows(ShenyuException.class, () -> websocketConfigurator
                    .modifyHandshake(mock(ServerEndpointConfig.class), mockHandshakeRequest(suppliedToken, "10.0.0.9"), mock(HandshakeResponse.class)));
            assertEquals("websocket sync token is invalid", invalid.getMessage());
        });
        assertEquals(3, events.size());
        String first = events.get(0).getFormattedMessage();
        assertTrue(first.contains("eventType=websocket_sync_auth"), first);
        assertTrue(first.contains("outcome=denied"), first);
        assertTrue(first.contains("action=websocket_sync_auth"), first);
        assertTrue(first.contains("reason=sync_token_not_configured"), first);
        assertTrue(first.contains("actor=unknown"), first);
        String second = events.get(1).getFormattedMessage();
        assertTrue(second.contains("reason=missing_sync_token"), second);
        assertTrue(second.contains("resource=/websocket"), second);
        assertTrue(second.contains("remoteAddr=10.0.0.9"), second);
        String third = events.get(2).getFormattedMessage();
        assertTrue(third.contains("reason=invalid_sync_token"), third);
        for (ILoggingEvent event : events) {
            String message = event.getFormattedMessage();
            assertFalse(message.contains(configuredToken), "configured token must not appear in audit logs");
            assertFalse(message.contains(suppliedToken), "supplied token must not appear in audit logs");
        }
    }

    @Test
    void testOriginForbiddenEmitsFixedReasonAuditWithoutOriginValue() {
        final String sentinelOrigin = "http://sentinel-origin-8179.attacker.example";
        websocketSyncProperties.setAllowOrigins("http://allowed.example");
        ch.qos.logback.classic.Logger configuratorLogger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(WebsocketConfigurator.class);
        ListAppender<ILoggingEvent> classAppender = new ListAppender<>();
        classAppender.start();
        configuratorLogger.addAppender(classAppender);
        final List<ILoggingEvent> events;
        try {
            events = captureAuditLogs(() -> assertFalse(websocketConfigurator.checkOrigin(sentinelOrigin)));
        } finally {
            configuratorLogger.detachAppender(classAppender);
        }
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("eventType=websocket_sync_auth"), message);
        assertTrue(message.contains("outcome=denied"), message);
        assertTrue(message.contains("action=websocket_origin_check"), message);
        assertTrue(message.contains("reason=origin_forbidden"), message);
        assertTrue(message.contains("resource=/websocket"), message);
        assertFalse(message.contains("sentinel-origin-8179"), "raw origin value must not appear in audit logs");
        assertTrue(classAppender.list.isEmpty(), "the forbidden origin must no longer be written to the class logger");
    }

    @Test
    void testPassingHandshakeChecksEmitNoAudit() {
        final String configuredToken = "configured-token-8179";
        websocketSyncProperties.setToken(configuredToken);
        websocketSyncProperties.setAllowOrigins("http://allowed.example");
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            assertTrue(websocketConfigurator.checkOrigin("http://allowed.example"));
            ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
            when(sec.getUserProperties()).thenReturn(new HashMap<>());
            websocketConfigurator.modifyHandshake(sec, mockHandshakeRequest(configuredToken, null), mock(HandshakeResponse.class));
        });
        assertTrue(events.isEmpty(), "passing handshake checks must not emit denial audit events");
    }

    @Test
    void testDenialAuditUsesMdcRequestIdOrPlaceholder() {
        websocketSyncProperties.setToken(" ");
        MDC.put(SecurityAuditLog.REQUEST_ID_MDC_KEY, "req-8179-websocket");
        List<ILoggingEvent> withRequestId = captureAuditLogs(() -> assertThrows(ShenyuException.class, () -> websocketConfigurator
                .modifyHandshake(mock(ServerEndpointConfig.class), mockHandshakeRequest(null, null), mock(HandshakeResponse.class))));
        assertTrue(withRequestId.get(0).getFormattedMessage().contains("requestId=req-8179-websocket"), withRequestId.get(0).getFormattedMessage());
        MDC.clear();
        List<ILoggingEvent> withoutRequestId = captureAuditLogs(() -> assertThrows(ShenyuException.class, () -> websocketConfigurator
                .modifyHandshake(mock(ServerEndpointConfig.class), mockHandshakeRequest(null, null), mock(HandshakeResponse.class))));
        assertTrue(withoutRequestId.get(0).getFormattedMessage().contains("requestId=no-request"), withoutRequestId.get(0).getFormattedMessage());
    }

    /**
     * build a handshake request mock with the given sync token header and client ip session attribute.
     *
     * @param token sync token header value, null for a request without the header
     * @param remoteAddr value of the ClientIP session attribute, null when absent
     * @return mocked handshake request
     */
    private HandshakeRequest mockHandshakeRequest(final String token, final String remoteAddr) {
        HandshakeRequest request = mock(HandshakeRequest.class);
        if (Objects.nonNull(token)) {
            when(request.getHeaders()).thenReturn(Collections.singletonMap(Constants.X_SHENYU_SYNC_TOKEN, List.of(token)));
        } else {
            when(request.getHeaders()).thenReturn(Collections.emptyMap());
        }
        when(request.getRequestURI()).thenReturn(URI.create("/websocket"));
        HttpSession session = mock(HttpSession.class);
        when(session.getAttribute(WebsocketListener.CLIENT_IP_NAME)).thenReturn(remoteAddr);
        when(request.getHttpSession()).thenReturn(session);
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
