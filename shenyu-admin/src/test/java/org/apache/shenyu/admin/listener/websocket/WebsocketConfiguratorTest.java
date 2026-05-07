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

import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpSession;
import jakarta.websocket.HandshakeResponse;
import jakarta.websocket.server.HandshakeRequest;
import jakarta.websocket.server.ServerEndpointConfig;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.apache.tomcat.websocket.server.Constants.BINARY_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM;
import static org.apache.tomcat.websocket.server.Constants.TEXT_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

    @Test
    void testModifyHandshake() {
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> userProperties = new HashMap<>();
        when(sec.getUserProperties()).thenReturn(userProperties);

        HandshakeRequest request = mock(HandshakeRequest.class);
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

    @Test
    void testModifyHandshakePutsAllAttributes() {
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> userProperties = new HashMap<>();
        when(sec.getUserProperties()).thenReturn(userProperties);

        HandshakeRequest request = mock(HandshakeRequest.class);
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
}
