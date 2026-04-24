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

import jakarta.servlet.ServletRequestEvent;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.apache.shenyu.admin.listener.websocket.WebsocketListener.CLIENT_IP_NAME;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The TestCase for WebsocketListener.
 */
public class WebsocketListenerTest {

    private WebsocketListener websocketListener;

    @BeforeEach
    void setUp() {
        this.websocketListener = new WebsocketListener();
    }

    @Test
    void testRequestDestroyed() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpSession session = mock(HttpSession.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(session);

        websocketListener.requestDestroyed(sre);

        verify(request).removeAttribute(CLIENT_IP_NAME);
        verify(session).removeAttribute(CLIENT_IP_NAME);
        verify(request).removeAttribute(Constants.SHENYU_NAMESPACE_ID);
        verify(session).removeAttribute(Constants.SHENYU_NAMESPACE_ID);
        verify(request).removeAttribute(Constants.CLIENT_PORT_NAME);
        verify(session).removeAttribute(Constants.CLIENT_PORT_NAME);
    }

    @Test
    void testRequestDestroyedWhenSessionIsNull() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(null);

        // Should not throw; inner if-guard skips body
        websocketListener.requestDestroyed(sre);

        verify(request, never()).removeAttribute(CLIENT_IP_NAME);
    }

    @Test
    void testRequestDestroyedWhenExceptionThrown() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(sre.getServletRequest()).thenReturn(request);
        // getSession() throws RuntimeException, caught by try/catch in listener
        when(request.getSession()).thenThrow(new RuntimeException("session error"));

        // Should not propagate; exception is caught internally
        websocketListener.requestDestroyed(sre);
    }

    @Test
    void testRequestInitialized() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpSession session = mock(HttpSession.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(session);
        when(request.getRemoteAddr()).thenReturn("10.0.0.1");
        when(request.getHeader(Constants.SHENYU_NAMESPACE_ID)).thenReturn(null);

        websocketListener.requestInitialized(sre);

        // request.setAttribute(CLIENT_IP_NAME, ...) is called once directly on the request
        verify(request).setAttribute(CLIENT_IP_NAME, "10.0.0.1");
        verify(session).setAttribute(CLIENT_IP_NAME, "10.0.0.1");
        // namespace is null → no setAttribute for SHENYU_NAMESPACE_ID or CLIENT_PORT_NAME
        verify(request, never()).setAttribute(Constants.SHENYU_NAMESPACE_ID, null);
        verify(session, never()).setAttribute(Constants.SHENYU_NAMESPACE_ID, null);
    }

    @Test
    void testRequestInitializedWithNamespaceHeader() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpSession session = mock(HttpSession.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(session);
        when(request.getRemoteAddr()).thenReturn("10.0.0.2");
        when(request.getHeader(Constants.SHENYU_NAMESPACE_ID)).thenReturn("ns-abc");
        when(request.getHeader(Constants.CLIENT_PORT_NAME)).thenReturn("9090");

        websocketListener.requestInitialized(sre);

        verify(request).setAttribute(CLIENT_IP_NAME, "10.0.0.2");
        verify(session).setAttribute(CLIENT_IP_NAME, "10.0.0.2");
        verify(request).setAttribute(Constants.SHENYU_NAMESPACE_ID, "ns-abc");
        verify(session).setAttribute(Constants.SHENYU_NAMESPACE_ID, "ns-abc");
        verify(request).setAttribute(Constants.CLIENT_PORT_NAME, "9090");
        verify(session).setAttribute(Constants.CLIENT_PORT_NAME, "9090");
    }

    @Test
    void testRequestInitializedWhenSessionIsNull() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(null);

        // Inner if-guard skips the body; no setAttribute should be called
        websocketListener.requestInitialized(sre);

        verify(request, never()).setAttribute(CLIENT_IP_NAME, null);
    }

    @Test
    void testRequestInitializedWhenExceptionThrown() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(sre.getServletRequest()).thenReturn(request);
        // getSession() throws RuntimeException, caught by try/catch in listener
        when(request.getSession()).thenThrow(new RuntimeException("session error"));

        // Should not propagate; exception is caught internally
        websocketListener.requestInitialized(sre);
    }
}
