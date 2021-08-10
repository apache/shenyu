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

import org.junit.Before;
import org.junit.Test;

import javax.servlet.ServletRequestEvent;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import static org.apache.shenyu.admin.listener.websocket.WebsocketListener.CLIENT_IP_NAME;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The TestCase for WebsocketListener.
 */
public class WebsocketListenerTest {

    private WebsocketListener websocketListener;

    @Before
    public void setUp() {
        this.websocketListener = new WebsocketListener();
    }

    @Test
    public void testRequestDestroyed() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpSession session = mock(HttpSession.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(session);
        websocketListener.requestDestroyed(sre);
        verify(request).removeAttribute(CLIENT_IP_NAME);
        verify(session).removeAttribute(CLIENT_IP_NAME);
    }

    @Test
    public void testRequestInitialized() {
        ServletRequestEvent sre = mock(ServletRequestEvent.class);
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpSession session = mock(HttpSession.class);
        when(sre.getServletRequest()).thenReturn(request);
        when(request.getSession()).thenReturn(session);
        websocketListener.requestInitialized(sre);
        // For session will invoke request one more time.
        verify(request, times(2)).setAttribute(CLIENT_IP_NAME, sre.getServletRequest().getRemoteAddr());
        verify(session).setAttribute(CLIENT_IP_NAME, sre.getServletRequest().getRemoteAddr());
    }
}
