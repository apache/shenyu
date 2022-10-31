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

import org.junit.jupiter.api.Test;

import javax.servlet.http.HttpSession;
import javax.websocket.HandshakeResponse;
import javax.websocket.server.HandshakeRequest;
import javax.websocket.server.ServerEndpointConfig;

import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The TestCase for WebsocketConfigurator.
 */
public class WebsocketConfiguratorTest {

    @Test
    public void testModifyHandshake() {
        WebsocketConfigurator websocketConfigurator = new WebsocketConfigurator();
        ServerEndpointConfig sec = mock(ServerEndpointConfig.class);
        Map<String, Object> userProperties = mock(Map.class);
        when(sec.getUserProperties()).thenReturn(userProperties);
        HandshakeRequest request = mock(HandshakeRequest.class);
        HttpSession httpSession = mock(HttpSession.class);
        when(request.getHttpSession()).thenReturn(httpSession);
        HandshakeResponse response = mock(HandshakeResponse.class);
        websocketConfigurator.modifyHandshake(sec, request, response);
        verify(userProperties).put(WebsocketListener.CLIENT_IP_NAME, httpSession.getAttribute(WebsocketListener.CLIENT_IP_NAME));
    }
}
