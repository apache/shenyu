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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;

import javax.servlet.ServletRequestEvent;
import javax.servlet.ServletRequestListener;
import javax.servlet.annotation.WebListener;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.Objects;

/**
 * The Websocket listener.
 *
 * @since 2.0.0
 */
@WebListener
@Configuration
public class WebsocketListener implements ServletRequestListener {

    public static final String CLIENT_IP_NAME = "ClientIP";

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketListener.class);

    @Override
    public void requestDestroyed(final ServletRequestEvent sre) {
        try {
            HttpServletRequest request = (HttpServletRequest) sre.getServletRequest();
            if (Objects.nonNull(request) && Objects.nonNull(request.getSession())) {
                HttpSession session = request.getSession();
                request.removeAttribute(CLIENT_IP_NAME);
                session.removeAttribute(CLIENT_IP_NAME);
            }
        } catch (Exception e) {
            LOG.error("request destroyed error", e);
        }
    }

    @Override
    public void requestInitialized(final ServletRequestEvent sre) {
        try {
            HttpServletRequest request = (HttpServletRequest) sre.getServletRequest();
            if (Objects.nonNull(request) && Objects.nonNull(request.getSession())) {
                HttpSession session = request.getSession();
                request.setAttribute(CLIENT_IP_NAME, sre.getServletRequest().getRemoteAddr());
                session.setAttribute(CLIENT_IP_NAME, sre.getServletRequest().getRemoteAddr());
            }
        } catch (Exception e) {
            LOG.error("request initialized error", e);
        }
    }
}
