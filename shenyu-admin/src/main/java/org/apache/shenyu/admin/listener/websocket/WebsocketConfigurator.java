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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.context.annotation.Configuration;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpSession;
import javax.websocket.HandshakeResponse;
import javax.websocket.server.HandshakeRequest;
import javax.websocket.server.ServerEndpointConfig;

import static org.apache.tomcat.websocket.server.Constants.BINARY_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM;
import static org.apache.tomcat.websocket.server.Constants.TEXT_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM;

/**
 * The Websocket configurator.
 *
 * @since 2.0.0
 */
@ConditionalOnProperty(name = "shenyu.sync.websocket.enabled", havingValue = "true", matchIfMissing = true)
@Configuration
public class WebsocketConfigurator extends ServerEndpointConfig.Configurator implements ServletContextInitializer {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketConfigurator.class);

    @Autowired
    private WebsocketSyncProperties websocketSyncProperties;

    @Override
    public void modifyHandshake(final ServerEndpointConfig sec, final HandshakeRequest request, final HandshakeResponse response) {
        HttpSession httpSession = (HttpSession) request.getHttpSession();
        sec.getUserProperties().put(WebsocketListener.CLIENT_IP_NAME, httpSession.getAttribute(WebsocketListener.CLIENT_IP_NAME));
        super.modifyHandshake(sec, request, response);
    }

    @Override
    public boolean checkOrigin(final String originHeaderValue) {
        final WebsocketSyncProperties bean = SpringBeanUtils.getInstance().getBean(WebsocketSyncProperties.class);
        if (StringUtils.isNotEmpty(bean.getAllowOrigins())) {
            String[] split = StringUtils.split(bean.getAllowOrigins(), ";");
            for (String configAllow : split) {
                if (StringUtils.equals(configAllow, originHeaderValue)) {
                    return true;
                }
            }
            LOG.error("originHeaderValue is forbidden: " + originHeaderValue);
            return false;
        }
        return super.checkOrigin(originHeaderValue);
    }

    @Override
    public void onStartup(final ServletContext servletContext) throws ServletException {
        int messageMaxSize = websocketSyncProperties.getMessageMaxSize();
        if (messageMaxSize > 0) {
            servletContext.setInitParameter(TEXT_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM,
                    String.valueOf(messageMaxSize));
            servletContext.setInitParameter(BINARY_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM,
                    String.valueOf(messageMaxSize));
        }
    }
}
