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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.context.annotation.Configuration;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

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
        checkSyncToken(request);
        HttpSession httpSession = (HttpSession) request.getHttpSession();
        sec.getUserProperties().put(WebsocketListener.CLIENT_IP_NAME, httpSession.getAttribute(WebsocketListener.CLIENT_IP_NAME));
        sec.getUserProperties().put(Constants.CLIENT_PORT_NAME, httpSession.getAttribute(Constants.CLIENT_PORT_NAME));
        sec.getUserProperties().put(Constants.SHENYU_NAMESPACE_ID, httpSession.getAttribute(Constants.SHENYU_NAMESPACE_ID));
        super.modifyHandshake(sec, request, response);
    }

    @Override
    public boolean checkOrigin(final String originHeaderValue) {
        final WebsocketSyncProperties bean = getWebsocketSyncProperties();
        if (StringUtils.isNotEmpty(bean.getAllowOrigins())) {
            String[] split = StringUtils.split(bean.getAllowOrigins(), ";");
            for (String configAllow : split) {
                if (StringUtils.equals(configAllow, originHeaderValue)) {
                    return true;
                }
            }
            LOG.error("originHeaderValue is forbidden: {}", originHeaderValue);
            return false;
        }
        return super.checkOrigin(originHeaderValue);
    }

    @Override
    public void onStartup(final ServletContext servletContext) {
        int messageMaxSize = getWebsocketSyncProperties().getMessageMaxSize();
        if (messageMaxSize > 0) {
            servletContext.setInitParameter(TEXT_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM,
                    String.valueOf(messageMaxSize));
            servletContext.setInitParameter(BINARY_BUFFER_SIZE_SERVLET_CONTEXT_INIT_PARAM,
                    String.valueOf(messageMaxSize));
        }
    }

    private void checkSyncToken(final HandshakeRequest request) {
        String configuredToken = getWebsocketSyncProperties().getToken();
        if (StringUtils.isBlank(configuredToken)) {
            throw new ShenyuException("websocket sync token is not configured");
        }
        String requestToken = getHeader(request.getHeaders(), Constants.X_SHENYU_SYNC_TOKEN);
        if (StringUtils.isBlank(requestToken) || !isSameToken(configuredToken, requestToken)) {
            throw new ShenyuException("websocket sync token is invalid");
        }
    }

    private WebsocketSyncProperties getWebsocketSyncProperties() {
        return Optional.ofNullable(websocketSyncProperties)
                .orElseGet(() -> SpringBeanUtils.getInstance().getBean(WebsocketSyncProperties.class));
    }

    private boolean isSameToken(final String configuredToken, final String requestToken) {
        return MessageDigest.isEqual(
                configuredToken.getBytes(StandardCharsets.UTF_8),
                requestToken.getBytes(StandardCharsets.UTF_8));
    }

    private String getHeader(final Map<String, List<String>> headers, final String name) {
        return Optional.ofNullable(headers)
                .orElse(Collections.emptyMap())
                .entrySet()
                .stream()
                .filter(entry -> StringUtils.equalsIgnoreCase(entry.getKey(), name))
                .map(Map.Entry::getValue)
                .filter(values -> !values.isEmpty())
                .map(values -> values.get(0))
                .findFirst()
                .orElse(null);
    }
}
