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

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ThreadLocalUtils;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * The type Websocket data changed listener.
 *
 * @since 2.0.0
 */
@ServerEndpoint(value = "/websocket", configurator = WebsocketConfigurator.class)
public class WebsocketCollector {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketCollector.class);

    private static final Set<Session> SESSION_SET = new CopyOnWriteArraySet<>();

    private static final String SESSION_KEY = "sessionKey";

    /**
     * On open.
     *
     * @param session the session
     */
    @OnOpen
    public void onOpen(final Session session) {
        LOG.info("websocket on client[{}] open successful....", getClientIp(session));
        SESSION_SET.add(session);
    }

    private static String getClientIp(final Session session) {
        Map<String, Object> userProperties = session.getUserProperties();
        if (MapUtils.isEmpty(userProperties)) {
            return StringUtils.EMPTY;
        }
        Object ipObject = userProperties.get(WebsocketListener.CLIENT_IP_NAME);
        if (null == ipObject) {
            return StringUtils.EMPTY;
        }
        return ipObject.toString();
    }

    /**
     * On message.
     *
     * @param message the message
     * @param session the session
     */
    @OnMessage
    public void onMessage(final String message, final Session session) {
        if (message.equals(DataEventTypeEnum.MYSELF.name())) {
            try {
                ThreadLocalUtils.put(SESSION_KEY, session);
                SpringBeanUtils.getInstance().getBean(SyncDataService.class).syncAll(DataEventTypeEnum.MYSELF);
            } finally {
                ThreadLocalUtils.clear();
            }
        }
    }

    /**
     * On close.
     *
     * @param session the session
     */
    @OnClose
    public void onClose(final Session session) {
        SESSION_SET.remove(session);
        ThreadLocalUtils.clear();
        LOG.warn("websocket close on client[{}]", getClientIp(session));
    }

    /**
     * On error.
     *
     * @param session the session
     * @param error   the error
     */
    @OnError
    public void onError(final Session session, final Throwable error) {
        SESSION_SET.remove(session);
        ThreadLocalUtils.clear();
        LOG.error("websocket collection on client[{}] error: ", getClientIp(session), error);
    }

    /**
     * Send.
     *
     * @param message the message
     * @param type    the type
     */
    public static void send(final String message, final DataEventTypeEnum type) {
        if (StringUtils.isNotBlank(message)) {
            if (DataEventTypeEnum.MYSELF == type) {
                Session session = (Session) ThreadLocalUtils.get(SESSION_KEY);
                if (session != null) {
                    sendMessageBySession(session, message);
                }
            } else {
                SESSION_SET.forEach(session -> sendMessageBySession(session, message));
            }
        }
    }

    private static void sendMessageBySession(final Session session, final String message) {
        try {
            session.getBasicRemote().sendText(message);
        } catch (IOException e) {
            LOG.error("websocket send result is exception: ", e);
        }
    }
}
