/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.admin.listener.websocket;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.spring.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * The type Websocket data changed listener.
 *
 * @author xiaoyu(Myth)
 * @author huangxiaofeng
 * @since 2.0.0
 */
@ServerEndpoint("/websocket")
public class WebsocketCollector {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketCollector.class);

    private static final Set<Session> SESSION_SET = new CopyOnWriteArraySet<>();

    @OnOpen
    public void onOpen(Session session) {
        SESSION_SET.add(session);
        SpringBeanUtils.getInstance().getBean(WebsocketDataChangedListener.class).onWebsocketConnect(session);
    }

    @OnClose
    public void onClose(final Session session) {
        SESSION_SET.remove(session);
    }

    @OnError
    public void onError(final Session session, final Throwable error) {
        SESSION_SET.remove(session);
        LOGGER.error("websocket collection error:{}", error);
    }

    protected static void send(final String message) {
        if (StringUtils.isNotBlank(message)) {
            for (Session session : SESSION_SET) {
                try {
                    session.getBasicRemote().sendText(message);
                } catch (IOException e) {
                    LOGGER.error("websocket send result is exception :{}", e);
                }
            }
        }
    }
}
