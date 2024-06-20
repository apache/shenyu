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

package org.apache.shenyu.examples.websocket.ws;

import org.apache.shenyu.client.spring.websocket.annotation.ShenyuServerEndpoint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;

/**
 * WsServerEndpoint.
 */
@Component
@ShenyuServerEndpoint("/myWs")
public class WsServerEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(WsServerEndpoint.class);

    /**
     * connect successful.
     * @param session session
     */
    @OnOpen
    public void onOpen(final Session session) {
        LOG.info("connect successful");
    }

    /**
     * connect close.
     * @param session session
     */
    @OnClose
    public void onClose(final Session session) {
        LOG.info("connect closed");
    }

    /**
     * received message.
     * @param text message
     * @return response
     */
    @OnMessage
    public String onMsg(final String text) {
        return "server send message：" + text;
    }
}
