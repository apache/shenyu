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
import org.apache.shenyu.examples.websocket.service.SaveFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Controller
@ShenyuServerEndpoint("/upload")
public class UploadController {

    private static final Logger LOG = LoggerFactory.getLogger(UploadController.class);

    @Autowired
    private SaveFile saveFile;

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
     * @param session used for verify
     */
    @OnClose
    public void onClose(final Session session) {
        LOG.info("connect1 closed");
    }

    /**
     * receive message.
     * @param message  message content
     * @param session  session
     */
    @OnMessage
    public void onMessage(final String message, final Session session) {
        try {
            session.getBasicRemote().sendText("ok");
        } catch (IOException e) {
            LOG.error("UpLoadController onMessage error", e);
        }
    }

    /**
     * receive message of bytes.
     * @param message  message bytes
     * @param session  session
     */
    @OnMessage
    public void onMessage(final byte[] message, final Session session) {
        File file = (File) session.getUserProperties().get("file");
        try {
            Map<String, Object> param = new HashMap<>(2);
            param.put("file", file);
            saveFile.saveFileFromBytes(message, param);
            session.getBasicRemote().sendText("ok");
        } catch (Exception e) {
            LOG.error("UpLoadController onMessage", e);
        }
    }
}
