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


import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.examples.websocket.service.SaveFile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;

@ShenyuSpringWebSocketClient("/upload")
@ServerEndpoint("/upload")
@Controller
public class UpLoadController {

    @Autowired
    private SaveFile saveFile;

    @OnOpen
    public void onOpen(Session session) {
        System.out.println("connect1 successful");
    }

    /**
     * connect close.
     *
     * @param session
     */
    @OnClose
    public void onClose(Session session) {
        System.out.println("connect1 closed");
    }

    @OnMessage
    public void onMessage(String message, Session session) {
        try {
            session.getBasicRemote().sendText("ok");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @OnMessage
    public void onMessage(byte[] message, Session session) {
        File file = (File) session.getUserProperties().get("file");
        try {
            HashMap map = new HashMap();
            map.put("file", file);
            saveFile.saveFileFromBytes(message, map);
            session.getBasicRemote().sendText("ok");
        } catch (Exception e) {

        }
    }
}
