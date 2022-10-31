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

package org.apache.shenyu.examples.websocket.controller;

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.examples.common.aop.Log;
import org.apache.shenyu.examples.websocket.config.WsSessionManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;

/**
 * TestController.
 */
@Controller
@RequestMapping("/ws")
@ShenyuSpringMvcClient("/ws/**")
public class TestHttpController {

    private static final Logger LOG = LoggerFactory.getLogger(TestHttpController.class);
    
    /**
     * Send message.
     * @param token session key
     * @param msg text-type message
     * @return response
     */
    @RequestMapping("/sendMsg")
    @Log
    @ResponseBody
    public String sendMsg(final String token, final String msg) {
        WebSocketSession webSocketSession = WsSessionManager.get(token);
        if (webSocketSession == null) {
            return "User login has expired";
        }
        try {
            webSocketSession.sendMessage(new TextMessage(msg));
        } catch (IOException e) {
            LOG.error("throw exception when sending message.", e);
        }
        return "Message sent successfully";
    }

    /**
     * Upload files.
     * @param token session key
     * @param file file
     * @return response
     */
    @PostMapping(value = "/upload")
    @Log
    @ResponseBody
    public String file(final String token, @RequestParam("file") final MultipartFile file) {
        try {
            WebSocketSession webSocketSession = WsSessionManager.get(token);
            if (webSocketSession == null) {
                return "User login has expired";
            }
            if (file.getOriginalFilename().endsWith(".bin")) {
                webSocketSession.sendMessage(new BinaryMessage(file.getBytes()));
            }
        } catch (Exception e) {
            // ignore exception
        }
        return "ok";
    }
}
