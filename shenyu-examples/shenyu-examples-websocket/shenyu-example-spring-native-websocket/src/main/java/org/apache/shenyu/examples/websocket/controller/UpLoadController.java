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


import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.examples.websocket.config.WsSessionManager;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.socket.BinaryMessage;
import org.springframework.web.socket.WebSocketSession;

@RestController
@RequestMapping("/ws")
@ShenyuSpringWebSocketClient("/ws/**")
public class UpLoadController {

    @PostMapping(value = "/file")
    public Object file(String token,@RequestParam("file") final MultipartFile file) {
        try {
            WebSocketSession webSocketSession = WsSessionManager.get(token);
            if (webSocketSession == null) {
                return "User login has expired";
            }
            if (file.getOriginalFilename().endsWith(".bin")){
                webSocketSession.sendMessage(new BinaryMessage(file.getBytes()));
            }
        }catch (Exception e){

        }
        return file.getOriginalFilename();
    }
}
