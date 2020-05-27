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

package org.dromara.soul.sync.data.websocket;


import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * @author xiaoyu(Myth)
 */
public class WebsocketClientTest {
    
    public static WebSocketClient client;
    
    @Before
    public void setUp() {
        try {
            client = new WebSocketClient(new URI("ws://localhost:8888/websocket")) {
                @Override
                public void onOpen(ServerHandshake serverHandshake) {
                    System.out.println("打开链接");
                }
                
                @Override
                public void onMessage(String s) {
                    System.out.println("收到消息" + s);
                }
                
                @Override
                public void onClose(int i, String s, boolean b) {
                    System.out.println("链接已关闭");
                }
                
                @Override
                public void onError(Exception e) {
                    e.printStackTrace();
                    System.out.println("发生错误已关闭");
                }
            };
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        client.connect();
        
    }
    
    @Test
    public void send() {
        client.send("xiaoyu");
    }
    
}
