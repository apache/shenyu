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

package org.apache.shenyu.integrated.test.websocket;

import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class WebsocketPluginTest extends AbstractPluginDataInit {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketPluginTest.class);

    private static final String WEBSOCKET_URI = "ws://localhost:9195/ws-native/myWebSocket?token=Jack";

    @Test
    public void testWebsocket() throws URISyntaxException, InterruptedException {
        final String sendMessage = "Shenyu says hello to you!";
        ArrayBlockingQueue<String> blockingQueue = new ArrayBlockingQueue<>(1);
        WebSocketClient webSocketClient = new WebSocketClient(new URI(WEBSOCKET_URI)) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {
            }

            @Override
            public void onMessage(final String s) {
                LOG.info("websocket client received message : {}", s);
                blockingQueue.add(s);
            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {
            }

            @Override
            public void onError(final Exception e) {
            }
        };
        webSocketClient.connectBlocking();
        webSocketClient.send(sendMessage);
        String receivedMessage = blockingQueue.poll(10, TimeUnit.SECONDS);
        assertThat(receivedMessage, is("apache shenyu server send to Jack message : -> " + sendMessage));
    }

}
