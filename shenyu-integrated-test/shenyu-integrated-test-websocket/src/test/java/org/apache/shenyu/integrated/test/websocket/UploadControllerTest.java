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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class UploadControllerTest extends AbstractPluginDataInit {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketPluginTest.class);

    private static final String WEBSOCKET_URI = "ws://localhost:9195/ws-annotation/upload?token=Jack";

    @Test
    public void testWebsocketUpLoad() throws InterruptedException, URISyntaxException {

        WebSocketClient webSocketClient = new WebSocketClient(new URI(WEBSOCKET_URI)) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {

            }

            @Override
            public void onMessage(final String s) {

            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {

            }

            @Override
            public void onError(final Exception e) {

            }
        };
        byte[] msg = new byte[16];
        msg[0] = 1;
        webSocketClient.connectBlocking();
        webSocketClient.send(msg);
        ArrayBlockingQueue<String> blockingQueue = new ArrayBlockingQueue<>(1);
        String receivedMessage = blockingQueue.poll(10, TimeUnit.SECONDS);
        assertThat(receivedMessage, is("apache shenyu server send to Jack message : -> " + msg));
    }
}
