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

import io.undertow.Undertow;
import io.undertow.websockets.core.AbstractReceiveListener;
import io.undertow.websockets.core.BufferedTextMessage;
import io.undertow.websockets.core.WebSocketChannel;
import io.undertow.websockets.core.WebSockets;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.enums.ReadyState;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static io.undertow.Handlers.path;
import static io.undertow.Handlers.websocket;

/**
 * @author xiaoyu(Myth)
 */
public class WebsocketClientTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketClientTest.class);
    public static WebSocketClient client;
    private static Undertow server;

    @BeforeClass
    public static void init() {
        server = Undertow.builder()
                .addHttpListener(8888, "localhost")
                .setHandler(path()
                        .addPrefixPath("/websocket", websocket((exchange, channel) -> {
                            channel.getReceiveSetter().set(new AbstractReceiveListener() {

                                @Override
                                protected void onFullTextMessage(WebSocketChannel channel, BufferedTextMessage message) {
                                    WebSockets.sendText(message.getData(), channel, null);
                                }
                            });
                            channel.resumeReceives();
                        })))
                .build();
        server.start();
    }

    @AfterClass
    public static void after() {
        server.stop();
    }

    @After
    public void destroy() {
        client.close();
    }

    @Test
    public void send() throws Exception {
        final CountDownLatch latch = new CountDownLatch(1);
        client = new WebSocketClient(new URI("ws://localhost:8888/websocket")) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {
                LOGGER.info("Open connection");
            }

            @Override
            public void onMessage(final String s) {
                LOGGER.info("Received {}", s);
                latch.countDown();
            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {
                LOGGER.info("Close connection");
            }

            @Override
            public void onError(final Exception e) {
                LOGGER.error("", e);
            }
        };
        client.connect();
        while (!client.getReadyState().equals(ReadyState.OPEN)) {
            LOGGER.debug("connecting...");
        }
        client.send("xiaoyu");
        latch.await(10, TimeUnit.SECONDS);
    }

}
