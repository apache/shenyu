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

package org.apache.shenyu.e2e.engine.scenario.function;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.exceptions.WebsocketNotConnectedException;
import org.junit.jupiter.api.Assertions;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Optional;

/**
 * Check if the endpoint exists.
 */
public class WebSocketCheckers {

    /**
     * exist endpoint.
     * @param endpoint endpoint
     * @param sendMessage sendMessage
     * @param receiveMessage receiveMessage
     * @return WebSocketChecker
     */
    public static WebSocketChecker exists(final String endpoint, final String sendMessage, final String receiveMessage) {
        return (websocketClient, gatewayClient) -> {
            try {
                updateWebSocketClientURI(websocketClient, endpoint);

                websocketClient.connectBlocking();
                websocketClient.send(sendMessage);
                Optional.of(websocketClient)
                        .filter(WebSocketClient::isOpen)
                        .ifPresent(client -> Assertions.assertEquals(receiveMessage, gatewayClient.getWebSocketMessage()));
            } catch (AssertionError | InterruptedException | RuntimeException error) {
                Assertions.fail("websocket endpoint '" + websocketClient.getURI() + "' not exists", error);
            } catch (NoSuchFieldException | IllegalAccessException | URISyntaxException e) {
                throw new RuntimeException(e);
            }
        };
    }

    /**
     * not exist endpoint.
     * @param endpoint endpoint
     * @param message message
     * @return WebSocketChecker
     */
    public static WebSocketChecker notExists(final String endpoint, final String message) {
        return (websocketClient, gatewayClient) -> {
            try {
                updateWebSocketClientURI(websocketClient, endpoint);

                websocketClient.connectBlocking();
                Optional.of(websocketClient)
                        .filter(WebSocketClient::isOpen)
                        .ifPresent(client -> Assertions.fail("websocket endpoint '" + endpoint + "' exists"));
            } catch (AssertionError | InterruptedException | WebsocketNotConnectedException error) {
                Assertions.fail("websocket endpoint '" + endpoint + "' not exists", error);
            } catch (URISyntaxException | NoSuchFieldException | IllegalAccessException e) {
                throw new RuntimeException(e);
            }
        };
    }


    /**
     * update websocket client uri.
     * @param client client
     * @param endpoint endpoint
     */
    private static void updateWebSocketClientURI(final WebSocketClient client, final String endpoint)
            throws NoSuchFieldException, IllegalAccessException, URISyntaxException {
        Field uriField = WebSocketClient.class.getDeclaredField("uri");
        uriField.setAccessible(true);
        URI originalUri = client.getURI();
        URI updatedUri = new URI(originalUri + endpoint);
        uriField.set(client, updatedUri);
    }
}
