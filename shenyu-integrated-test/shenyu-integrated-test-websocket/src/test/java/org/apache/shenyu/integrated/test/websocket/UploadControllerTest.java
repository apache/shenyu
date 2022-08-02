package org.apache.shenyu.integrated.test.websocket;

import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;

public class UploadControllerTest  extends AbstractPluginDataInit {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketPluginTest.class);

    private static final String WEBSOCKET_URI = "ws://localhost:9195/ws-annotation/upload?token=Jack";

    @Test
    public void testWebsocketUpLoad() {
        try {
            WebSocketClient webSocketClient = new WebSocketClient(new URI(WEBSOCKET_URI)) {
                @Override
                public void onOpen(ServerHandshake serverHandshake) {

                }

                @Override
                public void onMessage(String s) {

                }

                @Override
                public void onClose(int i, String s, boolean b) {

                }

                @Override
                public void onError(Exception e) {

                }
            };
            byte [] msg = new byte[16];
            msg[0] = 1;
            webSocketClient.connectBlocking();
            webSocketClient.send(msg);
            System.out.println("testeteteteteettete");
        } catch (Exception e) {

        }
    }
}
