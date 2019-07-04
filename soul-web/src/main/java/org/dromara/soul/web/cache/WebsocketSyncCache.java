package org.dromara.soul.web.cache;

import org.dromara.soul.web.config.WebsocketConfig;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * The type Websocket sync cache.
 *
 * @author xiaoyu(Myth)
 */
public class WebsocketSyncCache extends AbstractLocalCacheManager {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketSyncCache.class);

    /**
     * The Client.
     */
    public WebSocketClient client;

    /**
     * Instantiates a new Websocket sync cache.
     *
     * @param websocketConfig the websocket config
     */
    public WebsocketSyncCache(WebsocketConfig websocketConfig) {
        try {
            client = new WebSocketClient(new URI(websocketConfig.getUrl())) {
                @Override
                public void onOpen(ServerHandshake serverHandshake) {
                }

                @Override
                public void onMessage(String result) {
                }

                @Override
                public void onClose(int i, String s, boolean b) {
                    client.close();
                }

                @Override
                public void onError(Exception e) {
                    client.close();
                }
            };
        } catch (URISyntaxException e) {
            LOGGER.error("websocket url is error :{}", e);
        }
        client.connect();
    }
}
