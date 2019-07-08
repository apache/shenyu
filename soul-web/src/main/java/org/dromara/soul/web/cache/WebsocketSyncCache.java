package org.dromara.soul.web.cache;

import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.WebsocketData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.config.SoulConfig;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * The type Websocket sync cache.
 *
 * @author xiaoyu(Myth)
 */
public class WebsocketSyncCache extends WebsocketCacheHandler {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketSyncCache.class);

    /**
     * The Client.
     */
    private WebSocketClient client;

    /**
     * Instantiates a new Websocket sync cache.
     *
     * @param websocketConfig the websocket config
     */
    public WebsocketSyncCache(final SoulConfig.WebsocketConfig websocketConfig) {
        CompletableFuture.runAsync(() -> {
            try {
                client = new WebSocketClient(new URI(websocketConfig.getUrl())) {
                    @Override
                    public void onOpen(final ServerHandshake serverHandshake) {
                    }

                    @Override
                    public void onMessage(final String result) {
                        try {
                            handleResult(result);
                        } catch (Exception e) {
                            LOGGER.error("websocket handle data exception :{}", e);
                        }
                    }

                    @Override
                    public void onClose(final int i, final String s, final boolean b) {
                        client.close();
                        try {
                            client.reconnectBlocking();
                        } catch (InterruptedException e) {
                            LOGGER.error("websocket reconnectBlocking exception :{}", e);
                        }
                    }

                    @Override
                    public void onError(final Exception e) {
                        client.close();
                        try {
                            client.reconnectBlocking();
                        } catch (InterruptedException interruptedException) {
                            LOGGER.error("websocket reconnectBlocking exception :{}", interruptedException);
                        }
                    }
                };
            } catch (URISyntaxException e) {
                LOGGER.error("websocket url is error :{}", e);
            }
            try {
                boolean success = client.connectBlocking();
                if (success) {
                    LOGGER.info("websocket connection is successful.....");
                    client.send(DataEventTypeEnum.MYSELF.name());
                }
            } catch (InterruptedException e) {
                LOGGER.info("websocket connection...exception....{}", e);
            }

        });
    }

    private void handleResult(final String result) {
        WebsocketData websocketData = GsonUtils.getInstance().fromJson(result, WebsocketData.class);
        ConfigGroupEnum groupEnum = ConfigGroupEnum.acquireByName(websocketData.getGroupType());
        String eventType = websocketData.getEventType();
        switch (groupEnum) {
            case PLUGIN:
                String pluginData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<PluginData> pluginDataList =
                        GsonUtils.getInstance().fromList(pluginData, PluginData.class);
                handlePlugin(pluginDataList, eventType);
                break;
            case SELECTOR:
                String selectorData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<SelectorData> selectorDataList =
                        GsonUtils.getInstance().fromList(selectorData, SelectorData.class);
                handleSelector(selectorDataList, eventType);
                break;
            case RULE:
                String ruleData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<RuleData> ruleDataList =
                        GsonUtils.getInstance().fromList(ruleData, RuleData.class);
                handleRule(ruleDataList, eventType);
                break;
            case APP_AUTH:
                String appAuthData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<AppAuthData> appAuthDataList =
                        GsonUtils.getInstance().fromList(appAuthData, AppAuthData.class);
                handleAppAuth(appAuthDataList, eventType);
                break;
            default:
                break;
        }
    }
}
