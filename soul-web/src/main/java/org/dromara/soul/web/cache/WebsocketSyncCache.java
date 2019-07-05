package org.dromara.soul.web.cache;

import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.WebsocketData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.config.WebsocketConfig;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

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
                    handleResult(result);
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

    private void handleResult(final String result) {
        WebsocketData websocketData = GsonUtils.getInstance().fromJson(result, WebsocketData.class);
        ConfigGroupEnum groupEnum = ConfigGroupEnum.acquireByName(websocketData.getGroupType());
        String eventType = websocketData.getEventType();
        switch (groupEnum) {
            case PLUGIN:
                List<PluginData> pluginDataList =
                        GsonUtils.getInstance().fromList(websocketData.getData().toString(),
                                PluginData[].class);
                handlePlugin(pluginDataList, eventType);
                break;
            case SELECTOR:
                List<SelectorData> selectorDataList =
                        GsonUtils.getInstance().fromList(websocketData.getData().toString(),
                                SelectorData[].class);
                handleSelector(selectorDataList, eventType);
                break;
            case RULE:
                List<RuleData> ruleDataList =
                        GsonUtils.getInstance().fromList(websocketData.getData().toString(),
                                RuleData[].class);
                handleRule(ruleDataList, eventType);
                break;
            case APP_AUTH:
                List<AppAuthData> appAuthDataList =
                        GsonUtils.getInstance().fromList(websocketData.getData().toString(),
                                AppAuthData[].class);
                handleAppAuth(appAuthDataList, eventType);
                break;
            default:
                break;
        }
    }
}
