package org.dromara.soul.admin.listener.websocket;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.WebsocketData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * The type Websocket data changed listener.
 *
 * @author xiaoyu(Myth)
 */
@ServerEndpoint("/websocket")
public class WebsocketDataChangedListener implements DataChangedListener {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketDataChangedListener.class);

    private static final CopyOnWriteArraySet<Session> SESSION_SET = new CopyOnWriteArraySet<>();

    private SyncDataService syncDataService;

    public WebsocketDataChangedListener() {
        System.out.println("why......");
    }

    public WebsocketDataChangedListener(SyncDataService syncDataService) {
        this.syncDataService = syncDataService;
    }

    @OnOpen
    public void onOpen(Session session) {
        SESSION_SET.add(session);
    }

    @OnMessage
    public void onMessage(String message, Session session) {
        syncDataService.syncAll();
    }

    @OnClose
    public void onClose(final Session session) {
        SESSION_SET.remove(session);
    }

    @OnError
    public void onError(final Session session, final Throwable error) {
        SESSION_SET.remove(session);
        LOGGER.error("websocket collection error:{}", error);
    }

    @Override
    public void onPluginChanged(final List<PluginData> pluginDataList, final DataEventTypeEnum eventType) {
        WebsocketData<PluginData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.PLUGIN.name(), eventType.name(), pluginDataList);
        send(GsonUtils.getInstance().toJson(websocketData));
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> selectorDataList, final DataEventTypeEnum eventType) {
        WebsocketData<SelectorData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.SELECTOR.name(), eventType.name(), selectorDataList);
        send(GsonUtils.getInstance().toJson(websocketData));
    }

    @Override
    public void onRuleChanged(final List<RuleData> ruleDataList, final DataEventTypeEnum eventType) {
        WebsocketData<RuleData> configData =
                new WebsocketData<>(ConfigGroupEnum.RULE.name(), eventType.name(), ruleDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> appAuthDataList, final DataEventTypeEnum eventType) {
        WebsocketData<AppAuthData> configData =
                new WebsocketData<>(ConfigGroupEnum.APP_AUTH.name(), eventType.name(), appAuthDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    private void send(final String message) {
        if (StringUtils.isNotBlank(message)) {
            for (Session session : SESSION_SET) {
                try {
                    session.getBasicRemote().sendText(message);
                } catch (IOException e) {
                    LOGGER.error("websocket send result is exception :{}", e);
                }
            }
        }
    }
}
