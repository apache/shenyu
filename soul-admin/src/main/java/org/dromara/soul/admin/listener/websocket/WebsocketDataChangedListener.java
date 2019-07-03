package org.dromara.soul.admin.listener.websocket;

import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.listener.DataEventType;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The type Websocket data changed listener.
 *
 * @author xiaoyu(Myth)
 */
@ServerEndpoint("/websocket")
public class WebsocketDataChangedListener implements DataChangedListener {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketDataChangedListener.class);

    private static final Set<Session> SESSION_MAP = new HashSet<>();

    @OnOpen
    public void onOpen(Session session) {
        SESSION_MAP.add(session);
        send("Hello, connection opened!");
    }

    @OnMessage
    public void onMessage(final String message, final Session session) {
        for (Session entry : SESSION_MAP) {
            LOGGER.info("session.id={}", session.getId());

            break;
        }
        LOGGER.info("接收到消息：{}", message);

    }

    @OnClose
    public void onClose(final Session session) {
        SESSION_MAP.remove(session);
    }

    @OnError
    public void onError(final Session session, final Throwable error) {
        SESSION_MAP.remove(session);
        LOGGER.error("websocket collection error:{}", error);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventType eventType) {

    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventType eventType) {

    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventType eventType) {

    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventType eventType) {

    }

    private void send(final String message) {
        for (Session session : SESSION_MAP) {
            try {
                session.getBasicRemote().sendText(message);
            } catch (IOException e) {
                LOGGER.error("websocket send exception:{}", e);
            }
        }
    }

}
