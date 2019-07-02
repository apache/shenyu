package org.dromara.soul.admin.listener.websocket;

import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.listener.DataEventType;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;

import java.util.List;

/**
 * The type Websocket data changed listener.
 *
 * @author xiaoyu(Myth)
 */
public class WebsocketDataChangedListener implements DataChangedListener {

    /**
     * Instantiates a new Websocket data changed listener.
     */
    public WebsocketDataChangedListener() {

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

}
