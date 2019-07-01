package org.dromara.soul.admin.listener.websocket;

import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.listener.DataEventType;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;

import java.util.List;

/**
 * @author xiaoyu(Myth)
 */
public class WebsocketDataChangedListener implements DataChangedListener {

    @Override
    public void onAppAuthChanged(List<AppAuthData> changed, DataEventType eventType) {

    }

    @Override
    public void onPluginChanged(List<PluginData> changed, DataEventType eventType) {

    }

    @Override
    public void onSelectorChanged(List<SelectorData> changed, DataEventType eventType) {

    }

    @Override
    public void onRuleChanged(List<RuleData> changed, DataEventType eventType) {

    }

}
