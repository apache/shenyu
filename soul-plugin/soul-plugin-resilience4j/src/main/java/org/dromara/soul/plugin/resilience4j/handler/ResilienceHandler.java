package org.dromara.soul.plugin.resilience4j.handler;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.plugin.resilience4j.factory.ResilienceRegistryFactory;

/**
 * ResilienceHandler.
 *
 * @Author zhanglei
 */
public class ResilienceHandler implements PluginDataHandler {


    @Override
    public void handlerRule(RuleData ruleData) {
        ResilienceRegistryFactory.remove(getResourceName(ruleData));
    }


    @Override
    public String pluginNamed() {
        return PluginEnum.Resilence4J.getName();
    }

    /**
     * return resilience4J resource name.
     *
     * @param ruleData ruleData
     * @return string string
     */
    public static String getResourceName(final RuleData ruleData) {
        return ruleData.getSelectorId() + "_" + ruleData.getName();
    }
}
