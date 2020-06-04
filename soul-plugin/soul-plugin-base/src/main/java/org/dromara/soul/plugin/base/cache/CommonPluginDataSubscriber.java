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

package org.dromara.soul.plugin.base.cache;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;

/**
 * The type Common plugin data subscriber.
 *
 * @author xiaoyu
 */
public class CommonPluginDataSubscriber implements PluginDataSubscriber {
    
    private final Map<String, PluginDataHandler> handlerMap;
    
    /**
     * Instantiates a new Common plugin data subscriber.
     *
     * @param pluginDataHandlerList the plugin data handler list
     */
    public CommonPluginDataSubscriber(final List<PluginDataHandler> pluginDataHandlerList) {
        this.handlerMap = pluginDataHandlerList.stream().collect(Collectors.toMap(PluginDataHandler::pluginNamed, e -> e));
    }
    
    @Override
    public void onSubscribe(final PluginData pluginData) {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        handlerMap.get(pluginData.getName()).handlerPlugin(pluginData);
    }
    
    @Override
    public void unSubscribe(final PluginData pluginData) {
        BaseDataCache.getInstance().removePluginData(pluginData);
        handlerMap.get(pluginData.getName()).removePlugin(pluginData);
    }
    
    @Override
    public void onSelectorSubscribe(final SelectorData selectorData) {
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        handlerMap.get(selectorData.getPluginName()).handlerSelector(selectorData);
    }
    
    @Override
    public void unSelectorSubscribe(final SelectorData selectorData) {
        BaseDataCache.getInstance().removeSelectData(selectorData);
        handlerMap.get(selectorData.getPluginName()).removeSelector(selectorData);
    }
    
    @Override
    public void onRuleSubscribe(final RuleData ruleData) {
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        handlerMap.get(ruleData.getPluginName()).handlerRule(ruleData);
    }
    
    @Override
    public void unRuleSubscribe(final RuleData ruleData) {
        BaseDataCache.getInstance().removeRuleData(ruleData);
        handlerMap.get(ruleData.getPluginName()).removeRule(ruleData);
    }
}
