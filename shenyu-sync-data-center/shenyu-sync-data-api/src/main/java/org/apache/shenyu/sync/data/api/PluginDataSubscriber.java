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

package org.apache.shenyu.sync.data.api;

import java.util.List;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

/**
 * The interface Plugin data subscriber.
 */
public interface PluginDataSubscriber {
    
    /**
     * On  subscribe.
     *
     * @param pluginData the plugin data
     */
    default void onSubscribe(PluginData pluginData) {
    }
    
    /**
     * Un subscribe plugin data.
     *
     * @param pluginData the plugin data
     */
    default void unSubscribe(PluginData pluginData) {
    }
    
    /**
     * Refresh  all plugin data.
     */
    default void refreshPluginDataAll() {
    }
    
    /**
     * Refresh plugin data self.
     *
     * @param pluginDataList the plugin data list
     */
    default void refreshPluginDataSelf(List<PluginData> pluginDataList) {
    }
    
    /**
     * On selector subscribe.
     *
     * @param selectorData the selector data
     */
    default void onSelectorSubscribe(SelectorData selectorData) {
    }
    
    /**
     * Un selector subscribe.
     *
     * @param selectorData the selector data
     */
    default void unSelectorSubscribe(SelectorData selectorData) {
    }
    
    /**
     * Refresh all selector data.
     */
    default void refreshSelectorDataAll() {
    }
    
    /**
     * Refresh selector data.
     *
     * @param selectorDataList the selector data list
     */
    default void refreshSelectorDataSelf(List<SelectorData> selectorDataList) {
    }
    
    /**
     * On rule subscribe.
     *
     * @param ruleData the rule data
     */
    default void onRuleSubscribe(RuleData ruleData) {
    }
    
    /**
     * On rule subscribe.
     *
     * @param ruleData the rule data
     */
    default void unRuleSubscribe(RuleData ruleData) {
    }
    
    /**
     * Refresh rule data.
     */
    default void refreshRuleDataAll() {
    }
    
    /**
     * Refresh rule data self.
     *
     * @param ruleDataList the rule data list
     */
    default void refreshRuleDataSelf(List<RuleData> ruleDataList) {
    }
}
