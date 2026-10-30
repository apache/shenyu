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

package org.apache.shenyu.plugin.sync.data.websocket;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;

import java.util.ArrayList;
import java.util.List;

/**
 * A subscriber implementing only the legacy refresh callbacks.
 */
public final class LegacyPluginDataSubscriber implements PluginDataSubscriber {

    private final List<String> calls = new ArrayList<>();

    /**
     * Obtain the ordered callback log.
     *
     * @return callback log
     */
    public List<String> getCalls() {
        return List.copyOf(calls);
    }

    @Override
    public void refreshPluginDataAll() {
        calls.add("PLUGIN:all");
    }

    @Override
    public void refreshPluginDataSelf(final List<PluginData> pluginDataList) {
        pluginDataList.forEach(data -> calls.add("PLUGIN:self:" + data.getName()));
    }

    @Override
    public void onSubscribe(final PluginData pluginData) {
        calls.add("PLUGIN:subscribe:" + pluginData.getName());
    }

    @Override
    public void refreshSelectorDataAll() {
        calls.add("SELECTOR:all");
    }

    @Override
    public void refreshSelectorDataSelf(final List<SelectorData> selectorDataList) {
        selectorDataList.forEach(data -> calls.add("SELECTOR:self:" + data.getId()));
    }

    @Override
    public void onSelectorSubscribe(final SelectorData selectorData) {
        calls.add("SELECTOR:subscribe:" + selectorData.getId());
    }

    @Override
    public void refreshRuleDataAll() {
        calls.add("RULE:all");
    }

    @Override
    public void refreshRuleDataSelf(final List<RuleData> ruleDataList) {
        ruleDataList.forEach(data -> calls.add("RULE:self:" + data.getId()));
    }

    @Override
    public void onRuleSubscribe(final RuleData ruleData) {
        calls.add("RULE:subscribe:" + ruleData.getId());
    }
}
