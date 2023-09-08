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

package org.apache.shenyu.k8s.repository;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonMetaDataSubscriber;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;

import java.util.List;

/**
 * The repository to config shenyu.
 *
 * <p>Should try to avoid directly operating memory configuration through PluginDataSubscriber
 * in ingress-controller, but use ShenyuCacheRepository.
 * This will make it easier for us if we have architectural changes to the ingress-controller.
 * </p>
 */
public class ShenyuCacheRepository {

    private final PluginDataSubscriber subscriber;

    private final CommonMetaDataSubscriber metaDataSubscriber;

    /**
     * Shenyu Cache Repository Constructor.
     *
     * @param subscriber PluginDataSubscriber
     */
    public ShenyuCacheRepository(final PluginDataSubscriber subscriber, final CommonMetaDataSubscriber metaDataSubscriber) {
        this.subscriber = subscriber;
        this.metaDataSubscriber = metaDataSubscriber;
    }

    /**
     * Find PluginData by plugin name.
     *
     * @param pluginName  plugin name
     * @return PluginData
     */
    public PluginData findPluginData(final String pluginName) {
        return BaseDataCache.getInstance().obtainPluginData(pluginName);
    }

    /**
     * Save or update PluginData by PluginData.
     *
     * @param pluginData PluginData
     */
    public void saveOrUpdatePluginData(final PluginData pluginData) {
        subscriber.onSubscribe(pluginData);
    }

    /**
     * Delete PluginData by plugin name.
     *
     * @param pluginName plugin name
     */
    public void deletePluginData(final String pluginName) {
        subscriber.unSubscribe(PluginData.builder().name(pluginName).build());
    }

    /**
     * Find SelectorData list by pluginName.
     *
     * @param pluginName plugin name
     * @return SelectorData list
     */
    public List<SelectorData> findSelectorDataList(final String pluginName) {
        return BaseDataCache.getInstance().obtainSelectorData(pluginName);
    }

    /**
     * Save or update SelectorData by SelectorData.
     *
     * @param selectorData SelectorData
     */
    public void saveOrUpdateSelectorData(final SelectorData selectorData) {
        subscriber.onSelectorSubscribe(selectorData);
    }

    /**
     * Delete SelectorData by plugin name and selector id.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     */
    public void deleteSelectorData(final String pluginName, final String selectorId) {
        subscriber.unSelectorSubscribe(SelectorData.builder().pluginName(pluginName).id(selectorId).build());
    }

    /**
     * Find RuleData list by selector id.
     *
     * @param selectorId selector id
     * @return RuleData list
     */
    public List<RuleData> findRuleDataList(final String selectorId) {
        return BaseDataCache.getInstance().obtainRuleData(selectorId);
    }

    /**
     * Save or update RuleData by RuleData.
     *
     * @param ruleData RuleData
     */
    public void saveOrUpdateRuleData(final RuleData ruleData) {
        subscriber.onRuleSubscribe(ruleData);
    }

    /**
     * Delete RuleData by plugin name, selector id and rule id.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     * @param ruleId rule id
     */
    public void deleteRuleData(final String pluginName, final String selectorId, final String ruleId) {
        subscriber.unRuleSubscribe(RuleData.builder().pluginName(pluginName).selectorId(selectorId).id(ruleId).build());
    }

    /**
     * Find MetaData by path.
     * @param path path
     * @return MetaData
     */
    public MetaData findMetaData(final String path) {
        return MetaDataCache.getInstance().obtain(path);
    }

    /**
     * Save or update MetaData by MetaData.
     * @param metaData MetaData
     */
    public void saveOrUpdateMetaData(final MetaData metaData) {
        metaDataSubscriber.onSubscribe(metaData);
    }

    /**
     * Delete MetaData by MetaData.
     * @param metaData MetaData
     */
    public void deleteMetaData(final MetaData metaData) {
        metaDataSubscriber.unSubscribe(metaData);
    }
}
