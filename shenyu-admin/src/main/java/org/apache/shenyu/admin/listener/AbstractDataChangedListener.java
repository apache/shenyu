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

package org.apache.shenyu.admin.listener;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;

import javax.annotation.Resource;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;


/**
 * Abstract class for ConfigEventListener.
 * As we think that the md5 value of the in-memory data is the same as the md5 value of the database,
 * although it may be a little different, but it doesn't matter, we will have thread to periodically
 * pull the data in the database.
 *
 * @since 2.0.0
 */
public abstract class AbstractDataChangedListener implements DataChangedListener, InitializingBean {

    /**
     * The constant CACHE.
     */
    protected static final ConcurrentMap<String, ConfigDataCache> CACHE = new ConcurrentHashMap<>();

    private static final Logger LOG = LoggerFactory.getLogger(AbstractDataChangedListener.class);

    @Resource
    private AppAuthService appAuthService;

    /**
     * The Plugin service.
     */
    @Resource
    private PluginService pluginService;

    /**
     * The Rule service.
     */
    @Resource
    private RuleService ruleService;

    /**
     * The Selector service.
     */
    @Resource
    private SelectorService selectorService;

    @Resource
    private MetaDataService metaDataService;

    /**
     * fetch configuration from cache.
     *
     * @param groupKey the group key
     * @return the configuration data
     */
    public ConfigData<?> fetchConfig(final ConfigGroupEnum groupKey) {
        ConfigDataCache config = CACHE.get(groupKey.name());
        switch (groupKey) {
            case APP_AUTH:
                return buildConfigData(config, AppAuthData.class);
            case PLUGIN:
                return buildConfigData(config, PluginData.class);
            case RULE:
                return buildConfigData(config, RuleData.class);
            case SELECTOR:
                return buildConfigData(config, SelectorData.class);
            case META_DATA:
                return buildConfigData(config, MetaData.class);
            default:
                throw new IllegalStateException("Unexpected groupKey: " + groupKey);
        }
    }
    
    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updateAppAuthCache();
        this.afterAppAuthChanged(changed, eventType);
    }
    
    /**
     * After app auth changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
    }
    
    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updateMetaDataCache();
        this.afterMetaDataChanged(changed, eventType);
    }
    
    /**
     * After meta data changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
    }
    
    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updatePluginCache();
        this.afterPluginChanged(changed, eventType);
    }
    
    /**
     * After plugin changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
    }
    
    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updateRuleCache();
        this.afterRuleChanged(changed, eventType);
    }
    
    /**
     * After rule changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
    }
    
    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updateSelectorCache();
        this.afterSelectorChanged(changed, eventType);
    }
    
    /**
     * After selector changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
    }
    
    @Override
    public final void afterPropertiesSet() {
        this.refreshLocalCache();
        this.afterInitialize();
    }
    
    protected abstract void afterInitialize();
    
    /**
     * if md5 is not the same as the original, then update lcoal cache.
     * @param group ConfigGroupEnum
     * @param <T> the type of class
     * @param data the new config data
     */
    protected <T> void updateCache(final ConfigGroupEnum group, final List<T> data) {
        String json = GsonUtils.getInstance().toJson(data);
        ConfigDataCache newVal = new ConfigDataCache(group.name(), json, DigestUtils.md5Hex(json), System.currentTimeMillis());
        ConfigDataCache oldVal = CACHE.put(newVal.getGroup(), newVal);
        LOG.info("update config cache[{}], old: {}, updated: {}", group, oldVal, newVal);
    }
    
    /**
     * refresh local cache.
     */
    protected void refreshLocalCache() {
        this.updateAppAuthCache();
        this.updatePluginCache();
        this.updateRuleCache();
        this.updateSelectorCache();
        this.updateMetaDataCache();
    }
    
    /**
     * Update selector cache.
     */
    protected void updateSelectorCache() {
        this.updateCache(ConfigGroupEnum.SELECTOR, selectorService.listAll());
    }
    
    /**
     * Update rule cache.
     */
    protected void updateRuleCache() {
        this.updateCache(ConfigGroupEnum.RULE, ruleService.listAll());
    }
    
    /**
     * Update plugin cache.
     */
    protected void updatePluginCache() {
        this.updateCache(ConfigGroupEnum.PLUGIN, pluginService.listAll());
    }
    
    /**
     * Update app auth cache.
     */
    protected void updateAppAuthCache() {
        this.updateCache(ConfigGroupEnum.APP_AUTH, appAuthService.listAll());
    }
    
    /**
     * Update meta data cache.
     */
    protected void updateMetaDataCache() {
        this.updateCache(ConfigGroupEnum.META_DATA, metaDataService.listAll());
    }
    
    private <T> ConfigData<T> buildConfigData(final ConfigDataCache config, final Class<T> dataType) {
        return new ConfigData<>(config.getMd5(), config.getLastModifyTime(), GsonUtils.getInstance().fromList(config.getJson(), dataType));
    }
}
