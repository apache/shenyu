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

import jakarta.annotation.Resource;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;


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
    private NamespacePluginService namespacePluginService;

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

    /**
     * The MetaData service.
     */
    @Resource
    private MetaDataService metaDataService;

    @Resource
    private ProxySelectorService proxySelectorService;

    @Resource
    private DiscoveryUpstreamService discoveryUpstreamService;

    @Resource
    private NamespaceService namespaceService;

    /**
     * fetch configuration from cache.
     *
     * @param groupKey    the group key
     * @param namespaceId the namespaceId
     * @return the configuration data
     */
    public ConfigData<?> fetchConfig(final ConfigGroupEnum groupKey, final String namespaceId) {
        //todo:[Namespace] Currently, only plugin data is compatible with namespace, while other data is waiting for modification
        ConfigDataCache config = CACHE.get(groupKey.name());
        if (groupKey.equals(ConfigGroupEnum.PLUGIN)) {
            config = CACHE.get(namespaceId + "_" + groupKey.name());
        }
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
            case PROXY_SELECTOR:
                return buildConfigData(config, ProxySelectorData.class);
            case DISCOVER_UPSTREAM:
                return buildConfigData(config, DiscoverySyncData.class);
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
        String namespaceId = changed.stream().map(PluginData::getNamespaceId).findFirst().orElse(SYS_DEFAULT_NAMESPACE_ID);
        this.updatePluginCache(namespaceId);
        this.afterPluginChanged(changed, eventType, namespaceId);
    }

    /**
     * After plugin changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        LOG.info("onRuleChanged, changed:{}, eventType:{}", JsonUtils.toJson(changed), JsonUtils.toJson(eventType));
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
     * invoke this method when ProxySelector was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updateProxySelectorDataCache();
        this.afterProxySelectorChanged(changed, eventType);
    }

    /**
     * After proxySelector changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
    }

    /**
     * invoke this method when DiscoveryUpstream was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        this.updateDiscoveryUpstreamDataCache();
        this.afterDiscoveryUpstreamDataChanged(changed, eventType);
    }

    /**
     * After DiscoveryUpstreamData changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterDiscoveryUpstreamDataChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
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
     * if md5 is not the same as the original, then update local cache.
     *
     * @param group ConfigGroupEnum
     * @param <T>   the type of class
     * @param data  the new config data
     */
    protected <T> void updateCache(final ConfigGroupEnum group, final List<T> data, final String namespaceId) {
        String json = GsonUtils.getInstance().toJson(data);
        //todo:[Namespace] Currently, only plugin data is compatible with namespace, while other data is waiting for modification
        String configDataCacheKey = group.name();
        if (group.equals(ConfigGroupEnum.PLUGIN)) {
            configDataCacheKey = namespaceId + group.name();
        }
        ConfigDataCache newVal = new ConfigDataCache(configDataCacheKey, json, DigestUtils.md5Hex(json), System.currentTimeMillis(), namespaceId);
        ConfigDataCache oldVal = CACHE.put(newVal.getGroup(), newVal);
        LOG.info("update config cache[{}], old: {}, updated: {}", group, oldVal, newVal);
        LOG.info("update config json: {}", json);
    }

    /**
     * refresh local cache.
     */
    protected void refreshLocalCache() {
        List<NamespaceVO> namespaceList = namespaceService.list();
        for (NamespaceVO namespace : namespaceList) {
            String namespaceId = namespace.getNamespaceId();
            this.updatePluginCache(namespaceId);
        }
        this.updateAppAuthCache();
        this.updateRuleCache();
        this.updateSelectorCache();
        this.updateMetaDataCache();
        this.updateProxySelectorDataCache();
        this.updateDiscoveryUpstreamDataCache();
    }

    /**
     * Update selector cache.
     */
    protected void updateSelectorCache() {
        this.updateCache(ConfigGroupEnum.SELECTOR, selectorService.listAll(), "");
    }

    /**
     * Update rule cache.
     */
    protected void updateRuleCache() {
        this.updateCache(ConfigGroupEnum.RULE, ruleService.listAll(), "");
    }

    /**
     * Update plugin cache.
     */
    protected void updatePluginCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.PLUGIN, namespacePluginService.listAll(namespaceId), namespaceId);
    }

    /**
     * Update app auth cache.
     */
    protected void updateAppAuthCache() {
        this.updateCache(ConfigGroupEnum.APP_AUTH, appAuthService.listAll(), "");
    }

    /**
     * Update meta data cache.
     */
    protected void updateMetaDataCache() {
        this.updateCache(ConfigGroupEnum.META_DATA, metaDataService.listAll(), "");
    }

    protected void updateProxySelectorDataCache() {
        this.updateCache(ConfigGroupEnum.PROXY_SELECTOR, proxySelectorService.listAll(), "");
    }

    protected void updateDiscoveryUpstreamDataCache() {
        this.updateCache(ConfigGroupEnum.DISCOVER_UPSTREAM, discoveryUpstreamService.listAll(), "");
    }

    private <T> ConfigData<T> buildConfigData(final ConfigDataCache config, final Class<T> dataType) {
        return new ConfigData<>(config.getMd5(), config.getLastModifyTime(), GsonUtils.getInstance().fromList(config.getJson(), dataType));
    }

}
