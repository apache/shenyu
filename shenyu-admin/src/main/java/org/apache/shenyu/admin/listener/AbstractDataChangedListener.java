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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
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
        ConfigDataCache config = CACHE.get(HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, groupKey.name()));
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
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        this.updateAppAuthCache(namespaceId);
        this.afterAppAuthChanged(changed, eventType, namespaceId);
    }

    /**
     * After app auth changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        this.updateMetaDataCache(namespaceId);
        this.afterMetaDataChanged(changed, eventType, namespaceId);
    }

    /**
     * After meta data changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
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
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        this.updateRuleCache(namespaceId);
        this.afterRuleChanged(changed, eventType, namespaceId);
    }

    /**
     * After rule changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        this.updateSelectorCache(namespaceId);
        this.afterSelectorChanged(changed, eventType, namespaceId);
    }

    /**
     * invoke this method when ProxySelector was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        this.updateProxySelectorDataCache(namespaceId);
        this.afterProxySelectorChanged(changed, eventType, namespaceId);
    }

    /**
     * After proxySelector changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
    }

    /**
     * invoke this method when DiscoveryUpstream was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        String namespaceId = changed.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
        this.updateDiscoveryUpstreamDataCache(namespaceId);
        this.afterDiscoveryUpstreamDataChanged(changed, eventType, namespaceId);
    }

    /**
     * After DiscoveryUpstreamData changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterDiscoveryUpstreamDataChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
    }

    /**
     * After selector changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    protected void afterSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
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
        String configDataCacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group.name());
        ConfigDataCache newVal = new ConfigDataCache(configDataCacheKey, json, DigestUtils.md5Hex(json), System.currentTimeMillis(), namespaceId);
        ConfigDataCache oldVal = CACHE.put(newVal.getGroup(), newVal);
        LOG.info("update config cache[{}], old: {}, updated: {}", group, oldVal, newVal);
    }

    /**
     * refresh local cache.
     */
    protected void refreshLocalCache() {
        List<NamespaceVO> namespaceList = namespaceService.listAll();
        for (NamespaceVO namespace : namespaceList) {
            String namespaceId = namespace.getNamespaceId();
            this.updatePluginCache(namespaceId);
            this.updateAppAuthCache(namespaceId);
            this.updateRuleCache(namespaceId);
            this.updateSelectorCache(namespaceId);
            this.updateMetaDataCache(namespaceId);
            this.updateProxySelectorDataCache(namespaceId);
            this.updateDiscoveryUpstreamDataCache(namespaceId);
        }
    }

    /**
     * Update selector cache.
     */
    protected void updateSelectorCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.SELECTOR, selectorService.listAll(), namespaceId);
    }

    /**
     * Update rule cache.
     */
    protected void updateRuleCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.RULE, ruleService.listAll(), namespaceId);
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
    protected void updateAppAuthCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.APP_AUTH, appAuthService.listAll(), namespaceId);
    }

    /**
     * Update meta data cache.
     */
    protected void updateMetaDataCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.META_DATA, metaDataService.listAll(), namespaceId);
    }

    protected void updateProxySelectorDataCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.PROXY_SELECTOR, proxySelectorService.listAll(), namespaceId);
    }

    protected void updateDiscoveryUpstreamDataCache(final String namespaceId) {
        this.updateCache(ConfigGroupEnum.DISCOVER_UPSTREAM, discoveryUpstreamService.listAll(), namespaceId);
    }

    private <T> ConfigData<T> buildConfigData(final ConfigDataCache config, final Class<T> dataType) {
        return new ConfigData<>(config.getMd5(), config.getLastModifyTime(), GsonUtils.getInstance().fromList(config.getJson(), dataType));
    }

}
