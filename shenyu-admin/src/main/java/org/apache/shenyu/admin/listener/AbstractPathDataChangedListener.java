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

import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

/**
 * AbstractPathDataChangedListener.
 * 路径数据变更的监听器的抽象框架实现
 */
public abstract class AbstractPathDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractPathDataChangedListener.class);

    /**
     * 规则同步对象锁
     */
    private final Object ruleSyncObject = new Object();

    /**
     * 选择器同步对象锁
     */
    private final Object selectorSyncObject = new Object();

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        for (AppAuthData data : changed) {
            String appAuthPath = DefaultPathConstants.buildAppAuthPath(data.getAppKey());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(appAuthPath);
                LOG.debug("[DataChangedListener] delete appKey {}", data.getAppKey());
                continue;
            }
            // create or update
            createOrUpdate(appAuthPath, data);
            LOG.debug("[DataChangedListener] change appKey {}", data.getAppKey());
        }
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        for (MetaData data : changed) {
            try {
                String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(data.getPath(), "UTF-8"));
                // delete
                if (eventType == DataEventTypeEnum.DELETE) {
                    deleteNode(metaDataPath);
                    LOG.debug("[DataChangedListener] delete appKey {}", metaDataPath);
                    continue;
                }
                // create or update
                createOrUpdate(metaDataPath, data);
                LOG.debug("[DataChangedListener] change metaDataPath {}", metaDataPath);
            } catch (UnsupportedEncodingException e) {
                LOG.error("[DataChangedListener] url encode error.", e);
                throw new ShenyuException(e.getMessage());
            }
        }
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        for (ProxySelectorData data : changed) {
            String proxySelectorPath = DefaultPathConstants.buildProxySelectorPath(data.getPluginName(), data.getName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(proxySelectorPath);
                LOG.debug("[DataChangedListener] delete appKey {}", proxySelectorPath);
                continue;
            }
            // create or update
            createOrUpdate(proxySelectorPath, data);
            LOG.info("[DataChangedListener] change proxySelector path={}|data={}", proxySelectorPath, data);
        }
    }

    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> changed, final DataEventTypeEnum eventType) {
        for (DiscoverySyncData data : changed) {
            String upstreamPath = DefaultPathConstants.buildDiscoveryUpstreamPath(data.getPluginName(), data.getSelectorName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(upstreamPath);
                LOG.debug("[DataChangedListener] delete appKey {}", upstreamPath);
                continue;
            }
            // create or update
            createOrUpdate(upstreamPath, data);
            LOG.info("[DataChangedListener] change discoveryUpstream path={}|data={}", upstreamPath, data);
        }
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        // 选择器数据的刷新事件
        if (eventType == DataEventTypeEnum.REFRESH && !changed.isEmpty()) {
            // 构建选择器的父路径("/shenyu/selector/${pluginName}")
            String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(changed.get(0).getPluginName());
            // 递归地删除节点路径
            deletePathRecursive(selectorParentPath);
        }
        // 变更的选择器数据遍历
        for (SelectorData data : changed) {
            // 选择器的真实路径("/shenyu/selector/${pluginName}/${selectorId}")
            String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(data.getPluginName(), data.getId());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(selectorRealPath);
                LOG.debug("[DataChangedListener] delete appKey {}", selectorRealPath);
                continue;
            }
            // create or update
            synchronized (selectorSyncObject) {
                createOrUpdate(selectorRealPath, data);
            }
            // 操作日志
            LOG.debug("[DataChangedListener] change path {} with data {}", selectorRealPath, data);
        }
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        // 变更的插件数据遍历
        for (PluginData data : changed) {
            // 构建插件路径("/shenyu/plugin/${pluginName}")
            String pluginPath = DefaultPathConstants.buildPluginPath(data.getName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deletePathRecursive(pluginPath);
                String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(data.getName());
                deletePathRecursive(selectorParentPath);
                String ruleParentPath = DefaultPathConstants.buildRuleParentPath(data.getName());
                deletePathRecursive(ruleParentPath);
                LOG.debug("[DataChangedListener] delete pluginPath {}", pluginPath);
                continue;
            }
            // create or update
            createOrUpdate(pluginPath, data);
            // 操作日志
            LOG.debug("[DataChangedListener] change path {} with data {}", pluginPath, data);
        }
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        // 规则数据的刷新事件
        if (eventType == DataEventTypeEnum.REFRESH && !changed.isEmpty()) {
            // 构建规则的父路径("/shenyu/rule/${pluginName}")
            String ruleParentPath = DefaultPathConstants.buildRuleParentPath(changed.get(0).getPluginName());
            // 递归地删除节点路径
            deletePathRecursive(ruleParentPath);
        }
        // 变更的规则数据遍历
        for (RuleData data : changed) {
            // 规则的真实路径("/shenyu/rule/${pluginName}/${selectorId-ruleId}")
            String ruleRealPath = DefaultPathConstants.buildRulePath(data.getPluginName(), data.getSelectorId(), data.getId());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(ruleRealPath);
                continue;
            }
            // create or update
            synchronized (ruleSyncObject) {
                createOrUpdate(ruleRealPath, data);
            }
            // 操作日志
            LOG.debug("[DataChangedListener] change path {} with data {}", ruleRealPath, data);
        }
    }

    /**
     * createOrUpdate.
     *
     * @param pluginPath pluginPath
     * @param data       data
     */
    public abstract void createOrUpdate(String pluginPath, Object data);

    /**
     * deleteNode.
     *
     * @param pluginPath pluginPath
     */
    public abstract void deleteNode(String pluginPath);

    /**
     * deletePathRecursive.
     *
     * @param selectorParentPath selectorParentPath
     */
    public abstract void deletePathRecursive(String selectorParentPath);
}
