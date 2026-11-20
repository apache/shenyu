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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * AbstractPathDataChangedListener.
 */
public abstract class AbstractPathDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractPathDataChangedListener.class);

    private final Object ruleSyncObject = new Object();

    private final Object selectorSyncObject = new Object();

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        this.onAppAuthChanged(changed, eventType, null);
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        final String resolvedNamespaceId = DataChangedListener.snapshotNamespace(namespaceId, changed, AppAuthData::getNamespaceId);
        if (Objects.isNull(resolvedNamespaceId)) {
            return;
        }
        if (DataChangedListener.isNamespaceRefresh(eventType, namespaceId)) {
            this.reconcile(resolvedNamespaceId, "appAuth",
                    () -> this.refreshFlatGroup(resolvedNamespaceId, DefaultPathConstants.APP_AUTH_PARENT, changed,
                            AppAuthData::getAppKey, DefaultPathConstants::buildAppAuthPath));
            return;
        }
        for (AppAuthData data : changed) {
            String appAuthPath = DefaultPathConstants.buildAppAuthPath(data.getNamespaceId(), data.getAppKey());
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
            String metaDataPath = DefaultPathConstants.buildMetaDataPath(data.getNamespaceId(), data.getPath());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(metaDataPath);
                LOG.debug("[DataChangedListener] delete appKey {}", metaDataPath);
                continue;
            }
            // create or update
            createOrUpdate(metaDataPath, data);
            LOG.debug("[DataChangedListener] change metaDataPath {}", metaDataPath);
        }
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        for (ProxySelectorData data : changed) {
            String proxySelectorPath = DefaultPathConstants.buildProxySelectorPath(data.getNamespaceId(), data.getPluginName(), data.getId());
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
            if (StringUtils.isAnyBlank(data.getPluginName(), data.getSelectorId())) {
                LOG.warn("[DataChangedListener] ignore discoveryUpstream change with empty pluginName or selectorId, namespaceId={}, pluginName={}, selectorId={}",
                        data.getNamespaceId(), data.getPluginName(), data.getSelectorId());
                continue;
            }
            String upstreamPath = DefaultPathConstants.buildDiscoveryUpstreamPath(data.getNamespaceId(), data.getPluginName(), data.getSelectorId());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(upstreamPath);
                LOG.debug("[DataChangedListener] delete appKey {}", upstreamPath);
                continue;
            }
            // create or update
            createOrUpdate(upstreamPath, data);
            LOG.info("[DataChangedListener] change discoveryUpstream path={}|data={}", upstreamPath, GsonUtils.getInstance().toJson(data));
        }
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        this.onSelectorChanged(changed, eventType, null);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        final String resolvedNamespaceId = DataChangedListener.snapshotNamespace(namespaceId, changed, SelectorData::getNamespaceId);
        if (Objects.isNull(resolvedNamespaceId)) {
            return;
        }
        final boolean wholeNamespace = DataChangedListener.isNamespaceRefresh(eventType, namespaceId);
        if (DataEventTypeEnum.REFRESH == eventType) {
            this.reconcile(resolvedNamespaceId, "selector",
                    () -> this.refreshSelectorGroup(resolvedNamespaceId, changed, wholeNamespace));
            return;
        }
        for (SelectorData data : changed) {
            String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(data.getNamespaceId(), data.getPluginName(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(selectorRealPath);
                LOG.debug("[DataChangedListener] delete appKey {}", selectorRealPath);
                continue;
            }
            //create or update
            synchronized (selectorSyncObject) {
                createOrUpdate(selectorRealPath, data);
            }
            LOG.debug("[DataChangedListener] change path {} with data {}", selectorRealPath, data);
        }
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        this.onPluginChanged(changed, eventType, null);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        final String resolvedNamespaceId = DataChangedListener.snapshotNamespace(namespaceId, changed, PluginData::getNamespaceId);
        if (Objects.isNull(resolvedNamespaceId)) {
            return;
        }
        if (DataChangedListener.isNamespaceRefresh(eventType, namespaceId)) {
            this.reconcile(resolvedNamespaceId, "plugin", () -> {
                // enumerate the stale plugins before the snapshot overwrites the plugin listing
                final Set<String> stalePlugins = this.stalePluginNames(resolvedNamespaceId, changed);
                this.refreshFlatGroup(resolvedNamespaceId, DefaultPathConstants.PLUGIN_PARENT, changed,
                        PluginData::getName, DefaultPathConstants::buildPluginPath);
                stalePlugins.forEach(pluginName -> {
                    deletePathRecursive(DefaultPathConstants.buildSelectorParentPath(resolvedNamespaceId, pluginName));
                    deletePathRecursive(DefaultPathConstants.buildRuleParentPath(resolvedNamespaceId, pluginName));
                });
            });
            return;
        }
        for (PluginData data : changed) {
            String pluginPath = DefaultPathConstants.buildPluginPath(data.getNamespaceId(), data.getName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deletePathRecursive(pluginPath);
                String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(data.getNamespaceId(), data.getName());
                deletePathRecursive(selectorParentPath);
                String ruleParentPath = DefaultPathConstants.buildRuleParentPath(data.getNamespaceId(), data.getName());
                deletePathRecursive(ruleParentPath);
                LOG.debug("[DataChangedListener] delete pluginPath {}", pluginPath);
                continue;
            }
            //create or update
            createOrUpdate(pluginPath, data);
            LOG.debug("[DataChangedListener] change path {} with data {}", pluginPath, data);
        }
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        this.onRuleChanged(changed, eventType, null);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType, final String namespaceId) {
        final String resolvedNamespaceId = DataChangedListener.snapshotNamespace(namespaceId, changed, RuleData::getNamespaceId);
        if (Objects.isNull(resolvedNamespaceId)) {
            return;
        }
        final boolean wholeNamespace = DataChangedListener.isNamespaceRefresh(eventType, namespaceId);
        if (DataEventTypeEnum.REFRESH == eventType) {
            this.reconcile(resolvedNamespaceId, "rule",
                    () -> this.refreshRuleGroup(resolvedNamespaceId, changed, wholeNamespace));
            return;
        }
        for (RuleData data : changed) {
            String ruleRealPath = DefaultPathConstants.buildRulePath(data.getNamespaceId(), data.getPluginName(), data.getSelectorId(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(ruleRealPath);
                continue;
            }
            //create or update
            synchronized (ruleSyncObject) {
                createOrUpdate(ruleRealPath, data);
            }
            LOG.debug("[DataChangedListener] change path {} with data {}", ruleRealPath, data);
        }
    }

    /**
     * A namespace-scoped REFRESH is a full snapshot of the group. Upsert the payload first so
     * gateways never see an empty group, then remove the paths that are absent from the payload.
     */
    private <T> void refreshFlatGroup(final String namespaceId, final String groupPath, final List<T> changed,
                                      final Function<? super T, ? extends String> nameMapper,
                                      final BiFunction<String, String, String> pathBuilder) {
        final String rootPath = buildNamespaceGroupPath(namespaceId, groupPath);
        final List<String> existingNames = getChildren(rootPath);
        final Set<String> expectedNames = new HashSet<>();
        for (T data : changed) {
            final String name = nameMapper.apply(data);
            expectedNames.add(name);
            createOrUpdate(pathBuilder.apply(namespaceId, name), data);
        }
        if (Objects.isNull(existingNames)) {
            // this center cannot list children: keep the previous upsert-only behavior
            return;
        }
        for (String name : existingNames) {
            if (!expectedNames.contains(name)) {
                deleteNode(buildChildPath(rootPath, name));
            }
        }
    }

    private void refreshSelectorGroup(final String namespaceId, final List<SelectorData> changed, final boolean wholeNamespace) {
        final Map<String, Set<String>> expectedIds = new HashMap<>();
        for (SelectorData data : changed) {
            expectedIds.computeIfAbsent(data.getPluginName(), key -> new HashSet<>()).add(data.getId());
        }
        final String rootPath = buildNamespaceGroupPath(namespaceId, DefaultPathConstants.SELECTOR_PARENT);
        final List<String> existingPlugins = getChildren(rootPath);
        if (Objects.isNull(existingPlugins)) {
            // this center cannot list children: keep the previous delete-and-recreate behavior
            expectedIds.keySet().forEach(pluginName -> deletePathRecursive(
                    DefaultPathConstants.buildSelectorParentPath(namespaceId, pluginName)));
        }
        changed.forEach(data -> {
            synchronized (selectorSyncObject) {
                createOrUpdate(DefaultPathConstants.buildSelectorRealPath(namespaceId, data.getPluginName(), data.getId()), data);
            }
        });
        if (Objects.isNull(existingPlugins)) {
            return;
        }
        final Set<String> pluginNames = wholeNamespace ? new HashSet<>(existingPlugins) : new HashSet<>();
        pluginNames.addAll(expectedIds.keySet());
        for (String pluginName : pluginNames) {
            final String pluginPath = buildChildPath(rootPath, pluginName);
            final Set<String> ids = expectedIds.getOrDefault(pluginName, Collections.emptySet());
            final List<String> selectorIds = getChildren(pluginPath);
            for (String selectorId : selectorIds) {
                if (!ids.contains(selectorId)) {
                    deleteNode(buildChildPath(pluginPath, selectorId));
                    this.deleteSelectorRules(namespaceId, pluginName, selectorId);
                }
            }
            if (ids.isEmpty() && selectorIds.isEmpty()) {
                deletePathRecursive(pluginPath);
            }
        }
    }

    /**
     * Rules cannot outlive their selector, so drop them together with the stale selector.
     */
    private void deleteSelectorRules(final String namespaceId, final String pluginName, final String selectorId) {
        final String rulePluginPath = DefaultPathConstants.buildRuleParentPath(namespaceId, pluginName);
        final List<String> ruleNodes = getChildren(rulePluginPath);
        if (Objects.isNull(ruleNodes)) {
            return;
        }
        final String ruleNodePrefix = selectorId + DefaultPathConstants.SELECTOR_JOIN_RULE;
        ruleNodes.stream().filter(nodeName -> nodeName.startsWith(ruleNodePrefix))
                .forEach(nodeName -> deleteNode(buildChildPath(rulePluginPath, nodeName)));
    }

    private void refreshRuleGroup(final String namespaceId, final List<RuleData> changed, final boolean wholeNamespace) {
        final Map<String, Set<String>> expectedNames = new HashMap<>();
        for (RuleData data : changed) {
            final String ruleNodeName = data.getSelectorId() + DefaultPathConstants.SELECTOR_JOIN_RULE + data.getId();
            expectedNames.computeIfAbsent(data.getPluginName(), key -> new HashSet<>()).add(ruleNodeName);
        }
        final String rootPath = buildNamespaceGroupPath(namespaceId, DefaultPathConstants.RULE_PARENT);
        final List<String> existingPlugins = getChildren(rootPath);
        if (Objects.isNull(existingPlugins)) {
            // this center cannot list children: keep the previous delete-and-recreate behavior
            expectedNames.keySet().forEach(pluginName -> deletePathRecursive(
                    DefaultPathConstants.buildRuleParentPath(namespaceId, pluginName)));
        }
        changed.forEach(data -> {
            synchronized (ruleSyncObject) {
                createOrUpdate(DefaultPathConstants.buildRulePath(namespaceId, data.getPluginName(), data.getSelectorId(), data.getId()), data);
            }
        });
        if (Objects.isNull(existingPlugins)) {
            return;
        }
        final Set<String> pluginNames = wholeNamespace ? new HashSet<>(existingPlugins) : new HashSet<>();
        pluginNames.addAll(expectedNames.keySet());
        for (String pluginName : pluginNames) {
            final String pluginPath = buildChildPath(rootPath, pluginName);
            final Set<String> names = expectedNames.getOrDefault(pluginName, Collections.emptySet());
            final List<String> ruleNodes = getChildren(pluginPath);
            for (String ruleNodeName : ruleNodes) {
                if (!names.contains(ruleNodeName)) {
                    deleteNode(buildChildPath(pluginPath, ruleNodeName));
                }
            }
            if (names.isEmpty() && ruleNodes.isEmpty()) {
                deletePathRecursive(pluginPath);
            }
        }
    }

    /**
     * Plugins absent from the snapshot, enumerated before the snapshot overwrites the plugin listing.
     */
    private Set<String> stalePluginNames(final String namespaceId, final List<PluginData> changed) {
        final List<String> existingPlugins = getChildren(buildNamespaceGroupPath(namespaceId, DefaultPathConstants.PLUGIN_PARENT));
        if (Objects.isNull(existingPlugins)) {
            return Collections.emptySet();
        }
        final Set<String> currentPlugins = changed.stream().map(PluginData::getName).collect(Collectors.toSet());
        return existingPlugins.stream().filter(pluginName -> !currentPlugins.contains(pluginName)).collect(Collectors.toSet());
    }

    /**
     * Run one reconciliation step; a config center failure must not stop the remaining listeners.
     */
    private void reconcile(final String namespaceId, final String group, final Runnable action) {
        try {
            action.run();
        } catch (Exception e) {
            LOG.error("[DataChangedListener] {} refresh failed, namespaceId={}", group, namespaceId, e);
        }
    }

    private static String buildNamespaceGroupPath(final String namespaceId, final String groupPath) {
        return DefaultPathConstants.handlePathData(DefaultPathConstants.PATH_SEPARATOR + namespaceId + groupPath);
    }

    private static String buildChildPath(final String parentPath, final String childName) {
        return DefaultPathConstants.handlePathData(parentPath + DefaultPathConstants.PATH_SEPARATOR + childName);
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

    /**
     * Get the child names of a path, or null when this config center cannot list children.
     *
     * <p>A null result makes namespace REFRESH fall back to the behavior that existed before
     * namespace reconciliation, instead of silently doing nothing.
     *
     * @param path parent path
     * @return child names, or null when child listing is unsupported
     */
    public List<String> getChildren(final String path) {
        return null;
    }
}
