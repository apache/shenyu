/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.cache;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.web.plugin.dubbo.ApplicationConfigCache;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.boot.CommandLineRunner;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * this cache data with zookeeper.
 *
 * @author xiaoyu
 */
public class ZookeeperSyncCache extends CommonCacheHandler implements CommandLineRunner, DisposableBean {

    private final ZkClient zkClient;

    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param zkClient the zk client
     */
    public ZookeeperSyncCache(final ZkClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    public void run(final String... args) {
        watcherData();
        watchAppAuth();
        watchMetaData();
    }

    private void watcherData() {
        final String pluginParent = ZkPathConstants.PLUGIN_PARENT;
        if (!zkClient.exists(pluginParent)) {
            zkClient.createPersistent(pluginParent, true);
        }
        List<String> pluginZKs = zkClient.getChildren(ZkPathConstants.buildPluginParentPath());
        for (String pluginName : pluginZKs) {
            loadPlugin(pluginName);
        }
        zkClient.subscribeChildChanges(pluginParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String pluginName : currentChildren) {
                    loadPlugin(pluginName);
                }
            }
        });
    }

    private void loadPlugin(final String pluginName) {
        watcherPlugin(pluginName);
        watcherSelector(pluginName);
        watcherRule(pluginName);
    }

    private void watcherPlugin(final String pluginName) {
        String pluginPath = ZkPathConstants.buildPluginPath(pluginName);
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        PluginData data = zkClient.readData(pluginPath);
        Optional.ofNullable(data).ifPresent(d -> {
            configPlugin(Collections.singletonList(d));
            PLUGIN_MAP.put(data.getName(), data);
        });
        zkClient.subscribeDataChanges(pluginPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data)
                        .ifPresent(d -> {
                            PluginData pluginData = (PluginData) d;
                            configPlugin(Collections.singletonList(pluginData));
                            PLUGIN_MAP.put(pluginData.getName(), pluginData);
                        });
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                PLUGIN_MAP.remove(pluginName);
            }
        });
    }

    private void watcherSelector(final String pluginName) {
        String selectorParentPath =
                ZkPathConstants.buildSelectorParentPath(pluginName);

        if (!zkClient.exists(selectorParentPath)) {
            zkClient.createPersistent(selectorParentPath, true);
        }
        final List<String> childrenList = zkClient.getChildren(selectorParentPath);

        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(selectorParentPath, children);
                setSelectorData(realPath);
                subscribeSelectorDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(selectorParentPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(addPath -> {
                    final String realPath = buildRealPath(parentPath, addPath);
                    setSelectorData(realPath);
                    return realPath;
                }).forEach(this::subscribeSelectorDataChanges);

            }
        });
    }

    private void watcherRule(final String pluginName) {
        final String ruleParent = ZkPathConstants.buildRuleParentPath(pluginName);
        if (!zkClient.exists(ruleParent)) {
            zkClient.createPersistent(ruleParent, true);
        }
        final List<String> childrenList = zkClient.getChildren(ruleParent);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(ruleParent, children);
                setRuleData(realPath);
                subscribeRuleDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(ruleParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                //获取新增的节点数据，并对该节点进行订阅
                addSubscribePath.stream().map(addPath -> {
                    final String realPath = buildRealPath(parentPath, addPath);
                    setRuleData(realPath);
                    return realPath;
                }).forEach(this::subscribeRuleDataChanges);
            }
        });
    }

    private void subscribeSelectorDataChanges(final String path) {
        zkClient.subscribeDataChanges(path, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data).ifPresent(d -> cacheSelectorData((SelectorData) d));
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                //规定路径 key-id key为selectorId, id为规则id
                final String id = dataPath.substring(dataPath.lastIndexOf("/") + 1);
                final String str = dataPath.substring(ZkPathConstants.SELECTOR_PARENT.length());
                final String key = str.substring(1, str.length() - id.length() - 1);
                Optional.of(key).ifPresent(k -> {
                    final List<SelectorData> selectorDataList = SELECTOR_MAP.get(k);
                    selectorDataList.removeIf(e -> e.getId().equals(id));
                    UpstreamCacheManager.removeByKey(k);
                });
            }
        });
    }

    private void setSelectorData(final String realPath) {
        final SelectorData selectorData = zkClient.readData(realPath);
        Optional.ofNullable(selectorData)
                .ifPresent(s -> cacheSelectorData(selectorData));
    }

    private void setRuleData(final String rulePath) {
        final RuleData ruleData = zkClient.readData(rulePath);
        Optional.ofNullable(ruleData)
                .ifPresent(this::cacheRuleData);
    }

    private void subscribeRuleDataChanges(final String path) {
        zkClient.subscribeDataChanges(path, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data).ifPresent(d -> cacheRuleData((RuleData) d));
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                final List<String> list = Lists.newArrayList(Splitter.on(ZkPathConstants.SELECTOR_JOIN_RULE).split(dataPath.substring(dataPath.lastIndexOf("/") + 1)));
                final String selectorId = list.get(0);
                final String ruleId = list.get(1);
                Optional.ofNullable(selectorId).ifPresent(k -> {
                    UpstreamCacheManager.removeByKey(selectorId);
                    final List<RuleData> ruleDataList = RULE_MAP.get(selectorId);
                    ruleDataList.removeIf(rule -> rule.getId().equals(ruleId));
                });
            }
        });
    }

    private void watchAppAuth() {
        final String appAuthParent = ZkPathConstants.APP_AUTH_PARENT;
        if (!zkClient.exists(appAuthParent)) {
            zkClient.createPersistent(appAuthParent, true);
        }
        final List<String> childrenList = zkClient.getChildren(appAuthParent);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(appAuthParent, children);
                setAuthData(realPath);
                subscribeAppAuthDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(appAuthParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(children -> {
                    final String realPath = buildRealPath(parentPath, children);
                    setAuthData(realPath);
                    return realPath;
                }).forEach(this::subscribeAppAuthDataChanges);
            }
        });
    }

    private void watchMetaData() {
        final String metaDataPath = ZkPathConstants.META_DATA;
        if (!zkClient.exists(metaDataPath)) {
            zkClient.createPersistent(metaDataPath, true);
        }
        final List<String> childrenList = zkClient.getChildren(metaDataPath);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(metaDataPath, children);
                setMetaData(realPath);
                subscribeMetaDataChanges(realPath);
            });
        }

        zkClient.subscribeChildChanges(metaDataPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(children -> {
                    final String realPath = buildRealPath(parentPath, children);
                    setMetaData(realPath);
                    return realPath;
                }).forEach(this::subscribeMetaDataChanges);
            }
        });
    }

    private void subscribeMetaDataChanges(final String realPath) {
        zkClient.subscribeDataChanges(realPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable((MetaData) data)
                        .ifPresent(metaData -> {
                            initDubboRef(Collections.singletonList(metaData));
                            META_DATA.put(metaData.getPath(), metaData);
                        });
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                final MetaData metaData = zkClient.readData(dataPath);
                Optional.ofNullable(metaData).ifPresent(d -> {
                    META_DATA.remove(d.getPath());
                    ApplicationConfigCache.getInstance().invalidate(d.getServiceName());
                });
            }
        });
    }

    private void setMetaData(final String realPath) {
        final MetaData metaData = zkClient.readData(realPath);
        initDubboRef(Collections.singletonList(metaData));
        Optional.ofNullable(metaData)
                .ifPresent(dto -> META_DATA.put(dto.getPath(), dto));
    }

    private void setAuthData(final String realPath) {
        final AppAuthData appAuthData = zkClient.readData(realPath);
        Optional.ofNullable(appAuthData)
                .ifPresent(dto -> AUTH_MAP.put(dto.getAppKey(), dto));
    }

    private void subscribeAppAuthDataChanges(final String realPath) {
        zkClient.subscribeDataChanges(realPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data)
                        .ifPresent(o -> AUTH_MAP.put(((AppAuthData) o).getAppKey(), (AppAuthData) o));
            }

            @Override
            public void handleDataDeleted(final String dataPath) {
                final String key = dataPath.substring(ZkPathConstants.APP_AUTH_PARENT.length() + 1);
                AUTH_MAP.remove(key);
            }
        });
    }

    private List<String> addSubscribePath(final List<String> alreadyChildren, final List<String> currentChildren) {
        if (CollectionUtils.isEmpty(alreadyChildren)) {
            return currentChildren;
        }
        return currentChildren.stream().filter(current -> alreadyChildren.stream().noneMatch(current::equals)).collect(Collectors.toList());
    }

    private String buildRealPath(final String parent, final String children) {
        return parent + "/" + children;
    }

    @Override
    public void destroy() {
        zkClient.close();
    }
}
