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

package org.dromara.soul.sync.data.zookeeper;

import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.sync.data.api.SyncDataService;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * this cache data with zookeeper.
 *
 * @author xiaoyu
 */
public class ZookeeperSyncDataService implements SyncDataService, AutoCloseable {
    
    private final ZkClient zkClient;
    
    private final Map<String, PluginDataSubscriber> pluginDataSubscriberMap;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param zkClient the zk client
     */
    public ZookeeperSyncDataService(final ZkClient zkClient, final List<PluginDataSubscriber> pluginDataSubscribers,
                                    final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers) {
        this.zkClient = zkClient;
        pluginDataSubscriberMap = pluginDataSubscribers.stream().collect(Collectors.toMap(PluginDataSubscriber::pluginNamed, e -> e));
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
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
            watcherAll(pluginName);
        }
        zkClient.subscribeChildChanges(pluginParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                for (String pluginName : currentChildren) {
                    watcherAll(pluginName);
                }
            }
        });
    }
    
    private void watcherAll(final String pluginName) {
        watcherPlugin(pluginName);
        watcherSelector(pluginName);
        watcherRule(pluginName);
    }
    
    private void watcherPlugin(final String pluginName) {
        String pluginPath = ZkPathConstants.buildPluginPath(pluginName);
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        PluginData pluginData = zkClient.readData(pluginPath);
        Optional.ofNullable(pluginData).flatMap(data -> Optional.ofNullable(pluginDataSubscriberMap.get(pluginName))).ifPresent(e -> e.onSubscribe(pluginData));
        zkClient.subscribeDataChanges(pluginPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                Optional.ofNullable(data)
                        .ifPresent(d -> Optional.ofNullable(pluginDataSubscriberMap.get(pluginName)).ifPresent(e -> e.onSubscribe((PluginData) d)));
            }
            
            @Override
            public void handleDataDeleted(final String dataPath) {
                final PluginData data = zkClient.readData(dataPath);
                Optional.ofNullable(data)
                        .ifPresent(d -> Optional.ofNullable(pluginDataSubscriberMap.get(pluginName)).ifPresent(e -> e.unSubscribe((PluginData) d)));
            }
        });
    }
    
    private void watcherSelector(final String pluginName) {
        String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginName);
        if (!zkClient.exists(selectorParentPath)) {
            zkClient.createPersistent(selectorParentPath, true);
        }
        final List<String> childrenList = zkClient.getChildren(selectorParentPath);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(selectorParentPath, children);
                cacheSelectorData(zkClient.readData(realPath));
                subscribeSelectorDataChanges(realPath);
            });
        }
        
        zkClient.subscribeChildChanges(selectorParentPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(addPath -> {
                    String realPath = buildRealPath(parentPath, addPath);
                    cacheSelectorData(zkClient.readData(realPath));
                    return realPath;
                }).forEach(this::subscribeSelectorDataChanges);
                
            }
        });
    }
    
    private void watcherRule(final String pluginName) {
        String ruleParent = ZkPathConstants.buildRuleParentPath(pluginName);
        if (!zkClient.exists(ruleParent)) {
            zkClient.createPersistent(ruleParent, true);
        }
        List<String> childrenList = zkClient.getChildren(ruleParent);
        if (CollectionUtils.isNotEmpty(childrenList)) {
            childrenList.forEach(children -> {
                String realPath = buildRealPath(ruleParent, children);
                cacheRuleData(zkClient.readData(realPath));
                subscribeRuleDataChanges(realPath);
            });
        }
        
        zkClient.subscribeChildChanges(ruleParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                //获取新增的节点数据，并对该节点进行订阅
                addSubscribePath.stream().map(addPath -> {
                    String realPath = buildRealPath(parentPath, addPath);
                    cacheRuleData(zkClient.readData(realPath));
                    return realPath;
                }).forEach(this::subscribeRuleDataChanges);
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
                cacheAuthData(zkClient.readData(realPath));
                subscribeAppAuthDataChanges(realPath);
            });
        }
        
        zkClient.subscribeChildChanges(appAuthParent, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(children -> {
                    final String realPath = buildRealPath(parentPath, children);
                    cacheAuthData(zkClient.readData(realPath));
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
                cacheMetaData(zkClient.readData(realPath));
                subscribeMetaDataChanges(realPath);
            });
        }
        
        zkClient.subscribeChildChanges(metaDataPath, (parentPath, currentChildren) -> {
            if (CollectionUtils.isNotEmpty(currentChildren)) {
                final List<String> addSubscribePath = addSubscribePath(childrenList, currentChildren);
                addSubscribePath.stream().map(children -> {
                    final String realPath = buildRealPath(parentPath, children);
                    cacheMetaData(zkClient.readData(realPath));
                    return realPath;
                }).forEach(this::subscribeMetaDataChanges);
            }
        });
    }
    
    private void subscribeSelectorDataChanges(final String path) {
        zkClient.subscribeDataChanges(path, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                cacheSelectorData((SelectorData) data);
            }
            
            @Override
            public void handleDataDeleted(final String dataPath) {
                unCacheSelectorData(zkClient.readData(dataPath));
            }
        });
    }
    
    private void subscribeRuleDataChanges(final String path) {
        zkClient.subscribeDataChanges(path, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                cacheRuleData((RuleData) data);
            }
            
            @Override
            public void handleDataDeleted(final String dataPath) {
                unCacheRuleData(zkClient.readData(dataPath));
            }
        });
    }
    
    private void subscribeAppAuthDataChanges(final String realPath) {
        zkClient.subscribeDataChanges(realPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                cacheAuthData((AppAuthData) data);
            }
            
            @Override
            public void handleDataDeleted(final String dataPath) {
                unCacheAuthData(zkClient.readData(dataPath));
            }
        });
    }
    
    private void subscribeMetaDataChanges(final String realPath) {
        zkClient.subscribeDataChanges(realPath, new IZkDataListener() {
            @Override
            public void handleDataChange(final String dataPath, final Object data) {
                cacheMetaData((MetaData) data);
            }
            
            @Override
            public void handleDataDeleted(final String dataPath) {
                unCacheMetaData(zkClient.readData(dataPath));
            }
        });
    }
    
    
    private void cacheSelectorData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriberMap.get(selectorData.getPluginName())).ifPresent(e -> e.onSelectorSubscribe(data)));
    }
    
    private void unCacheSelectorData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriberMap.get(selectorData.getPluginName())).ifPresent(e -> e.unSelectorSubscribe(data)));
    }
    
    private void cacheRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriberMap.get(ruleData.getPluginName())).ifPresent(e -> e.onRuleSubscribe(data)));
    }
    
    private void unCacheRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriberMap.get(ruleData.getPluginName())).ifPresent(e -> e.unRuleSubscribe(data)));
    }
    
    private void cacheAuthData(final AppAuthData appAuthData) {
        Optional.ofNullable(appAuthData).ifPresent(data -> authDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }
    
    private void unCacheAuthData(final AppAuthData appAuthData) {
        Optional.ofNullable(appAuthData).ifPresent(data -> authDataSubscribers.forEach(e -> e.unSubscribe(data)));
    }
    
    private void cacheMetaData(final MetaData metaData) {
        Optional.ofNullable(metaData).ifPresent(data -> metaDataSubscribers.forEach(e -> e.onSubscribe(metaData)));
    }
    
    private void unCacheMetaData(final MetaData metaData) {
        Optional.ofNullable(metaData).ifPresent(data -> metaDataSubscribers.forEach(e -> e.unSubscribe(metaData)));
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
    public void close() {
        zkClient.close();
    }
}
