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

package org.apache.shenyu.sync.data.zookeeper;

import com.google.common.base.Strings;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.core.AbstractNodeDataSyncService;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * this cache data with zookeeper.
 */
public class ZookeeperSyncDataService extends AbstractNodeDataSyncService {

    private final ZookeeperClient zkClient;

    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param zkClient             the zk client
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public ZookeeperSyncDataService(final ZookeeperClient zkClient,
                                    final PluginDataSubscriber pluginDataSubscriber,
                                    final List<MetaDataSubscriber> metaDataSubscribers,
                                    final List<AuthDataSubscriber> authDataSubscribers,
                                    final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                    final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.zkClient = zkClient;
        watcherData();
    }

    private void watcherData() {
        watcherData0(DefaultPathConstants.PLUGIN_PARENT);
        watcherData0(DefaultPathConstants.SELECTOR_PARENT);
        watcherData0(DefaultPathConstants.RULE_PARENT);
        watcherData0(DefaultPathConstants.PROXY_SELECTOR);
        watcherData0(DefaultPathConstants.DISCOVERY_UPSTREAM);
        watcherData0(DefaultPathConstants.APP_AUTH_PARENT);
        watcherData0(DefaultPathConstants.META_DATA);
    }

    private void watcherData0(final String registerPath) {
        zkClient.addCache(registerPath, (curatorFramework, treeCacheEvent) -> {
            ChildData childData = treeCacheEvent.getData();
            if (null == childData) {
                return;
            }
            String path = childData.getPath();
            if (Strings.isNullOrEmpty(path)) {
                return;
            }
            // if not uri register path, return.
            if (!path.contains(registerPath)) {
                return;
            }

            EventType eventType = treeCacheEvent.getType().equals(TreeCacheEvent.Type.NODE_REMOVED) ? EventType.DELETE : EventType.PUT;
            final String updateData = childData.getData() != null ? new String(childData.getData(), StandardCharsets.UTF_8) : null;
            this.event(path, updateData, registerPath, eventType);
        });
    }

    @Override
    public void close() {
        if (Objects.nonNull(zkClient)) {
            zkClient.close();
        }
    }

}
