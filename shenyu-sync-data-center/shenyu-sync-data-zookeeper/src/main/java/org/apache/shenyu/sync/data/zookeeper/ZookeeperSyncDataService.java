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
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.core.AbstractPathDataSyncService;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

import static org.apache.shenyu.common.constant.DefaultPathConstants.handlePathData;

/**
 * this cache data with zookeeper.
 */
public class ZookeeperSyncDataService extends AbstractPathDataSyncService {

    private final ZookeeperClient zkClient;
    
    private final ShenyuConfig shenyuConfig;

    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param shenyuConfig         the shenyu config
     * @param zkClient             the zk client
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     * @param proxySelectorDataSubscribers the proxy selector data subscribers
     * @param discoveryUpstreamDataSubscribers the discovery upstream data subscribers
     */
    public ZookeeperSyncDataService(final ShenyuConfig shenyuConfig,
                                    final ZookeeperClient zkClient,
                                    final PluginDataSubscriber pluginDataSubscriber,
                                    final List<MetaDataSubscriber> metaDataSubscribers,
                                    final List<AuthDataSubscriber> authDataSubscribers,
                                    final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                    final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.zkClient = zkClient;
        this.shenyuConfig = shenyuConfig;
        watcherData();
    }

    private void watcherData() {
        String configNamespace = Constants.PATH_SEPARATOR + shenyuConfig.getNamespace();
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.PLUGIN_PARENT)));
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.SELECTOR_PARENT)));
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.RULE_PARENT)));
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.PROXY_SELECTOR)));
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.DISCOVERY_UPSTREAM)));
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.APP_AUTH_PARENT)));
        watcherData0(handlePathData(String.join(Constants.PATH_SEPARATOR, configNamespace, DefaultPathConstants.META_DATA)));
    }

    private void watcherData0(final String registerPath) {
        String configNamespace = Constants.PATH_SEPARATOR + shenyuConfig.getNamespace();
        zkClient.addCache(registerPath, (curatorFramework, treeCacheEvent) -> {
            ChildData childData = treeCacheEvent.getData();
            if (Objects.isNull(childData)) {
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
            if (!StringUtils.containsIgnoreCase(path, configNamespace)) {
                return;
            }

            EventType eventType = treeCacheEvent.getType().equals(TreeCacheEvent.Type.NODE_REMOVED) ? EventType.DELETE : EventType.PUT;
            final String updateData = Objects.nonNull(childData.getData()) ? new String(childData.getData(), StandardCharsets.UTF_8) : null;
            this.event(configNamespace, path, updateData, registerPath, eventType);
        });
    }

    @Override
    public void close() {
        if (Objects.nonNull(zkClient)) {
            zkClient.close();
        }
    }

}
