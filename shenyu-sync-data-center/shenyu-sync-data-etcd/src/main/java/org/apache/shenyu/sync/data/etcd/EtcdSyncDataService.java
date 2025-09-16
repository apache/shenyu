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

package org.apache.shenyu.sync.data.etcd;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import static org.apache.shenyu.common.constant.DefaultPathConstants.handlePathData;

import org.apache.shenyu.infra.etcd.client.EtcdClient;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.core.AbstractPathDataSyncService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;

/**
 * Data synchronize of etcd.
 */
public class EtcdSyncDataService extends AbstractPathDataSyncService {

    private static final Logger LOG = LoggerFactory.getLogger(EtcdSyncDataService.class);

    private final EtcdClient etcdClient;
    
    private final ShenyuConfig shenyuConfig;

    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param shenyuConfig          shenyuConfig
     * @param etcdClient           etcdClient
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public EtcdSyncDataService(final ShenyuConfig shenyuConfig,
                               final EtcdClient etcdClient,
                               final PluginDataSubscriber pluginDataSubscriber,
                               final List<MetaDataSubscriber> metaDataSubscribers,
                               final List<AuthDataSubscriber> authDataSubscribers,
                               final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                               final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.shenyuConfig = shenyuConfig;
        this.etcdClient = etcdClient;
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
        etcdClient.watchChildChange(
                registerPath,
            (updatePath, updateValue) -> super.event(configNamespace, updatePath, updateValue, registerPath, EventType.PUT),
            deletePath -> super.event(configNamespace, deletePath, null, registerPath, EventType.DELETE));
        try {
            // load all key
            final List<String> childrenKeys = etcdClient.getChildrenKeys(registerPath, "/");
            if (!ObjectUtils.isEmpty(childrenKeys)) {
                childrenKeys.forEach(nodePath -> {
                    // fix the update path
                    String updatePath = String.join(Constants.PATH_SEPARATOR, registerPath, nodePath);
                    final String nodeData = etcdClient.get(updatePath);
                    super.event(configNamespace, updatePath, nodeData, registerPath, EventType.PUT);
                });
            } else {
                LOG.info("etcd watcher path {} is empty", registerPath);
            }
        } catch (Exception e) {
            LOG.error("watcherData0 errors", e);
        }
    }

    @Override
    public void close() {
        if (Objects.nonNull(etcdClient)) {
            etcdClient.close();
        }
    }

}
