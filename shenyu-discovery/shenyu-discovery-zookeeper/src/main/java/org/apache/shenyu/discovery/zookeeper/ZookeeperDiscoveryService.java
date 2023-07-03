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

package org.apache.shenyu.discovery.zookeeper;

import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.Join;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.data.Stat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * The type Zookeeper for shenyu discovery service.
 */
@Join
public class ZookeeperDiscoveryService implements ShenyuDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperDiscoveryService.class);

    private CuratorFramework client;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    private final Map<String, TreeCache> cacheMap = new HashMap<>();

    @Override
    public void init(final DiscoveryConfig config) {
        String baseSleepTimeMilliseconds = config.getProps().getProperty("baseSleepTimeMilliseconds", "1000");
        String maxRetries = config.getProps().getProperty("maxRetries", "3");
        String maxSleepTimeMilliseconds = config.getProps().getProperty("maxSleepTimeMilliseconds", "1000");
        String connectionTimeoutMilliseconds = config.getProps().getProperty("connectionTimeoutMilliseconds", "1000");
        String sessionTimeoutMilliseconds = config.getProps().getProperty("sessionTimeoutMilliseconds", "1000");
        String digest = config.getProps().getProperty("digest", null);
        ExponentialBackoffRetry retryPolicy = new ExponentialBackoffRetry(Integer.parseInt(baseSleepTimeMilliseconds), Integer.parseInt(maxRetries), Integer.parseInt(maxSleepTimeMilliseconds));
        CuratorFrameworkFactory.Builder builder = CuratorFrameworkFactory.builder()
                .connectString(config.getServerList())
                .retryPolicy(retryPolicy)
                .connectionTimeoutMs(Integer.parseInt(connectionTimeoutMilliseconds))
                .sessionTimeoutMs(Integer.parseInt(sessionTimeoutMilliseconds))
                .namespace(config.getName());
        if (StringUtils.isNoneBlank(digest)) {
            builder.authorization("digest", digest.getBytes(StandardCharsets.UTF_8));
        }
        this.client = builder.build();
        this.start();
    }

    private void start() {
        this.client.getConnectionStateListenable().addListener((c, newState) -> {
            if (newState == ConnectionState.RECONNECTED) {
                nodeDataMap.forEach((k, v) -> {
                    if (!this.isExist(k)) {
                        this.createOrUpdate(k, v, CreateMode.EPHEMERAL);
                        LOGGER.info("zookeeper client register instance success: key={}|value={}", k, v);
                    }
                });
            }
        });
        this.client.start();
        try {
            this.client.blockUntilConnected();
        } catch (InterruptedException e) {
            throw new ShenyuException(e);
        }
    }

    private boolean isExist(final String key) {
        try {
            return null != client.checkExists().forPath(key);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private void createOrUpdate(final String key, final String value, final CreateMode mode) {
        String val = StringUtils.isEmpty(value) ? "" : value;
        try {
            this.client.create().orSetData().creatingParentsIfNeeded().withMode(mode).forPath(key, val.getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watcher(final String key, final DataChangedEventListener listener) {
        try {
            TreeCache treeCache = new TreeCache(client, key);
            TreeCacheListener treeCacheListener = (curatorFramework, event) -> {
                ChildData data = event.getData();
                DiscoveryDataChangedEvent dataChangedEvent;
                if (Objects.nonNull(data) && Objects.nonNull(data.getData())) {
                    String currentPath = data.getPath();
                    String parentPath = currentPath.substring(0, currentPath.lastIndexOf("/"));
                    String parentData = this.getData(parentPath);
                    String currentData = new String(data.getData(), StandardCharsets.UTF_8);
                    String resultData = parentData + "|" + currentData;
                    LOGGER.info("shenyu find resultData ={}", resultData);
                    Stat stat = data.getStat();
                    boolean isEphemeral = Objects.nonNull(stat) && stat.getEphemeralOwner() > 0;
                    if (!isEphemeral) {
                        LOGGER.info("shenyu Ignore non-ephemeral node changes");
                        return;
                    }
                    switch (event.getType()) {
                        case NODE_ADDED:
                            dataChangedEvent = new DiscoveryDataChangedEvent(currentPath, resultData, DiscoveryDataChangedEvent.Event.ADDED);
                            break;
                        case NODE_UPDATED:
                            dataChangedEvent = new DiscoveryDataChangedEvent(currentPath, resultData, DiscoveryDataChangedEvent.Event.UPDATED);
                            break;
                        case NODE_REMOVED:
                            dataChangedEvent = new DiscoveryDataChangedEvent(currentPath, resultData, DiscoveryDataChangedEvent.Event.DELETED);
                            break;
                        default:
                            dataChangedEvent = new DiscoveryDataChangedEvent(currentPath, resultData, DiscoveryDataChangedEvent.Event.IGNORED);
                            break;
                    }
                    listener.onChange(dataChangedEvent);
                }
            };
            treeCache.getListenable().addListener(treeCacheListener);
            treeCache.start();
            cacheMap.put(key, treeCache);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void unWatcher(final String key) {
        if (cacheMap.containsKey(key)) {
            cacheMap.remove(key).close();
        }
    }

    @Override
    public void register(final String key, final String value) {
        this.createOrUpdate(key, value, CreateMode.PERSISTENT);
    }

    @Override
    public String getData(final String key) {
        try {
            TreeCache treeCache = cacheMap.get(key);
            if (Objects.isNull(treeCache)) {
                return null;
            }
            ChildData currentData = treeCache.getCurrentData(key);
            byte[] ret = currentData.getData();
            return Objects.isNull(ret) ? null : new String(ret, StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void shutdown() {
        try {
            //close treeCache
            for (String key : cacheMap.keySet()) {
                cacheMap.get(key).close();
            }
            client.close();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }

    }
}
