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
import org.apache.curator.framework.api.CuratorWatcher;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.Join;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.WatchedEvent;
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
            return Objects.nonNull(client.checkExists().forPath(key));
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
            this.client.getData().usingWatcher(new CuratorWatcher() {
                @Override
                public void process(final WatchedEvent watchedEvent) throws Exception {
                    if (Objects.nonNull(listener)) {
                        String path = Objects.isNull(watchedEvent.getPath()) ? "" : watchedEvent.getPath();
                        if (StringUtils.isNoneBlank(path)) {
                            client.getData().usingWatcher(this).forPath(path);
                            byte[] ret = client.getData().forPath(key);
                            String data = Objects.isNull(ret) ? null : new String(ret, StandardCharsets.UTF_8);
                            LOGGER.info("shenyu ZookeeperDiscoveryService onChange key={}", path);
                            listener.onChange(buildDataChangedEvent(path, data, watchedEvent));
                        }
                    }
                }
            }).forPath(key);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void register(final String key, final String value) {
        this.createOrUpdate(key, value, CreateMode.EPHEMERAL);
    }

    @Override
    public String getData(final String key) {
        try {
            byte[] ret = client.getData().forPath(key);
            return Objects.isNull(ret) ? null : new String(ret, StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private DataChangedEvent buildDataChangedEvent(final String key, final String value, final WatchedEvent watchedEvent) {
        DataChangedEvent.Event event = null;
        switch (watchedEvent.getType()) {
            case NodeCreated:
                event = DataChangedEvent.Event.ADDED;
                break;
            case NodeDeleted:
                event = DataChangedEvent.Event.DELETED;
                break;
            case NodeDataChanged:
                event = DataChangedEvent.Event.UPDATED;
                break;
            case NodeChildrenChanged:
            case DataWatchRemoved:
            case ChildWatchRemoved:
            default:
                event = DataChangedEvent.Event.IGNORED;
        }
        return new DataChangedEvent(key, value, event);
    }
}
