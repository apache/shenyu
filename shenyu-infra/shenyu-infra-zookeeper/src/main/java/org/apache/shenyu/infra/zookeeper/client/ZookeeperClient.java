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

package org.apache.shenyu.infra.zookeeper.client;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.api.CuratorWatcher;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.framework.state.ConnectionStateListener;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.CuratorCache;
import org.apache.curator.framework.recipes.cache.CuratorCacheListener;
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.apache.curator.utils.CloseableUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.infra.zookeeper.config.ZookeeperConfig;
import org.apache.zookeeper.CreateMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Zookeeper client.
 */
@SuppressWarnings("deprecation")
public class ZookeeperClient implements AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(ZookeeperClient.class);

    private final ZookeeperConfig config;

    private final CuratorFramework client;

    private final Map<String, CuratorCache> caches = new ConcurrentHashMap<>();

    private final Map<String, TreeCache> treeCaches = new ConcurrentHashMap<>();

    public ZookeeperClient(final ZookeeperConfig zookeeperConfig) {
        this.config = zookeeperConfig;
        ExponentialBackoffRetry retryPolicy = new ExponentialBackoffRetry(
                config.getBaseSleepTimeMilliseconds(),
                config.getMaxRetries(),
                config.getMaxSleepTimeMilliseconds());

        CuratorFrameworkFactory.Builder builder = CuratorFrameworkFactory.builder()
                .connectString(config.getUrl())
                .retryPolicy(retryPolicy)
                .connectionTimeoutMs(config.getConnectionTimeoutMilliseconds())
                .sessionTimeoutMs(config.getSessionTimeoutMilliseconds());

        if (StringUtils.isNotEmpty(config.getNamespace())) {
            builder.namespace(config.getNamespace());
        }

        if (StringUtils.isNotEmpty(config.getDigest())) {
            builder.authorization("digest", config.getDigest().getBytes(StandardCharsets.UTF_8));
        }

        this.client = builder.build();
        
        // Add connection state listener for better error handling and logging
        this.client.getConnectionStateListenable().addListener(new ConnectionStateListener() {
            @Override
            public void stateChanged(final CuratorFramework client, final ConnectionState newState) {
                switch (newState) {
                    case CONNECTED:
                        LOG.info("ZooKeeper client connected successfully to {}", config.getUrl());
                        break;
                    case RECONNECTED:
                        LOG.info("ZooKeeper client reconnected to {}", config.getUrl());
                        break;
                    case SUSPENDED:
                        LOG.warn("ZooKeeper client connection suspended to {}", config.getUrl());
                        break;
                    case LOST:
                        LOG.error("ZooKeeper client connection lost to {}", config.getUrl());
                        break;
                    default:
                        LOG.debug("ZooKeeper client state changed to {} for {}", newState, config.getUrl());
                        break;
                }
            }
        });
    }

    /**
     * start.
     */
    public void start() {
        this.client.start();
        try {
            // Use blockUntilConnected with timeout to avoid indefinite blocking
            // Use connection timeout + session timeout as maximum wait time
            int maxWaitTime = config.getConnectionTimeoutMilliseconds() 
                    + (Objects.nonNull(config.getSessionTimeoutMilliseconds()) ? config.getSessionTimeoutMilliseconds() : 60000);
            boolean connected = this.client.blockUntilConnected(
                    Math.min(maxWaitTime, 30000) / 1000, TimeUnit.SECONDS);
            if (!connected) {
                LOG.warn("ZooKeeper client failed to connect within timeout to {}", config.getUrl());
            } else {
                LOG.info("ZooKeeper client connected successfully to {}", config.getUrl());
            }
        } catch (InterruptedException e) {
            LOG.warn("Interrupted during zookeeper client starting.");
            Thread.currentThread().interrupt();
        }
    }

    /**
     * close.
     */
    @Override
    public void close() {
        // close all caches
        for (Map.Entry<String, CuratorCache> cache : caches.entrySet()) {
            CloseableUtils.closeQuietly(cache.getValue());
        }
        // close all tree caches
        for (Map.Entry<String, TreeCache> cache : treeCaches.entrySet()) {
            CloseableUtils.closeQuietly(cache.getValue());
        }
        // close client
        CloseableUtils.closeQuietly(client);
    }

    /**
     * get curator framework.
     *
     * @return curator framework client.
     */
    public CuratorFramework getClient() {
        return client;
    }

    /**
     * check if key exist.
     *
     * @param key zookeeper path
     * @return if exist.
     */
    public boolean isExist(final String key) {
        try {
            return Objects.nonNull(client.checkExists().forPath(key));
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * get from zk directly.
     *
     * @param key zookeeper path
     * @return value.
     */
    public String getDirectly(final String key) {
        try {
            byte[] ret = client.getData().forPath(key);
            return Objects.isNull(ret) ? null : new String(ret, StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * get value for specific key.
     *
     * @param key zookeeper path
     * @return value.
     */
    public String get(final String key) {
        CuratorCache cache = findFromCache(key);
        if (Objects.isNull(cache)) {
            return getDirectly(key);
        }
        ChildData data = cache.get(key).orElse(null);
        if (Objects.isNull(data)) {
            return getDirectly(key);
        }
        return Objects.isNull(data.getData()) ? null : new String(data.getData(), StandardCharsets.UTF_8);
    }

    /**
     * create or update key with value.
     *
     * @param key   zookeeper path key.
     * @param value string value.
     * @param mode  creation mode.
     */
    public void createOrUpdate(final String key, final String value, final CreateMode mode) {
        String val = StringUtils.isEmpty(value) ? "" : value;
        try {
            synchronized (ZookeeperClient.class) {
                if (Objects.nonNull(client.checkExists()) && Objects.nonNull(client.checkExists().forPath(key))) {
                    LOG.debug("path exists, update zookeeper key={} with value={}", key, val);
                    client.setData().forPath(key, val.getBytes(StandardCharsets.UTF_8));
                    return;
                }
                LOG.debug("path not exists, set zookeeper key={} with value={}", key, val);
                client.create().orSetData().creatingParentsIfNeeded().withMode(mode).forPath(key, val.getBytes(StandardCharsets.UTF_8));
            }
        } catch (Exception e) {
            LOG.error("create or update key with value error, key:{} value:{}", key, value, e);
            throw new ShenyuException(e);
        }
    }

    /**
     * create or update key with value.
     *
     * @param key   zookeeper path key.
     * @param value object value.
     * @param mode  creation mode.
     */
    public void createOrUpdate(final String key, final Object value, final CreateMode mode) {
        if (Objects.nonNull(value)) {
            String val = GsonUtils.getInstance().toJson(value);
            createOrUpdate(key, val, mode);
        } else {
            createOrUpdate(key, "", mode);
        }
    }

    /**
     * delete a node with specific key.
     *
     * @param key zookeeper path key.
     */
    public void delete(final String key) {
        try {
            client.delete().guaranteed().deletingChildrenIfNeeded().forPath(key);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * get children with specific key.
     *
     * @param key zookeeper key.
     * @return children node name.
     */
    public List<String> getChildren(final String key) {
        try {
            return client.getChildren().forPath(key);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * get created cache.
     * @param path path.
     * @return cache.
     */
    public CuratorCache getCache(final String path) {
        return caches.get(path);
    }

    /**
     * add new curator cache.
     * @param path path.
     * @param listeners listeners.
     * @return cache.
     */
    public CuratorCache addCache(final String path, final CuratorCacheListener... listeners) {
        CuratorCache cache = CuratorCache.build(client, path);
        caches.put(path, cache);
        if (ArrayUtils.isNotEmpty(listeners)) {
            for (CuratorCacheListener listener : listeners) {
                cache.listenable().addListener(listener);
            }
        }
        try {
            cache.start();
        } catch (Exception e) {
            throw new ShenyuException("failed to add curator cache.", e);
        }
        return cache;
    }

    /**
     * add new curator cache.
     * @param path path.
     * @param listeners listeners.
     * @return cache.
     */
    public CuratorCache addCuratorCache(final String path, final CuratorCacheListener... listeners) {
        return addCache(path, listeners);
    }

    /**
     * add new tree cache.
     * @param path path.
     * @param listeners listeners.
     * @return tree cache.
     * @deprecated Use {@link #addCuratorCache(String, CuratorCacheListener...)} instead.
     */
    @Deprecated
    public TreeCache addTreeCache(final String path, final TreeCacheListener... listeners) {
        TreeCache cache = TreeCache.newBuilder(client, path).build();
        treeCaches.put(path, cache);
        if (ArrayUtils.isNotEmpty(listeners)) {
            for (TreeCacheListener listener : listeners) {
                cache.getListenable().addListener(listener);
            }
        }
        try {
            cache.start();
        } catch (Exception e) {
            throw new ShenyuException("failed to add tree cache.", e);
        }
        return cache;
    }

    /**
     * get created tree cache.
     * @param path path.
     * @return tree cache.
     * @deprecated Use {@link #getCache(String)} instead.
     */
    @Deprecated
    public TreeCache getTreeCache(final String path) {
        return treeCaches.get(path);
    }

    /**
     * add children watcher.
     * @param key selectKey
     * @param curatorWatcher watcher
     * @return children List
     */
    public List<String> subscribeChildrenChanges(final String key, final CuratorWatcher curatorWatcher) {
        try {
            return client.getChildren().usingWatcher(curatorWatcher).forPath(key);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * find cache with  key.
     * @param key key.
     * @return cache.
     */
    private CuratorCache findFromCache(final String key) {
        for (Map.Entry<String, CuratorCache> cache : caches.entrySet()) {
            if (key.startsWith(cache.getKey())) {
                return cache.getValue();
            }
        }
        return null;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {

        private ZookeeperConfig config;

        /**
         * private constructor. not allow to create instance.
         */
        private Builder() {
        }

        public Builder config(final ZookeeperConfig config) {
            this.config = config;
            return this;
        }

        public ZookeeperClient build() {
            return new ZookeeperClient(config);
        }
    }
}
