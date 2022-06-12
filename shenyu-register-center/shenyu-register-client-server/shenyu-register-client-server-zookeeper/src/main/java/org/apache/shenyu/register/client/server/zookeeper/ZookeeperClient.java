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

package org.apache.shenyu.register.client.server.zookeeper;

import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.CuratorCache;
import org.apache.curator.framework.recipes.cache.CuratorCacheListener;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.apache.curator.utils.CloseableUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.zookeeper.CreateMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class ZookeeperClient {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperClient.class);

    private final ZookeeperConfig config;

    private final CuratorFramework client;

    private final Map<String, CuratorCache> caches = new ConcurrentHashMap<>();

    public ZookeeperClient(final ZookeeperConfig zookeeperConfig) {
        this.config = zookeeperConfig;
        ExponentialBackoffRetry retryPolicy = new ExponentialBackoffRetry(config.getBaseSleepTimeMilliseconds(), config.getMaxRetries(), config.getMaxSleepTimeMilliseconds());

        CuratorFrameworkFactory.Builder builder = CuratorFrameworkFactory.builder()
                .connectString(config.getServerLists())
                .retryPolicy(retryPolicy)
                .connectionTimeoutMs(config.getConnectionTimeoutMilliseconds())
                .sessionTimeoutMs(config.getSessionTimeoutMilliseconds())
                .namespace(config.getNamespace());

        if (!StringUtils.isEmpty(config.getDigest())) {
            builder.authorization("digest", config.getDigest().getBytes(StandardCharsets.UTF_8));
        }

        this.client = builder.build();
    }

    /**
     * start.
     */
    public void start() {
        this.client.start();
        try {
            this.client.blockUntilConnected();
        } catch (InterruptedException e) {
            LOGGER.warn("Interrupted during zookeeper client starting.");
            Thread.currentThread().interrupt();
        }
    }

    /**
     * start.
     */
    public void close() {
        // close all caches
        for (Map.Entry<String, CuratorCache> cache : caches.entrySet()) {
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
            return null != client.checkExists().forPath(key);
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
        CuratorCache cache = findFromcache(key);
        if (Objects.isNull(cache)) {
            return getDirectly(key);
        }
        Optional<ChildData> data = cache.get(key);
        return data.map(childData -> new String(childData.getData(), StandardCharsets.UTF_8)).orElseGet(() -> getDirectly(key));
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
            client.create().orSetData().creatingParentsIfNeeded().withMode(mode).forPath(key, val.getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
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
        if (value != null) {
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
        if (listeners != null && listeners.length > 0) {
            for (CuratorCacheListener listener : listeners) {
                cache.listenable().addListener(listener);
            }
        }
        cache.start();
        return cache;
    }

    /**
     * find cache with  key.
     * @param key key.
     * @return cache.
     */
    private CuratorCache findFromcache(final String key) {
        for (Map.Entry<String, CuratorCache> cache : caches.entrySet()) {
            if (key.startsWith(cache.getKey())) {
                return cache.getValue();
            }
        }
        return null;
    }
}
