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

package org.apache.shenyu.common.client.zookeeper;

import com.google.common.base.Charsets;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.api.transaction.CuratorOp;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.apache.curator.utils.CloseableUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.KeeperException;

import java.nio.charset.StandardCharsets;
import java.util.List;

public class ZookeeperClient {

    private ZookeeperConfig config;

    private CuratorFramework client;

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
    }

    /**
     * start.
     */
    public void close() {
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
            client.delete().deletingChildrenIfNeeded().forPath(key);
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
}
