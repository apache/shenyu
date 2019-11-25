/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.remoting.zookeeper;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.retry.RetryNTimes;
import org.apache.zookeeper.CreateMode;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.common.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * ZookeeperClient
 * zookeeper The relevant client to process.
 *
 * @author sixh
 * @see URL zookeeper://192.168.1.2:2181?cluster=192.168.1.3:2181,192.168.1.4:2181
 */
public class ZookeeperClient {
    private Logger logger = LoggerFactory.getLogger(ZookeeperClient.class);

    private final CuratorFramework client;

    private static final String URL_CLUSTER_KEY = "cluster";

    private Set<ZookeeperStatusCallback> callbacks = new HashSet<>();

    /**
     * Instantiates a new Zookeeper client.
     *
     * @param url the url.
     */
    public ZookeeperClient(URL url) {
        String host = url.getHost();
        Integer port = url.getPort();
        List<String> cluster = getCluster(url);
        cluster.add(host + Constants.COLONS + port);
        String connectString = Joiner.on(Constants.URL_SPLIT_SYMBOL_KEY).join(cluster);
        CuratorFrameworkFactory.Builder builder = CuratorFrameworkFactory.builder()
                .connectString(connectString)
                .retryPolicy(new RetryNTimes(1, 1000))
                .connectionTimeoutMs(5000);
        client = builder.build();
        client.getConnectionStateListenable()
                .addListener((curatorFramework, connectionState) -> {
                    if (!callbacks.isEmpty()) {
                        callbacks.forEach(callback -> callback.callback(connectionState.ordinal()));
                    }
                });
        client.start();
    }

    /**
     * Status change.
     *
     * @param callback the callback.
     */
    public void statusChange(ZookeeperStatusCallback callback) {
        callbacks.add(callback);
    }

    /**
     * Create.
     *
     * @param path      the path
     * @param ephemeral the ephemeral.
     */
    public void create(String path, boolean ephemeral) {
        if (!ephemeral && checkExists(path)) {
            return;
        }
        int i = path.lastIndexOf(Constants.BASE_URL_PATH_KEY);
        if (i > 0) {
            create(path.substring(0, i), false);
        }
        if (ephemeral) {
            createEphemeral(path);
        } else {
            createPersistent(path);
        }
    }

    /**
     * Delete.
     *
     * @param toPath the to path.
     */
    public void delete(String toPath) {
        try {
            client.delete().forPath(toPath);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Query zookeeper node data according to pat.
     *
     * @param path the path
     * @return node json or string data.
     */
    public String getNodeData(String path) {
        try {
            boolean b = checkExists(path);
            if (b) {
                byte[] bytes = client.getData().forPath(path);
                if (bytes == null || bytes.length <= 0) {
                    return "";
                }
                return new String(bytes, StandardCharsets.UTF_8);
            } else {
                return "";
            }
        } catch (Exception e) {
            logger.warn("get {} node data error", path, e);
            return "";
        }
    }

    /**
     * Gets cluster to getParameters.
     *
     * @return the cluster
     */
    private List<String> getCluster(URL url) {
        String cluster = url.getParameters().get(URL_CLUSTER_KEY);
        ArrayList<String> lists = new ArrayList<>();
        if (StringUtils.isBlank(cluster)) {
            return lists;
        }
        List<String> splitToList = Splitter.on(Constants.URL_SPLIT_SYMBOL_KEY).splitToList(cluster);
        lists.addAll(splitToList);
        return lists;
    }

    private void createEphemeral(String path) {
        try {
            client.create().withMode(CreateMode.EPHEMERAL).forPath(path);
        } catch (Exception ignored) {
        }
    }

    private void createPersistent(String path) {
        try {
            client.create().withMode(CreateMode.PERSISTENT).forPath(path);
        } catch (Exception ignored) {
        }
    }

    private boolean checkExists(String path) {
        try {
            return client.checkExists().forPath(path) != null;
        } catch (Exception e) {
            return false;
        }
    }


}
