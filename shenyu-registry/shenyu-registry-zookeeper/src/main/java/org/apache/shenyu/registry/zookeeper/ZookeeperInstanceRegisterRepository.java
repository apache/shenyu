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

package org.apache.shenyu.registry.zookeeper;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.google.gson.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.framework.api.CuratorWatcher;
import org.apache.curator.framework.recipes.cache.CuratorCache;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.apache.shenyu.spi.Join;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.data.Stat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type Zookeeper instance register repository.
 */
@Join
public class ZookeeperInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperInstanceRegisterRepository.class);

    private ZookeeperClient client;

    private String watchPath;

    private final Map<String, String> nodeDataMap = new HashMap<>();

    private final Multimap<String, CuratorCache> cacheMap = ArrayListMultimap.create();

    private final Map<String, List<InstanceEntity>> watcherInstanceRegisterMap = new HashMap<>();

    @Override
    public void init(final RegisterConfig config) {
        Properties props = config.getProps();
        int sessionTimeout = Integer.parseInt(props.getProperty("sessionTimeout", "3000"));
        int connectionTimeout = Integer.parseInt(props.getProperty("connectionTimeout", "3000"));

        int baseSleepTime = Integer.parseInt(props.getProperty("baseSleepTime", "1000"));
        int maxRetries = Integer.parseInt(props.getProperty("maxRetries", "3"));
        int maxSleepTime = Integer.parseInt(props.getProperty("maxSleepTime", String.valueOf(Integer.MAX_VALUE)));
        watchPath = props.getProperty("watchPath", null);

        ZookeeperConfig zkConfig = new ZookeeperConfig(config.getServerLists());
        zkConfig.setBaseSleepTimeMilliseconds(baseSleepTime)
                .setMaxRetries(maxRetries)
                .setMaxSleepTimeMilliseconds(maxSleepTime)
                .setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);

        String digest = props.getProperty("digest");
        if (!StringUtils.isEmpty(digest)) {
            zkConfig.setDigest(digest);
        }
        this.client = new ZookeeperClient(zkConfig);
        this.client.getClient().getConnectionStateListenable().addListener((c, newState) -> {
            if (newState == ConnectionState.RECONNECTED) {
                nodeDataMap.forEach((k, v) -> {
                    if (!client.isExist(k)) {
                        client.createOrUpdate(k, v, CreateMode.EPHEMERAL);
                        LOGGER.info("zookeeper registry client register instance success: {}", v);
                    }
                });
            }
        });

        LOGGER.info("zookeeper registry init...");
        client.start();
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        String uriNodeName = buildInstanceNodeName(instance);
        String instancePath = InstancePathConstants.buildInstanceParentPath(instance.getAppName());
        if (!client.isExist(instancePath)) {
            client.createOrUpdate(instancePath, "", CreateMode.PERSISTENT);
        }
        String realNode = InstancePathConstants.buildRealNode(instancePath, uriNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        nodeDataMap.put(realNode, nodeData);
        client.createOrUpdate(realNode, nodeData, CreateMode.EPHEMERAL);
        LOGGER.info("zookeeper registry persistInstance success: {}", nodeData);
    }

    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        try {
            final String watchKey = StringUtils.isNotBlank(watchPath)
                    ? InstancePathConstants.buildRealNode(watchPath, selectKey) : InstancePathConstants.buildInstanceParentPath(selectKey);
            final Function<List<String>, List<InstanceEntity>> getInstanceRegisterFun = childrenList -> childrenList.stream().map(childPath -> {
                String instanceRegisterJsonStr = client.get(InstancePathConstants.buildRealNode(watchKey, childPath));
                InstanceEntity instanceEntity = GsonUtils.getInstance().fromJson(instanceRegisterJsonStr, InstanceEntity.class);
                instanceEntity.setUri(getURI(instanceRegisterJsonStr, instanceEntity.getPort(), instanceEntity.getHost()));
                if (instanceEntity.getUri() == null) {
                    final JsonObject hashMap = GsonUtils.getInstance().fromJson(instanceRegisterJsonStr, JsonObject.class);
                    final String address = hashMap.get("address").getAsString();
                    final Integer port = hashMap.get("port").getAsInt();
                    instanceEntity.setUri(getURI(instanceRegisterJsonStr, port, address));
                }
                return instanceEntity;
            }).collect(Collectors.toList());

            if (watcherInstanceRegisterMap.containsKey(selectKey)) {
                return watcherInstanceRegisterMap.get(selectKey);
            }

            List<String> childrenPathList = client.subscribeChildrenChanges(watchKey, new CuratorWatcher() {
                @Override
                public void process(final WatchedEvent event) {
                    try {
                        String path = Objects.isNull(event.getPath()) ? selectKey : event.getPath();
                        List<String> childrenList = StringUtils.isNotBlank(path) ? client.subscribeChildrenChanges(path, this)
                                : Collections.emptyList();
                        if (!childrenList.isEmpty()) {
                            watcherInstanceRegisterMap.put(selectKey, getInstanceRegisterFun.apply(childrenList));
                        }
                    } catch (Exception e) {
                        watcherInstanceRegisterMap.remove(selectKey);
                        LOGGER.error("zookeeper registry client subscribeChildrenChanges watch interrupt error:", e);
                    }
                }
            });

            final List<InstanceEntity> instanceEntities = getInstanceRegisterFun.apply(childrenPathList);
            watcherInstanceRegisterMap.put(selectKey, instanceEntities);
            return instanceEntities;
        } catch (Exception e) {
            LOGGER.error("zookeeper registry client selectInstances error:", e);
            return Collections.emptyList();
        }
    }

    @Override
    public boolean serviceExists(final String key) {
        try {
            return null != client.get(key);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watchInstances(final String key, final ChangedEventListener listener) {
        try {
            CuratorCache treeCache = client.addCache(key, (type, oldData, data) -> {
                if (!Objects.nonNull(data) || !Objects.nonNull(data.getData())) {
                    return;
                }
                String currentPath = data.getPath();
                String currentData = new String(data.getData(), StandardCharsets.UTF_8);
                LOGGER.info("zookeeper registry watch find resultData ={}", currentData);
                Stat stat = data.getStat();
                boolean isEphemeral = Objects.nonNull(stat) && stat.getEphemeralOwner() > 0;
                if (!isEphemeral) {
                    LOGGER.info("zookeeper registry watch Ignore non-ephemeral node changes path {}", currentPath);
                    return;
                }
                switch (type) {
                    case NODE_CREATED:
                        listener.onEvent(currentPath, currentData, ChangedEventListener.Event.ADDED);
                        break;
                    case NODE_CHANGED:
                        listener.onEvent(currentPath, currentData, ChangedEventListener.Event.UPDATED);
                        break;
                    case NODE_DELETED:
                        listener.onEvent(currentPath, currentData, ChangedEventListener.Event.DELETED);
                        break;
                    default:
                        listener.onEvent(currentPath, currentData, ChangedEventListener.Event.IGNORED);
                        break;
                }
            });
            cacheMap.put(key, treeCache);
            LOGGER.info("zookeeper registry subscribed to eureka updates for key: {}", key);
        } catch (Exception e) {
            LOGGER.error("zookeeper registry error watching key: {}", key, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void unWatchInstances(final String key) {
        if (cacheMap.containsKey(key)) {
            cacheMap.get(key).forEach(CuratorCache::close);
            cacheMap.removeAll(key);
        }
    }

    private URI getURI(final String instanceRegisterJsonStr, final Integer port, final String host) {
        if (port == null || host == null) {
            return null;
        }
        String scheme = (instanceRegisterJsonStr.contains("https") || instanceRegisterJsonStr.contains("HTTPS")) ? "https" : "http";
        String uri = String.format("%s://%s:%s", scheme, host, port);
        return URI.create(uri);
    }

    @Override
    public void close() {
        try {
            watcherInstanceRegisterMap.clear();
            //close treeCache
            cacheMap.values().forEach(CuratorCache::close);
            this.client.close();
            this.client = null;
            LOGGER.info("zookeeper registry shutting down...");
        } catch (Exception e) {
            LOGGER.error("zookeeper registry shutting down error", e);
            throw new ShenyuException(e);
        }
    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }
}
