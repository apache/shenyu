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

package org.apache.shenyu.registry.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import com.ctrip.framework.apollo.ConfigService;
import com.ctrip.framework.apollo.core.ConfigConsts;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * The type apollo instance register repository.
 */
@Join
public class ApolloInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final String APOLLO_CLUSTER = "apollo.cluster";

    private static final String PROP_APP_ID = "app.id";

    private static final String PROP_APOLLO_META = "apollo.meta";

    private static final String APOLLO_NAMESPACE = "apollo.bootstrap.namespace";

    private static final Logger LOGGER = LoggerFactory.getLogger(ApolloInstanceRegisterRepository.class);

    private ApolloClient apolloClient;

    private final Map<String, ConfigChangeListener> configChangeListenerMap = Maps.newConcurrentMap();

    private final Map<String, List<InstanceEntity>> watcherInstanceRegisterMap = new HashMap<>();

    private String namespace;

    private Config configService;

    @Override
    public void init(final RegisterConfig config) {
        Properties properties = config.getProps();
        String meta = config.getServerLists();
        String appId = properties.getProperty("appId", "shenyu");
        String clusterName = properties.getProperty("clusterName", ConfigConsts.CLUSTER_NAME_DEFAULT);
        String namespace = properties.getProperty("namespace", ConfigConsts.NAMESPACE_APPLICATION);
        Optional.ofNullable(appId).ifPresent(x -> System.setProperty(PROP_APP_ID, x));
        Optional.ofNullable(meta).ifPresent(x -> System.setProperty(PROP_APOLLO_META, x));
        Optional.ofNullable(clusterName).ifPresent(x -> System.setProperty(APOLLO_CLUSTER, x));
        Optional.ofNullable(namespace).ifPresent(x -> System.setProperty(APOLLO_NAMESPACE, x));
        Optional.ofNullable(namespace).ifPresent(x -> System.setProperty(APOLLO_NAMESPACE, x));
        this.namespace = namespace;
        this.configService = ConfigService.getAppConfig();
        buildClient(config);
    }

    private void buildClient(final RegisterConfig config) {
        // init apollo client
        Properties properties = config.getProps();
        String appId = properties.getProperty("appId");
        String token = properties.getProperty("token");
        String env = properties.getProperty("env", "DEV");
        String clusterName = properties.getProperty("clusterName", ConfigConsts.CLUSTER_NAME_DEFAULT);
        String namespace = properties.getProperty("namespace", ConfigConsts.NAMESPACE_APPLICATION);
        String portalUrl = properties.getProperty("portalUrl", "http://localhost:8070");
        ApolloConfig apolloConfig = new ApolloConfig();
        apolloConfig.setAppId(appId);
        apolloConfig.setPortalUrl(portalUrl);
        apolloConfig.setToken(token);
        apolloConfig.setEnv(env);
        apolloConfig.setClusterName(clusterName);
        apolloConfig.setNamespace(namespace);
        this.apolloClient = new ApolloClient(apolloConfig);
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        String instanceNodeName = buildInstanceNodeName(instance);
        String instancePath = InstancePathConstants.buildInstanceParentPath(instance.getAppName());
        String realNode = InstancePathConstants.buildRealNode(instancePath, instanceNodeName);
        String nodeData = GsonUtils.getInstance().toJson(instance);
        apolloClient.createOrUpdateItem(realNode, nodeData, "register instance");
        apolloClient.publishNamespace(namespace, "publish instance");
        LOGGER.info("apollo instance register success: {}", nodeData);
    }

    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        final String watchKey = InstancePathConstants.buildInstanceParentPath(selectKey);

        final Function<Map<String, String>, List<InstanceEntity>> getInstanceRegisterFun = childrenList ->
                childrenList.values().stream().map(x -> {
                    InstanceEntity instanceEntity = GsonUtils.getInstance().fromJson(x, InstanceEntity.class);
                    instanceEntity.setUri(getURI(x, instanceEntity.getPort(), instanceEntity.getHost()));
                    return instanceEntity;
                }).collect(Collectors.toList());
        Map<String, String> childrenList = new HashMap<>();

        if (watcherInstanceRegisterMap.containsKey(selectKey)) {
            return watcherInstanceRegisterMap.get(selectKey);
        }

        configService.getPropertyNames().forEach(key -> {
            if (key.startsWith(watchKey)) {
                String itemValue = apolloClient.getItemValue(key);
                childrenList.put(key, itemValue);
            }
        });
        ConfigChangeListener configChangeListener = changeEvent -> {
            Set<String> keys = changeEvent.changedKeys();
            keys.forEach(key -> {
                if (key.startsWith(watchKey)) {
                    switch (changeEvent.getChange(key).getChangeType()) {
                        case ADDED:
                            childrenList.put(key, changeEvent.getChange(key).getNewValue());
                            LOGGER.info("apollo instance register add {}", changeEvent.getChange(key).getNewValue());
                            break;
                        case DELETED:
                            childrenList.remove(key);
                            LOGGER.info("apollo instance register delete success: {}", changeEvent.getChange(key).getOldValue());
                            break;
                        default:
                            break;
                    }
                    watcherInstanceRegisterMap.put(selectKey, getInstanceRegisterFun.apply(childrenList));
                }
            });
        };
        configService.addChangeListener(configChangeListener);
        configChangeListenerMap.put(watchKey, configChangeListener);
        final List<InstanceEntity> instanceEntities = getInstanceRegisterFun.apply(childrenList);
        watcherInstanceRegisterMap.put(selectKey, instanceEntities);
        return instanceEntities;
    }

    private URI getURI(final String instanceRegisterJsonStr, final int port, final String host) {
        String scheme = (instanceRegisterJsonStr.contains("https") || instanceRegisterJsonStr.contains("HTTPS")) ? "https" : "http";
        String uri = String.format("%s://%s:%s", scheme, host, port);
        return URI.create(uri);
    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    @Override
    public void close() {
        configChangeListenerMap.forEach((key, value) -> configService.removeChangeListener(value));
    }
}
