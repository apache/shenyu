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

package org.apache.shenyu.registry.nacos;

import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * The type Nacos instance register repository.
 */
@Join
public class NacosInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(NacosInstanceRegisterRepository.class);

    private static final String NAMESPACE = "nacosNameSpace";

    private final ConcurrentMap<String, EventListener> listenerMap = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, List<Instance>> instanceListMap = new ConcurrentHashMap<>();

    private NamingService namingService;

    private String groupName;

    @Override
    public void init(final RegisterConfig config) {
        Properties properties = config.getProps();
        Properties nacosProperties = new Properties();
        this.groupName = properties.getProperty("groupName", "DEFAULT_GROUP");
        String serverAddr = config.getServerLists();
        nacosProperties.put(PropertyKeyConst.SERVER_ADDR, serverAddr);
        nacosProperties.put(PropertyKeyConst.NAMESPACE, properties.getProperty(NAMESPACE, ""));
        nacosProperties.put(PropertyKeyConst.USERNAME, properties.getProperty(PropertyKeyConst.USERNAME, ""));
        nacosProperties.put(PropertyKeyConst.PASSWORD, properties.getProperty(PropertyKeyConst.PASSWORD, ""));
        nacosProperties.put(PropertyKeyConst.ACCESS_KEY, properties.getProperty(PropertyKeyConst.ACCESS_KEY, ""));
        nacosProperties.put(PropertyKeyConst.SECRET_KEY, properties.getProperty(PropertyKeyConst.SECRET_KEY, ""));
        try {
            LOGGER.info("nacos registry init...");
            this.namingService = NamingFactory.createNamingService(nacosProperties);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        try {
            Instance inst = new Instance();
            inst.setWeight(1.0d);
            inst.setEphemeral(true);
            inst.setIp(instance.getHost());
            inst.setPort(instance.getPort());
            inst.setInstanceId(buildInstanceNodeName(instance));
            inst.setServiceName(instance.getAppName());
            namingService.registerInstance(instance.getAppName(), groupName, inst);
            LOGGER.info("nacos registry persistInstance success: {}", inst);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        return getInstanceRegisterDTOS(selectKey);
    }

    @Override
    public boolean serviceExists(final String key) {
        try {
            List<Instance> instances = namingService.selectInstances(key, groupName, true);
            return !instances.isEmpty();
        } catch (NacosException e) {
            LOGGER.error("nacos registry Error checking Nacos service existence: {}", e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watchInstances(final String key, final ChangedEventListener listener) {
        try {
            List<Instance> initialInstances = namingService.selectInstances(key, groupName, true);
            instanceListMap.put(key, initialInstances);
            for (Instance instance : initialInstances) {
                listener.onEvent(key, buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.ADDED);
            }
            EventListener nacosListener = event -> {
                if (event instanceof NamingEvent) {
                    try {
                        List<Instance> previousInstances = instanceListMap.get(key);
                        List<Instance> currentInstances = namingService.selectInstances(key, groupName, true);
                        compareInstances(previousInstances, currentInstances, listener);
                        instanceListMap.put(key, currentInstances);
                    } catch (NacosException e) {
                        throw new ShenyuException(e);
                    }
                }
            };
            namingService.subscribe(key, groupName, nacosListener);
            listenerMap.put(key, nacosListener);
            LOGGER.info("nacos registry subscribed to nacos updates for key: {}", key);
        } catch (NacosException e) {
            LOGGER.error("nacos registry error watching key: {}", key, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void unWatchInstances(final String key) {
        try {
            EventListener nacosListener = listenerMap.get(key);
            if (Objects.nonNull(nacosListener)) {
                namingService.unsubscribe(key, groupName, nacosListener);
                listenerMap.remove(key);
                LOGGER.info("nacos registry unwatch key: {}", key);
            }
        } catch (NacosException e) {
            LOGGER.error("nacos registry error removing nacos service listener...", e);
            throw new ShenyuException(e);
        }
    }

    private void compareInstances(final List<Instance> previousInstances, final List<Instance> currentInstances, final ChangedEventListener listener) {
        Set<Instance> addedInstances = currentInstances.stream()
                .filter(item -> !previousInstances.contains(item))
                .collect(Collectors.toSet());
        if (!addedInstances.isEmpty()) {
            for (Instance instance: addedInstances) {
                listener.onEvent(instance.getServiceName(), buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.ADDED);
            }
        }

        Set<Instance> deletedInstances = previousInstances.stream()
                .filter(item -> !currentInstances.contains(item))
                .collect(Collectors.toSet());
        if (!deletedInstances.isEmpty()) {
            for (Instance instance: deletedInstances) {
                instance.setHealthy(false);
                listener.onEvent(instance.getServiceName(), buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.DELETED);
            }
        }

        Set<Instance> updatedInstances = currentInstances.stream()
                .filter(currentInstance -> previousInstances.stream()
                        .anyMatch(previousInstance -> currentInstance.getInstanceId().equals(previousInstance.getInstanceId()) && !currentInstance.equals(previousInstance)))
                .collect(Collectors.toSet());
        if (!updatedInstances.isEmpty()) {
            for (Instance instance: updatedInstances) {
                listener.onEvent(instance.getServiceName(), buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.UPDATED);
            }
        }
    }

    private String buildUpstreamJsonFromInstance(final Instance instance) {
        JsonObject upstreamJson = new JsonObject();
        upstreamJson.addProperty("url", instance.getIp() + ":" + instance.getPort());
        // status 0:true, 1:false
        upstreamJson.addProperty("status", instance.isHealthy() ? 0 : 1);
        upstreamJson.addProperty("weight", instance.getWeight());
        Map<String, String> metadata = instance.getMetadata();
        upstreamJson.addProperty("props", metadata.get("props"));
        boolean secure = false;
        if (metadata.containsKey("secure")) {
            secure = Boolean.parseBoolean(metadata.get("secure"));
        }
        String scheme = secure ? "https://" : "http://";
        upstreamJson.addProperty("protocol", Optional.ofNullable(metadata.get("protocol")).orElse(scheme));
        return GsonUtils.getInstance().toJson(upstreamJson);
    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    private List<InstanceEntity> getInstanceRegisterDTOS(final String selectKey) {
        List<InstanceEntity> result = new ArrayList<>();
        try {
            List<Instance> instances = namingService.selectInstances(selectKey, groupName, true);
            instances.forEach(instance -> result.add(convertFromInstance(instance)));
        } catch (Exception e) {
            LOGGER.error("getInstanceRegisterDTOS error", e);
        }
        return result;
    }

    private InstanceEntity convertFromInstance(final Instance instance) {
        InstanceEntity instanceEntity = new InstanceEntity();
        instanceEntity.setPort(instance.getPort());
        instanceEntity.setHost(instance.getIp());
        instanceEntity.setAppName(instance.getServiceName());
        instanceEntity.setUri(getURI(instance.getMetadata(), instance));
        return instanceEntity;
    }

    private URI getURI(final Map<String, String> metadata, final Instance instance) {
        boolean secure = false;
        if (metadata.containsKey("secure")) {
            secure = Boolean.parseBoolean(metadata.get("secure"));
        }
        String scheme = secure ? "https" : "http";
        int port = instance.getPort();
        if (port <= 0) {
            port = secure ? 443 : 80;
        }
        String uri = String.format("%s://%s:%s", scheme, instance.getIp(), port);
        return URI.create(uri);
    }

    @Override
    public void close() {
        try {
            if (Objects.nonNull(this.namingService)) {
                for (Map.Entry<String, EventListener> entry : listenerMap.entrySet()) {
                    String key = entry.getKey();
                    EventListener listener = entry.getValue();
                    this.namingService.unsubscribe(key, groupName, listener);
                }
                listenerMap.clear();
                this.namingService.shutDown();
                this.namingService = null;
                LOGGER.info("nacos registry shutting down...");
            }
        } catch (NacosException e) {
            LOGGER.error("nacos registry shutting down error ", e);
            throw new ShenyuException(e);
        }
    }
}
