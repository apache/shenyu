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


package org.apache.shenyu.discovery.nacos;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * The type Nacos for shenyu discovery service.
 */
@Join
public class NacosDiscoveryService implements ShenyuDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(NacosDiscoveryService.class);

    private static final String NAMESPACE = "nacosNameSpace";

    private ConcurrentMap<String, EventListener> listenerMap = new ConcurrentHashMap<>();

    private NamingService namingService;

    private String groupName;

    private List<Instance> instancesList = new ArrayList<>();


    @Override
    public void init(DiscoveryConfig config) {
        Properties properties = config.getProps();
        Properties nacosProperties = new Properties();
        this.groupName = properties.getProperty("groupName", "SHENYU_GROUP");
        String serverAddr = config.getServerList();
        nacosProperties.put(PropertyKeyConst.SERVER_ADDR, serverAddr);
        nacosProperties.put(PropertyKeyConst.NAMESPACE, properties.getProperty(NAMESPACE, ""));
        nacosProperties.put(PropertyKeyConst.USERNAME, properties.getProperty(PropertyKeyConst.USERNAME, ""));
        nacosProperties.put(PropertyKeyConst.PASSWORD, properties.getProperty(PropertyKeyConst.PASSWORD, ""));
        nacosProperties.put(PropertyKeyConst.ACCESS_KEY, properties.getProperty(PropertyKeyConst.ACCESS_KEY, ""));
        nacosProperties.put(PropertyKeyConst.SECRET_KEY, properties.getProperty(PropertyKeyConst.SECRET_KEY, ""));
        try {
            this.namingService = NamingFactory.createNamingService(nacosProperties);
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }


    @Override
    public void watch(String key, DataChangedEventListener listener) {
        EventListener nacosListener = listenerMap.computeIfAbsent(key, k -> createNacosListener(key, listener));
        try {
            namingService.subscribe(key, groupName, nacosListener);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private EventListener createNacosListener(String key, DataChangedEventListener listener) {
        return event -> {
            if (event instanceof NamingEvent) {
                List<Instance> currentInstances = ((NamingEvent) event).getInstances();
                compareInstances(this.instancesList, currentInstances, listener);
                this.instancesList.clear();
                this.instancesList.addAll(currentInstances);
            }
        };
    }


    @Override
    public void unwatch(String key) {
        try {
            EventListener nacosListener = listenerMap.get(key);
            if (nacosListener != null) {
                namingService.unsubscribe(key, nacosListener);
                listenerMap.remove(key);
            }
        } catch (NacosException e) {
            LOGGER.error("Error removing Nacos service listener: {}", e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }


    @Override
    public void register(String key, String value) {
        try {
            JSONObject jsonValue = JSONObject.parse(value);
            Instance instance = new Instance();
            instance.setIp(jsonValue.getString("ip"));
            instance.setPort(jsonValue.getIntValue("port"));
            instance.setWeight(jsonValue.getDoubleValue("weight"));
            instance.setServiceName(jsonValue.getString("serviceName"));
            namingService.registerInstance(key, instance);

        } catch (NacosException e) {
            LOGGER.error("Error registering Nacos service instance: {}", e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }


    @Override
    public List<String> getRegisterData(String key) {
        try {
            List<Instance> instances = namingService.getAllInstances(key);
            List<String> registerData = new ArrayList<>();
            for (Instance instance : instances) {
                String data = buildInstanceInfoJson(instance);
                registerData.add(data);
            }
            return registerData;
        } catch (NacosException e) {
            LOGGER.error("Error getting Nacos service instances: {}", e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }


    @Override
    public Boolean exists(String key) {
        try {
            List<Instance> instances = namingService.selectInstances(key, true);
            return !instances.isEmpty();
        } catch (NacosException e) {
            LOGGER.error("Error checking Nacos service existence: {}", e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }


    @Override
    public void shutdown() {
        try {
            for (Map.Entry<String, EventListener> entry : listenerMap.entrySet()) {
                String key = entry.getKey();
                EventListener listener = entry.getValue();
                namingService.unsubscribe(key, listener);
            }
            listenerMap.clear();
            namingService.shutDown();
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

    private void compareInstances(List<Instance> previousInstances, List<Instance> currentInstances, DataChangedEventListener listener) {
        DiscoveryDataChangedEvent dataChangedEvent = null;
        if (currentInstances.size() > previousInstances.size()){
            Set<Instance> addedInstances = currentInstances.stream()
                    .filter(item -> !previousInstances.contains(item))
                    .collect(Collectors.toSet());
            if (!addedInstances.isEmpty()) {
                Instance addedInstance = addedInstances.iterator().next();
                dataChangedEvent = new DiscoveryDataChangedEvent(addedInstance.getServiceName(), buildInstanceInfoJson(addedInstance), DiscoveryDataChangedEvent.Event.ADDED);
            }
        } else if (currentInstances.size() < previousInstances.size()) {
            Set<Instance> deletedInstances = previousInstances.stream()
                    .filter(item -> !currentInstances.contains(item))
                    .collect(Collectors.toSet());
            if (!deletedInstances.isEmpty()) {
                Instance deletedInstance = deletedInstances.iterator().next();
                dataChangedEvent = new DiscoveryDataChangedEvent(deletedInstance.getServiceName(), buildInstanceInfoJson(deletedInstance), DiscoveryDataChangedEvent.Event.DELETED);
            }
        } else {
            Set<Instance> updatedInstances = currentInstances.stream()
                    .filter(previousInstances::contains)
                    .collect(Collectors.toSet());
            if (!updatedInstances.isEmpty()) {
                Instance updatedInstance = updatedInstances.iterator().next();
                dataChangedEvent = new DiscoveryDataChangedEvent(updatedInstance.getServiceName(), buildInstanceInfoJson(updatedInstance), DiscoveryDataChangedEvent.Event.UPDATED);
            }
        }
        listener.onChange(dataChangedEvent);
    }

    private String buildInstanceInfoJson(Instance instance) {
        JSONObject instanceJson = new JSONObject();
        instanceJson.put("url", instance.getIp()+":"+instance.getPort());
        instanceJson.put("status", instance.isHealthy() ? 0: -1);
        instanceJson.put("weight", instance.getWeight());

        return instanceJson.toString();
    }
}





