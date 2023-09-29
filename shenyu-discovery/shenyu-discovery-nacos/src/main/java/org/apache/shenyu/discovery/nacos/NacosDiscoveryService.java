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

import com.alibaba.cloud.nacos.NacosDiscoveryProperties;
import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.listener.Event;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import com.alibaba.nacos.client.naming.event.InstancesChangeEvent;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executor;

/**
 * The type Nacos for shenyu discovery service.
 */
@Join
public class NacosDiscoveryService implements ShenyuDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(NacosDiscoveryService.class);

    private static final String NAMESPACE = "nacosNameSpace";

    private final ConcurrentHashMap<String, Listener> watchCache = new ConcurrentHashMap<>();

    private NamingService namingService;

    // private final ConfigService configService;

    private String groupName;


    // 用于管理监听器，key 为监听的键（服务名或实例名），value 为对应的监听器
    private ConcurrentMap<String, EventListener> listenerMap = new ConcurrentHashMap<>();


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
//        EventListener eventListener = EventListenerFactory.createEventListener(new EventListener() {
//            @Override
//            public void onEvent(Event event) {
//                if (event instanceof NamingEvent) {
//                    NamingEvent namingEvent = (NamingEvent) event;
//                    EventType eventType = namingEvent.getEventType();
//
//                    switch (eventType) {
//                        case ADDED:
//                            // 处理服务实例添加事件
//                            Instance addedInstance = namingEvent.getInstance();
//                            System.out.println("Service Instance Added: " + addedInstance.getIp() + ":" + addedInstance.getPort());
//                            break;
//                        case REMOVED:
//                            // 处理服务实例移除事件
//                            Instance removedInstance = namingEvent.getInstance();
//                            System.out.println("Service Instance Removed: " + removedInstance.getIp() + ":" + removedInstance.getPort());
//                            break;
//                        case MODIFIED:
//                            // 处理服务实例修改事件
//                            Instance modifiedInstance = namingEvent.getInstance();
//                            System.out.println("Service Instance Modified: " + modifiedInstance.getIp() + ":" + modifiedInstance.getPort());
//                            break;
//                    }
//                }
//            }
//        });


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
            Instance instance = new Instance();
            // 设置服务实例的相关信息，例如 IP、端口、权重等
            // 你需要根据你的实际需求来设置这些信息
            instance.setIp("your_ip");
//            instance.setPort(your_port);
//            instance.setWeight(your_weight);

            // 注册服务实例，并根据 isPersistent 参数决定持久化类型

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
                // 根据实际需要提取服务实例的信息并添加到 registerData 列表中
                String data = "IP: " + instance.getIp() + ", Port: " + instance.getPort();
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
            // 关闭监听器
            for (Map.Entry<String, EventListener> entry : listenerMap.entrySet()) {
                String key = entry.getKey();
                EventListener listener = entry.getValue();
                namingService.unsubscribe(key, listener);
            }
            listenerMap.clear();
            // 关闭其他资源，例如命名服务
            namingService.shutDown();
        } catch (NacosException e) {
            throw new ShenyuException(e);
        }
    }

}






        //        try {
//            EventListener nacosListener = new EventListener() {
//                @Override
//                public void onEvent(Event event) {
//                    if (event instanceof NamingEvent) {
//                        NamingEvent namingEvent = (NamingEvent) event;
//                        String serviceName = namingEvent.getServiceName();
//
//                        // 根据事件类型执行相应的操作
//                        // DiscoveryDataChangedEvent.Event shenyuEvent = mapNacosEventToShenyuEvent(event);
//
//                        // 获取实例列表
//                        List<Instance> instances = namingEvent.getInstances();
//                        for (Instance instance : instances) {
//                            String currentData = instance.getMetadata().get("yourDataKey"); // 替换为你的数据键
//                            // 创建 DiscoveryDataChangedEvent
//                            // DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(serviceName, currentData, shenyuEvent);
//
//                            // 通知外部代码
//                           // listener.onChange(dataChangedEvent);
//                        }
//                    }
//                }
//            };
//
//            namingService.subscribe(key, nacosListener);
//            listenerMap.put(key, nacosListener);
//        } catch (NacosException e) {
//            LOGGER.error("Error adding Nacos service listener: {}", e.getMessage(), e);
//            throw new ShenyuException(e);
//        }









