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

package org.apache.shenyu.discovery.eureka;

import com.alibaba.fastjson2.JSONObject;
import com.netflix.appinfo.*;
import com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider;
import com.netflix.config.ConfigurationManager;
import com.netflix.discovery.*;
import com.netflix.discovery.shared.transport.EurekaHttpClient;
import com.netflix.discovery.shared.transport.jersey.JerseyApplicationClient;
import com.sun.jersey.client.apache4.ApacheHttpClient4;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

@Join
public class EurekaDiscoveryService implements ShenyuDiscoveryService {
    private static final Logger LOGGER = LoggerFactory.getLogger(EurekaDiscoveryService.class);
    private ApplicationInfoManager applicationInfoManager;
    private EurekaClient eurekaClient;
    private EurekaHttpClient eurekaHttpClient;

    private static final ConcurrentMap<String, List<EurekaEventListener>> LISTENER_SERVICE_MAP = new ConcurrentHashMap<>();

    @Override
    public void init(final DiscoveryConfig config) {
        try {
            if (eurekaClient == null) {
                // 配置 Eureka 服务器地址
                ConfigurationManager.loadProperties(getEurekaProperties(config));
                // 初始化 ApplicationInfoManager
                applicationInfoManager = initializeApplicationInfoManager(new MyDataCenterInstanceConfig());
                eurekaClient = initializeEurekaClient(applicationInfoManager, new DefaultEurekaClientConfig());
                eurekaHttpClient = new JerseyApplicationClient(new ApacheHttpClient4(), config.getServerList(), null);
            }
        } catch (Exception e) {
            clean();
            throw new ShenyuException(e);
        }
    }


    @Override
    public void watch(String key, DataChangedEventListener listener) {
        // 使用Eureka的监听器来监听服务实例变化

//        eurekaClient.registerEventListener(event -> {
//            String appName = event.getAppName(); // 获取应用名称
//            if (key.equals(appName)) { // 检查是否是指定的服务名称
//                String instanceId = event.getInstanceId(); // 获取服务实例ID
//                String instanceData = buildInstanceInfoJson(appName, instanceId); // 构建服务实例信息
//                DiscoveryDataChangedEvent dataChangedEvent;
//
//                if (event instanceof StatusChangeEvent) {
//                    StatusChangeEvent statusChangeEvent = (StatusChangeEvent) event;
//                    StatusChangeEvent.StatusEventType statusEventType = statusChangeEvent.getStatusEventType();
//
//                    if (statusEventType == StatusChangeEvent.StatusEventType.UP) {
//                        // 服务实例上线
//                        dataChangedEvent = new DiscoveryDataChangedEvent(key, instanceData, DiscoveryDataChangedEvent.Event.ADDED);
//                    } else if (statusEventType == StatusChangeEvent.StatusEventType.DOWN) {
//                        // 服务实例下线
//                        dataChangedEvent = new DiscoveryDataChangedEvent(key, instanceData, DiscoveryDataChangedEvent.Event.DELETED);
//                    } else {
//                        return; // 忽略其他状态事件
//                    }
//
//                    listener.onChange(dataChangedEvent);
//                }
//            }
//        });
    }

    @Override
    public void unwatch(String key) {

    }

    @Override
    public void register(String key, String value) {
        JSONObject jsonValue = JSONObject.parse(value);
        InstanceInfo instanceInfo = InstanceInfo.Builder.newBuilder()
                .setAppName(key)
                .setIPAddr(jsonValue.getString("ip"))
                .setHostName(jsonValue.getString("hostname"))
                .setPort(jsonValue.getIntValue("port"))
                .setDataCenterInfo(new MyDataCenterInfo(DataCenterInfo.Name.MyOwn))
                .build();
        eurekaHttpClient.register(instanceInfo);
    }

    @Override
    public List<String> getRegisterData(String key) {
        try {
            List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            List<String> registerDataList = new ArrayList<>();
            for (InstanceInfo instanceInfo : instances) {
                // 构建服务实例信息的 JSON 字符串
                String instanceInfoJson = buildInstanceInfoJson(instanceInfo);
                // 将 JSON 字符串添加到列表
                registerDataList.add(instanceInfoJson);
            }
            return registerDataList;
        } catch (Exception e) {
            throw  new ShenyuException(e);
        }
    }

    @Override
    public Boolean exists(String key) {
        try {
            InstanceInfo instanceInfo = eurekaClient.getNextServerFromEureka(key, false);
            return instanceInfo != null;
        } catch (Exception e) {
            // 处理查询过程中的异常
            throw new ShenyuException(e);
        }
    }

    @Override
    public void shutdown() {
        try {
            if (eurekaClient != null) {
                eurekaClient.shutdown();
            }
            clean();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private Properties getEurekaProperties(DiscoveryConfig config) {
        Properties eurekaProperties = new Properties();
        eurekaProperties.setProperty("eureka.client.service-url.defaultZone", config.getServerList());
        eurekaProperties.setProperty("eureka.serviceUrl.default", config.getServerList());
        eurekaProperties.setProperty("eureka.client.refresh.interval", config.getProps().getProperty("eureka.client.refresh.interval", "60"));
        return eurekaProperties;
    }

    private ApplicationInfoManager initializeApplicationInfoManager(final EurekaInstanceConfig instanceConfig) {
        if (applicationInfoManager == null) {
            InstanceInfo instanceInfo = new EurekaConfigBasedInstanceInfoProvider(instanceConfig).get();//应用实例信息
            applicationInfoManager = new ApplicationInfoManager(instanceConfig, instanceInfo);
        }

        return applicationInfoManager;
    }

    private EurekaClient initializeEurekaClient(ApplicationInfoManager applicationInfoManager, EurekaClientConfig clientConfig) {
        if (eurekaClient == null) {
            eurekaClient = new DiscoveryClient(applicationInfoManager, clientConfig);
        }

        return eurekaClient;
    }

    private void clean() {
        eurekaClient = null;
        applicationInfoManager = null;
        eurekaHttpClient = null;
    }

    private String buildInstanceInfoJson(InstanceInfo instanceInfo) {
        // 构建一个包含服务实例信息的 JSON 对象
        // TODO：具体需要哪些数据？
        JSONObject instanceJson = new JSONObject();
        instanceJson.put("url", instanceInfo.getIPAddr()+":"+instanceInfo.getPort());
        instanceJson.put("status", instanceInfo.getStatus().toString());
        instanceJson.put("weight", instanceInfo.getMetadata().get("weight"));

        // 将 JSON 对象转化为字符串
        return instanceJson.toString();
    }
}
