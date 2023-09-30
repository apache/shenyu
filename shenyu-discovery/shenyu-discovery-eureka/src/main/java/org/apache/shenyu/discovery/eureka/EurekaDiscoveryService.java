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
import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.MyDataCenterInstanceConfig;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.appinfo.DataCenterInfo;
import com.netflix.appinfo.MyDataCenterInfo;
import com.netflix.appinfo.EurekaInstanceConfig;
import com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider;
import com.netflix.config.ConfigurationManager;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.EurekaEventListener;
import com.netflix.discovery.EurekaClientConfig;
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.DefaultEurekaClientConfig;
import com.netflix.discovery.shared.transport.EurekaHttpClient;
import com.netflix.discovery.shared.transport.jersey.JerseyApplicationClient;
import com.sun.jersey.client.apache4.ApacheHttpClient4;
import org.apache.shenyu.common.exception.ShenyuException;
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

    private final ConcurrentMap<String, EurekaEventListener> listenerMap = new ConcurrentHashMap<>();

    @Override
    public void init(final DiscoveryConfig config) {
        try {
            if (eurekaClient == null) {
                ConfigurationManager.loadProperties(getEurekaProperties(config));
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
    public void watch(final String key, final DataChangedEventListener listener) {
        EurekaEventListener eurekaEventListener = listenerMap.computeIfAbsent(key, k -> createEurekaListener(key, listener));
        eurekaClient.registerEventListener(eurekaEventListener);
        try {
            eurekaClient.registerEventListener(eurekaEventListener);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private EurekaEventListener createEurekaListener(final String key, final DataChangedEventListener listener) {
        return event -> {
        };
    }

    @Override
    public void unwatch(final String key) {
        try {
            EurekaEventListener eurekaEventListener = listenerMap.get(key);
            if (eurekaEventListener != null) {
                eurekaClient.unregisterEventListener(eurekaEventListener);
                listenerMap.remove(key);
            }
        } catch (Exception e) {
            LOGGER.error("Error removing Eureka service listener: {}", e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void register(final String key, final String value) {
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
    public List<String> getRegisterData(final String key) {
        try {
            List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            List<String> registerDataList = new ArrayList<>();
            for (InstanceInfo instanceInfo : instances) {
                String instanceInfoJson = buildInstanceInfoJson(instanceInfo);
                registerDataList.add(instanceInfoJson);
            }
            return registerDataList;
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public Boolean exists(final String key) {
        try {
            InstanceInfo instanceInfo = eurekaClient.getNextServerFromEureka(key, false);
            return instanceInfo != null;
        } catch (Exception e) {
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

    private Properties getEurekaProperties(final DiscoveryConfig config) {
        Properties eurekaProperties = new Properties();
        eurekaProperties.setProperty("eureka.client.service-url.defaultZone", config.getServerList());
        eurekaProperties.setProperty("eureka.serviceUrl.default", config.getServerList());
        eurekaProperties.setProperty("eureka.client.refresh.interval", config.getProps().getProperty("eureka.client.refresh.interval", "60"));
        return eurekaProperties;
    }

    private ApplicationInfoManager initializeApplicationInfoManager(final EurekaInstanceConfig instanceConfig) {
        if (applicationInfoManager == null) {
            InstanceInfo instanceInfo = new EurekaConfigBasedInstanceInfoProvider(instanceConfig).get();
            applicationInfoManager = new ApplicationInfoManager(instanceConfig, instanceInfo);
        }

        return applicationInfoManager;
    }

    private EurekaClient initializeEurekaClient(final ApplicationInfoManager applicationInfoManager, final EurekaClientConfig clientConfig) {
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

    private String buildInstanceInfoJson(final InstanceInfo instanceInfo) {
        JSONObject instanceJson = new JSONObject();
        instanceJson.put("url", instanceInfo.getIPAddr() + ":" + instanceInfo.getPort());
        instanceJson.put("status", instanceInfo.getStatus().toString());
        instanceJson.put("weight", instanceInfo.getMetadata().get("weight"));

        return instanceJson.toString();
    }
}
