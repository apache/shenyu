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

import com.google.gson.JsonObject;
import org.apache.shenyu.common.utils.GsonUtils;
import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.MyDataCenterInstanceConfig;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.appinfo.EurekaInstanceConfig;
import com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider;
import com.netflix.config.ConfigurationManager;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.EurekaClientConfig;
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.DefaultEurekaClientConfig;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
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
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Join
public class EurekaDiscoveryService implements ShenyuDiscoveryService {
    private static final Logger LOGGER = LoggerFactory.getLogger(EurekaDiscoveryService.class);

    private ApplicationInfoManager applicationInfoManager;

    private EurekaClient eurekaClient;

    private final ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(10, ShenyuThreadFactory.create("scheduled-eureka-watcher", true));

    private final ConcurrentMap<String, ScheduledFuture<?>> listenerThreadsMap = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, List<InstanceInfo>> instanceListMap = new ConcurrentHashMap<>();

    @Override
    public void init(final DiscoveryConfig config) {
        try {
            if (Objects.isNull(eurekaClient)) {
                ConfigurationManager.loadProperties(getEurekaProperties(config));
                applicationInfoManager = initializeApplicationInfoManager(new MyDataCenterInstanceConfig());
                eurekaClient = initializeEurekaClient(applicationInfoManager, new DefaultEurekaClientConfig());
                LOGGER.info("Initializing EurekaDiscoveryService success");
            }
        } catch (Exception e) {
            LOGGER.error("Error initializing EurekaDiscoveryService", e);
            clean();
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watch(final String key, final DataChangedEventListener listener) {
        if (!listenerThreadsMap.containsKey(key)) {
            List<InstanceInfo> initialInstances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            instanceListMap.put(key, initialInstances);
            ScheduledFuture<?> scheduledFuture = executorService.scheduleAtFixedRate(() -> {
                try {
                    List<InstanceInfo> previousInstances = instanceListMap.get(key);
                    List<InstanceInfo> currentInstances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
                    compareInstances(previousInstances, currentInstances, listener);
                    instanceListMap.put(key, currentInstances);
                    LOGGER.info("EurekaDiscoveryService watch key: {} success", key);
                } catch (Exception e) {
                    LOGGER.error("EurekaDiscoveryService watch key: {} error", key, e);
                    throw new ShenyuException(e);
                }
            }, 0, 1, TimeUnit.SECONDS);
            listenerThreadsMap.put(key, scheduledFuture);
        }
    }

    @Override
    public void unwatch(final String key) {
        try {
            ScheduledFuture<?> scheduledFuture = listenerThreadsMap.get(key);
            if (Objects.nonNull(scheduledFuture)) {
                scheduledFuture.cancel(true);
                listenerThreadsMap.remove(key);
                LOGGER.info("EurekaDiscoveryService unwatch key {} successfully", key);
            }
        } catch (Exception e) {
            LOGGER.error("Error removing eureka watch task for key '{}': {}", key, e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void register(final String key, final String value) {
        InstanceInfo instanceInfoFromJson = GsonUtils.getInstance().fromJson(value, InstanceInfo.class);
        CustomedEurekaConfig customedEurekaConfig = new CustomedEurekaConfig();
        customedEurekaConfig.setIpAddress(instanceInfoFromJson.getVIPAddress());
        customedEurekaConfig.setPort(instanceInfoFromJson.getPort());
        customedEurekaConfig.setApplicationName(key);
        customedEurekaConfig.setInstanceId(instanceInfoFromJson.getInstanceId());
        InstanceInfo instanceInfo = new EurekaConfigBasedInstanceInfoProvider(customedEurekaConfig).get();
        applicationInfoManager = new ApplicationInfoManager(customedEurekaConfig, instanceInfo);
        eurekaClient = new DiscoveryClient(applicationInfoManager, new DefaultEurekaClientConfig());
        applicationInfoManager.setInstanceStatus(InstanceInfo.InstanceStatus.UP);
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
            return Objects.nonNull(instanceInfo);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void shutdown() {
        try {
            for (ScheduledFuture<?> scheduledFuture : listenerThreadsMap.values()) {
                scheduledFuture.cancel(true);
            }
            executorService.shutdown();
            listenerThreadsMap.clear();
            if (Objects.nonNull(eurekaClient)) {
                eurekaClient.shutdown();
            }
            LOGGER.info("Shutting down EurekaDiscoveryService");
            clean();
        } catch (Exception e) {
            LOGGER.error("Shutting down EurekaDiscoveryService", e);
            throw new ShenyuException(e);
        }
    }

    private Properties getEurekaProperties(final DiscoveryConfig config) {
        Properties eurekaProperties = new Properties();
        eurekaProperties.setProperty("eureka.client.service-url.defaultZone", config.getServerList());
        eurekaProperties.setProperty("eureka.serviceUrl.default", config.getServerList());
        eurekaProperties.setProperty("eureka.client.refresh.interval", config.getProps().getProperty("eureka.client.refresh.interval", "15"));

        return eurekaProperties;
    }

    private ApplicationInfoManager initializeApplicationInfoManager(final EurekaInstanceConfig instanceConfig) {
        if (Objects.isNull(applicationInfoManager)) {
            InstanceInfo instanceInfo = new EurekaConfigBasedInstanceInfoProvider(instanceConfig).get();
            applicationInfoManager = new ApplicationInfoManager(instanceConfig, instanceInfo);
        }

        return applicationInfoManager;
    }

    private EurekaClient initializeEurekaClient(final ApplicationInfoManager applicationInfoManager, final EurekaClientConfig clientConfig) {
        if (Objects.isNull(eurekaClient)) {
            eurekaClient = new DiscoveryClient(applicationInfoManager, clientConfig);
        }

        return eurekaClient;
    }

    private void clean() {
        eurekaClient = null;
        applicationInfoManager = null;
    }

    private String buildInstanceInfoJson(final InstanceInfo instanceInfo) {
        JsonObject instanceJson = new JsonObject();
        instanceJson.addProperty("url", instanceInfo.getIPAddr() + ":" + instanceInfo.getPort());
        instanceJson.addProperty("weight", instanceInfo.getMetadata().get("weight"));
        if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.UP) {
            instanceJson.addProperty("status", 0);
        } else if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.DOWN) {
            instanceJson.addProperty("status", 1);
        }
        return GsonUtils.getInstance().toJson(instanceJson);
    }

    private void compareInstances(final List<InstanceInfo> previousInstances, final List<InstanceInfo> currentInstances, final DataChangedEventListener listener) {
        Set<InstanceInfo> addedInstances = currentInstances.stream()
                .filter(item -> !previousInstances.contains(item))
                .collect(Collectors.toSet());
        if (!addedInstances.isEmpty()) {
            for (InstanceInfo instance: addedInstances) {
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                        buildInstanceInfoJson(instance), DiscoveryDataChangedEvent.Event.ADDED);
                listener.onChange(dataChangedEvent);
            }
        }

        Set<InstanceInfo> deletedInstances = previousInstances.stream()
                .filter(item -> !currentInstances.contains(item))
                .collect(Collectors.toSet());
        if (!deletedInstances.isEmpty()) {
            for (InstanceInfo instance: deletedInstances) {
                instance.setStatus(InstanceInfo.InstanceStatus.DOWN);
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                        buildInstanceInfoJson(instance), DiscoveryDataChangedEvent.Event.DELETED);
                listener.onChange(dataChangedEvent);
            }
        }

        Set<InstanceInfo> updatedInstances = currentInstances.stream()
                .filter(currentInstance -> previousInstances.stream()
                        .anyMatch(previousInstance -> currentInstance.getInstanceId().equals(previousInstance.getInstanceId()) && !currentInstance.equals(previousInstance)))
                .collect(Collectors.toSet());
        if (!updatedInstances.isEmpty()) {
            for (InstanceInfo instance: updatedInstances) {
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                        buildInstanceInfoJson(instance), DiscoveryDataChangedEvent.Event.UPDATED);
                listener.onChange(dataChangedEvent);
            }
        }
    }
}
