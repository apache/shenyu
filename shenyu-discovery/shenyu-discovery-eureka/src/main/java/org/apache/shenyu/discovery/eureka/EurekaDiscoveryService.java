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
import com.google.gson.JsonSyntaxException;
import com.netflix.appinfo.EurekaInstanceConfig;
import com.netflix.appinfo.MyDataCenterInstanceConfig;
import com.netflix.discovery.EurekaClientConfig;
import com.netflix.discovery.shared.transport.jersey3.Jersey3TransportClientFactories;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.utils.GsonUtils;
import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider;
import com.netflix.config.ConfigurationManager;
import com.netflix.discovery.EurekaClient;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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

    private DiscoveryConfig discoveryConfig;

    private final ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(10, ShenyuThreadFactory.create("scheduled-eureka-watcher", true));

    private final ConcurrentMap<String, ScheduledFuture<?>> listenerThreadsMap = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, List<InstanceInfo>> instanceListMap = new ConcurrentHashMap<>();

    @Override
    public void init(final DiscoveryConfig config) {
        if (Objects.nonNull(this.eurekaClient)) {
            LOGGER.info("Eureka naming service already registered");
        }
        discoveryConfig = config;
        try {
            ConfigurationManager.loadProperties(getEurekaProperties(false));
            applicationInfoManager = initializeApplicationInfoManager(new MyDataCenterInstanceConfig());
            eurekaClient = initializeEurekaClient(applicationInfoManager, new DefaultEurekaClientConfig());
            LOGGER.info("Initializing EurekaDiscoveryService success");
        } catch (Exception e) {
            LOGGER.error("Error initializing EurekaDiscoveryService", e);
            clean();
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watch(final String key, final DataChangedEventListener listener) {
        List<InstanceInfo> initialInstances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
        instanceListMap.put(key, initialInstances);
        for (InstanceInfo instance : initialInstances) {
            DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                    buildUpstreamJsonFromInstanceInfo(instance), DiscoveryDataChangedEvent.Event.ADDED);
            listener.onChange(dataChangedEvent);
        }
        ScheduledFuture<?> scheduledFuture = executorService.scheduleAtFixedRate(() -> {
            try {
                List<InstanceInfo> previousInstances = instanceListMap.get(key);
                List<InstanceInfo> currentInstances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
                compareInstances(previousInstances, currentInstances, listener);
                instanceListMap.put(key, currentInstances);
            } catch (Exception e) {
                LOGGER.error("EurekaDiscoveryService watch key: {} error", key, e);
                throw new ShenyuException(e);
            }
        }, 0, 1, TimeUnit.SECONDS);
        listenerThreadsMap.put(key, scheduledFuture);
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
        CustomedEurekaConfig customedEurekaConfig = new CustomedEurekaConfig();
        InstanceInfo instanceInfoFromJson = buildInstanceInfoFromUpstream(key, value);
        customedEurekaConfig.setIpAddress(instanceInfoFromJson.getIPAddr());
        customedEurekaConfig.setPort(instanceInfoFromJson.getPort());
        customedEurekaConfig.setApplicationName(key);
        customedEurekaConfig.setInstanceId(instanceInfoFromJson.getInstanceId());
        customedEurekaConfig.setMetadataMap(instanceInfoFromJson.getMetadata());
        try {
            ConfigurationManager.loadProperties(getEurekaProperties(true));
            InstanceInfo instanceInfo = new EurekaConfigBasedInstanceInfoProvider(customedEurekaConfig).get();
            applicationInfoManager = new ApplicationInfoManager(customedEurekaConfig, instanceInfo);
            eurekaClient = new DiscoveryClient(applicationInfoManager, new DefaultEurekaClientConfig(), new Jersey3TransportClientFactories());
            applicationInfoManager.setInstanceStatus(InstanceInfo.InstanceStatus.UP);
        } catch (Exception e) {
            LOGGER.error("Error register eureka instance", e);
            clean();
            throw new ShenyuException(e);
        }
    }

    @Override
    public List<String> getRegisterData(final String key) {
        try {
            List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            List<String> registerDataList = new ArrayList<>();
            for (InstanceInfo instanceInfo : instances) {
                String instanceInfoJson = buildUpstreamJsonFromInstanceInfo(instanceInfo);
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
            List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            return !instances.isEmpty();
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
            listenerThreadsMap.clear();
            if (Objects.nonNull(eurekaClient)) {
                eurekaClient.getApplicationInfoManager().setInstanceStatus(InstanceInfo.InstanceStatus.DOWN);
                eurekaClient.shutdown();
            }
            LOGGER.info("Shutting down EurekaDiscoveryService");
            clean();
        } catch (Exception e) {
            LOGGER.error("Shutting down EurekaDiscoveryService error", e);
            throw new ShenyuException(e);
        }
    }

    private Properties getEurekaProperties(final boolean needRegister) {
        Properties eurekaProperties = new Properties();
        eurekaProperties.setProperty("eureka.serviceUrl.default", discoveryConfig.getServerList());
        eurekaProperties.setProperty("eureka.client.refresh.interval", discoveryConfig.getProps().getProperty("eurekaClientRefreshInterval", "10"));
        eurekaProperties.setProperty("eureka.client.registry-fetch-interval-seconds", discoveryConfig.getProps().getProperty("eurekaClientRegistryFetchIntervalSeconds", "10"));
        eurekaProperties.setProperty("eureka.registration.enabled", String.valueOf(needRegister));

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
            eurekaClient = new DiscoveryClient(applicationInfoManager, clientConfig, new Jersey3TransportClientFactories());
        }

        return eurekaClient;
    }

    private void clean() {
        eurekaClient = null;
        applicationInfoManager = null;
    }

    private String buildUpstreamJsonFromInstanceInfo(final InstanceInfo instanceInfo) {
        JsonObject upstreamJson = new JsonObject();
        upstreamJson.addProperty("url", instanceInfo.getIPAddr() + ":" + instanceInfo.getPort());
        upstreamJson.addProperty("weight", instanceInfo.getMetadata().get("weight"));
        upstreamJson.addProperty("protocol", instanceInfo.getMetadata().get("protocol"));
        upstreamJson.addProperty("props", instanceInfo.getMetadata().get("props"));
        if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.UP) {
            upstreamJson.addProperty("status", 0);
        } else if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.DOWN) {
            upstreamJson.addProperty("status", 1);
        }
        return GsonUtils.getInstance().toJson(upstreamJson);
    }

    private void compareInstances(final List<InstanceInfo> previousInstances, final List<InstanceInfo> currentInstances, final DataChangedEventListener listener) {
        Set<InstanceInfo> addedInstances = currentInstances.stream()
                .filter(item -> !previousInstances.contains(item))
                .collect(Collectors.toSet());
        if (!addedInstances.isEmpty()) {
            for (InstanceInfo instance : addedInstances) {
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                        buildUpstreamJsonFromInstanceInfo(instance), DiscoveryDataChangedEvent.Event.ADDED);
                listener.onChange(dataChangedEvent);
            }
        }

        Set<InstanceInfo> deletedInstances = previousInstances.stream()
                .filter(item -> !currentInstances.contains(item))
                .collect(Collectors.toSet());
        if (!deletedInstances.isEmpty()) {
            for (InstanceInfo instance : deletedInstances) {
                instance.setStatus(InstanceInfo.InstanceStatus.DOWN);
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                        buildUpstreamJsonFromInstanceInfo(instance), DiscoveryDataChangedEvent.Event.DELETED);
                listener.onChange(dataChangedEvent);
            }
        }

        Set<InstanceInfo> updatedInstances = currentInstances.stream()
                .filter(currentInstance -> previousInstances.stream()
                        .anyMatch(previousInstance -> currentInstance.getInstanceId().equals(previousInstance.getInstanceId()) && !currentInstance.equals(previousInstance)))
                .collect(Collectors.toSet());
        if (!updatedInstances.isEmpty()) {
            for (InstanceInfo instance : updatedInstances) {
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(instance.getAppName(),
                        buildUpstreamJsonFromInstanceInfo(instance), DiscoveryDataChangedEvent.Event.UPDATED);
                listener.onChange(dataChangedEvent);
            }
        }
    }

    private InstanceInfo buildInstanceInfoFromUpstream(final String key, final String value) {
        try {
            DiscoveryUpstreamData upstreamData = GsonUtils.getInstance().fromJson(value, DiscoveryUpstreamData.class);
            Map<String, String> metadata = GsonUtils.getInstance().toObjectMap(upstreamData.getProps(), String.class);
            metadata = Objects.nonNull(metadata) ? metadata : new HashMap<>();
            metadata.put("weight", String.valueOf(upstreamData.getWeight()));
            metadata.put("protocol", String.valueOf(upstreamData.getProtocol()));
            metadata.put("props", Optional.ofNullable(upstreamData.getProps()).orElse("{}"));
            String[] urls = upstreamData.getUrl().split(":", 2);
            return InstanceInfo.Builder.newBuilder()
                    .setAppName(key)
                    .setIPAddr(urls[0])
                    .setPort(Integer.parseInt(urls[1]))
                    .setMetadata(metadata)
                    .setInstanceId(urls[0] + ":" + key + ":" + urls[1])
                    .build();
        } catch (JsonSyntaxException jsonSyntaxException) {
            LOGGER.error("The json format of value is wrong: {}", jsonSyntaxException.getMessage(), jsonSyntaxException);
            throw new ShenyuException(jsonSyntaxException);
        }
    }

}
