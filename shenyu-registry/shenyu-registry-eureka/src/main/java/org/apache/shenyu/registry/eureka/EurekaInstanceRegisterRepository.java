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

package org.apache.shenyu.registry.eureka;

import com.google.gson.JsonObject;
import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.DataCenterInfo;
import com.netflix.appinfo.EurekaInstanceConfig;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.appinfo.LeaseInfo;
import com.netflix.appinfo.MyDataCenterInstanceConfig;
import com.netflix.appinfo.RefreshableInstanceConfig;
import com.netflix.appinfo.UniqueIdentifier;
import com.netflix.appinfo.providers.Archaius1VipAddressResolver;
import com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider;
import com.netflix.discovery.DefaultEurekaClientConfig;
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.shared.transport.jersey3.Jersey3TransportClientFactories;
import org.apache.commons.lang.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Join
public class EurekaInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EurekaInstanceRegisterRepository.class);

    private EurekaClient eurekaClient;

    private EurekaClient registerDiscoveryClient;

    private DefaultEurekaClientConfig eurekaClientConfig;

    private EurekaInstanceConfig eurekaInstanceConfig;

    private final ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(10, ShenyuThreadFactory.create("scheduled-eureka-watcher", true));

    private final ConcurrentMap<String, ScheduledFuture<?>> listenerThreadsMap = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, List<InstanceInfo>> instanceListMap = new ConcurrentHashMap<>();

    @Override
    public void init(final RegisterConfig config) {
        eurekaInstanceConfig = new MyDataCenterInstanceConfig();
        eurekaClientConfig = new DefaultEurekaClientConfig() {
            @Override
            public List<String> getEurekaServerServiceUrls(final String zone) {
                return Arrays.asList(config.getServerLists().split(","));
            }

            @Override
            public boolean shouldFetchRegistry() {
                return false;
            }
        };

        DefaultEurekaClientConfig eurekaClientNotRegisterEurekaConfig = new DefaultEurekaClientConfig() {
            @Override
            public List<String> getEurekaServerServiceUrls(final String zone) {
                return Arrays.asList(config.getServerLists().split(","));
            }

            @Override
            public boolean shouldRegisterWithEureka() {
                return false;
            }
        };
        LOGGER.info("eureka registry init...");
        eurekaClient = new DiscoveryClient(new ApplicationInfoManager(eurekaInstanceConfig,
                new EurekaConfigBasedInstanceInfoProvider(eurekaInstanceConfig).get()), eurekaClientNotRegisterEurekaConfig,
                new Jersey3TransportClientFactories());
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {

        InstanceInfo.Builder instanceInfoBuilder = instanceInfoBuilder();
        InstanceInfo instanceInfo = instanceInfoBuilder
                .setAppName(instance.getAppName())
                .setIPAddr(instance.getHost())
                .setHostName(instance.getHost())
                .setPort(instance.getPort())
                .setLastDirtyTimestamp(System.currentTimeMillis())
                .setStatus(InstanceInfo.InstanceStatus.UP)
                .build();
        LeaseInfo.Builder leaseInfoBuilder = LeaseInfo.Builder.newBuilder()
                .setRenewalIntervalInSecs(eurekaInstanceConfig.getLeaseRenewalIntervalInSeconds())
                .setDurationInSecs(eurekaInstanceConfig.getLeaseExpirationDurationInSeconds());
        instanceInfo.setLeaseInfo(leaseInfoBuilder.build());
        ApplicationInfoManager applicationInfoManager = new ApplicationInfoManager(eurekaInstanceConfig, instanceInfo);
        registerDiscoveryClient = new DiscoveryClient(applicationInfoManager, eurekaClientConfig, new Jersey3TransportClientFactories());
        LOGGER.info("eureka registry persistInstance success: {}", instanceInfo);
    }

    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        return getInstances(selectKey);
    }

    @Override
    public boolean serviceExists(final String key) {
        try {
            List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            return !instances.isEmpty();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watchInstances(final String key, final ChangedEventListener listener) {
        try {
            List<InstanceInfo> initialInstances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
            instanceListMap.put(key, initialInstances);
            for (InstanceInfo instance : initialInstances) {
                listener.onEvent(key, buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.ADDED);
            }
            ScheduledFuture<?> scheduledFuture = executorService.scheduleAtFixedRate(() -> {
                try {
                    List<InstanceInfo> previousInstances = instanceListMap.get(key);
                    List<InstanceInfo> currentInstances = eurekaClient.getInstancesByVipAddressAndAppName(null, key, true);
                    compareInstances(previousInstances, currentInstances, listener);
                    instanceListMap.put(key, currentInstances);
                } catch (Exception e) {
                    LOGGER.error("eureka registry eurekaDiscoveryService watch key: {} error", key, e);
                    throw new ShenyuException(e);
                }
            }, 0, 1, TimeUnit.SECONDS);
            listenerThreadsMap.put(key, scheduledFuture);
            LOGGER.info("eureka registry subscribed to eureka updates for key: {}", key);
        } catch (Exception e) {
            LOGGER.error("eureka registry error watching key: {}", key, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void unWatchInstances(final String key) {
        try {
            ScheduledFuture<?> scheduledFuture = listenerThreadsMap.get(key);
            if (Objects.nonNull(scheduledFuture)) {
                scheduledFuture.cancel(true);
                listenerThreadsMap.remove(key);
                LOGGER.info("eureka registry unwatch key {} successfully", key);
            }
        } catch (Exception e) {
            LOGGER.error("eureka registry error removing eureka watch task for key {} {}", key, e.getMessage(), e);
            throw new ShenyuException(e);
        }
    }

    private void compareInstances(final List<InstanceInfo> previousInstances, final List<InstanceInfo> currentInstances, final ChangedEventListener listener) {
        Set<InstanceInfo> addedInstances = currentInstances.stream()
                .filter(item -> !previousInstances.contains(item))
                .collect(Collectors.toSet());
        if (!addedInstances.isEmpty()) {
            for (InstanceInfo instance : addedInstances) {
                listener.onEvent(instance.getAppName(), buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.ADDED);
            }
        }

        Set<InstanceInfo> deletedInstances = previousInstances.stream()
                .filter(item -> !currentInstances.contains(item))
                .collect(Collectors.toSet());
        if (!deletedInstances.isEmpty()) {
            for (InstanceInfo instance : deletedInstances) {
                instance.setStatus(InstanceInfo.InstanceStatus.DOWN);
                listener.onEvent(instance.getAppName(), buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.DELETED);
            }
        }

        Set<InstanceInfo> updatedInstances = currentInstances.stream()
                .filter(currentInstance -> previousInstances.stream()
                        .anyMatch(previousInstance -> currentInstance.getInstanceId().equals(previousInstance.getInstanceId()) && !currentInstance.equals(previousInstance)))
                .collect(Collectors.toSet());
        if (!updatedInstances.isEmpty()) {
            for (InstanceInfo instance : updatedInstances) {
                listener.onEvent(instance.getAppName(), buildUpstreamJsonFromInstance(instance), ChangedEventListener.Event.UPDATED);
            }
        }
    }

    private String buildUpstreamJsonFromInstance(final InstanceInfo instanceInfo) {
        JsonObject upstreamJson = new JsonObject();
        upstreamJson.addProperty("url", instanceInfo.getIPAddr() + ":" + instanceInfo.getPort());
        upstreamJson.addProperty("weight", instanceInfo.getMetadata().get("weight"));
        boolean secure = instanceInfo.isPortEnabled(InstanceInfo.PortType.SECURE);
        String scheme = secure ? "https://" : "http://";
        upstreamJson.addProperty("protocol", Optional.ofNullable(instanceInfo.getMetadata().get("protocol")).orElse(scheme));
        upstreamJson.addProperty("props", instanceInfo.getMetadata().get("props"));
        if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.UP) {
            upstreamJson.addProperty("status", 0);
        } else if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.DOWN) {
            upstreamJson.addProperty("status", 1);
        }
        return GsonUtils.getInstance().toJson(upstreamJson);
    }

    private List<InstanceEntity> getInstances(final String selectKey) {
        List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, selectKey, true);
        return instances.stream()
                .map(i -> InstanceEntity.builder()
                        .appName(i.getAppName()).host(i.getHostName()).port(i.getPort()).uri(getURI(i))
                        .build()
                ).collect(Collectors.toList());
    }

    private URI getURI(final InstanceInfo instance) {
        boolean secure = instance.isPortEnabled(InstanceInfo.PortType.SECURE);
        String scheme = secure ? "https" : "http";
        int port = instance.getPort();
        if (port <= 0) {
            port = secure ? 443 : 80;
        }
        String uri = String.format("%s://%s:%s", scheme, instance.getIPAddr(), port);
        return URI.create(uri);
    }

    @Override
    public void close() {
        try {
            for (ScheduledFuture<?> scheduledFuture : listenerThreadsMap.values()) {
                scheduledFuture.cancel(true);
            }
            listenerThreadsMap.clear();
            if (Objects.nonNull(eurekaClient)) {
                eurekaClient.getApplicationInfoManager().setInstanceStatus(InstanceInfo.InstanceStatus.DOWN);
                eurekaClient.shutdown();
            }
            if (Objects.nonNull(registerDiscoveryClient)) {
                registerDiscoveryClient.getApplicationInfoManager().setInstanceStatus(InstanceInfo.InstanceStatus.DOWN);
                registerDiscoveryClient.shutdown();
            }
            LOGGER.info("eureka registry shutting down...");
        } catch (Exception e) {
            LOGGER.error("eureka registry shutting down error", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * Gets the instance information from the config instance and returns it after setting the appropriate status.
     * ref: com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider#get
     *
     * @return InstanceInfo instance to be registered with eureka server
     */
    public InstanceInfo.Builder instanceInfoBuilder() {
        // Builder the instance information to be registered with eureka server
        final InstanceInfo.Builder builder = InstanceInfo.Builder.newBuilder(new Archaius1VipAddressResolver());

        // set the appropriate id for the InstanceInfo, falling back to datacenter Id if applicable, else hostname
        String instanceId = eurekaInstanceConfig.getInstanceId();
        if (StringUtils.isEmpty(instanceId)) {
            DataCenterInfo dataCenterInfo = eurekaInstanceConfig.getDataCenterInfo();
            if (dataCenterInfo instanceof UniqueIdentifier) {
                instanceId = ((UniqueIdentifier) dataCenterInfo).getId();
            } else {
                instanceId = eurekaInstanceConfig.getHostName(false);
            }
        }

        String defaultAddress;
        if (eurekaInstanceConfig instanceof RefreshableInstanceConfig) {
            // Refresh AWS data center info, and return up to date address
            defaultAddress = ((RefreshableInstanceConfig) eurekaInstanceConfig).resolveDefaultAddress(false);
        } else {
            defaultAddress = eurekaInstanceConfig.getHostName(false);
        }

        // fail safe
        if (StringUtils.isEmpty(defaultAddress)) {
            defaultAddress = eurekaInstanceConfig.getIpAddress();
        }

        builder.setNamespace(eurekaInstanceConfig.getNamespace())
                .setInstanceId(instanceId)
                .setAppName(eurekaInstanceConfig.getAppname())
                .setAppGroupName(eurekaInstanceConfig.getAppGroupName())
                .setDataCenterInfo(eurekaInstanceConfig.getDataCenterInfo())
                .setIPAddr(eurekaInstanceConfig.getIpAddress())
                .setHostName(defaultAddress)
                .setPort(eurekaInstanceConfig.getNonSecurePort())
                .enablePort(InstanceInfo.PortType.UNSECURE, eurekaInstanceConfig.isNonSecurePortEnabled())
                .setSecurePort(eurekaInstanceConfig.getSecurePort())
                .enablePort(InstanceInfo.PortType.SECURE, eurekaInstanceConfig.getSecurePortEnabled())
                .setVIPAddress(eurekaInstanceConfig.getVirtualHostName())
                .setSecureVIPAddress(eurekaInstanceConfig.getSecureVirtualHostName())
                .setHomePageUrl(eurekaInstanceConfig.getHomePageUrlPath(), eurekaInstanceConfig.getHomePageUrl())
                .setStatusPageUrl(eurekaInstanceConfig.getStatusPageUrlPath(), eurekaInstanceConfig.getStatusPageUrl())
                .setASGName(eurekaInstanceConfig.getASGName())
                .setHealthCheckUrls(eurekaInstanceConfig.getHealthCheckUrlPath(),
                        eurekaInstanceConfig.getHealthCheckUrl(), eurekaInstanceConfig.getSecureHealthCheckUrl());

        // Start off with the STARTING state to avoid traffic
        if (!eurekaInstanceConfig.isInstanceEnabledOnit()) {
            InstanceInfo.InstanceStatus initialStatus = InstanceInfo.InstanceStatus.STARTING;
            LOGGER.info("Setting initial instance status as: {}", initialStatus);
            builder.setStatus(initialStatus);
        } else {
            LOGGER.info("Setting initial instance status as: {}. This may be too early for the instance to advertise "
                            + "itself as available. You would instead want to control this via a healthcheck handler.",
                    InstanceInfo.InstanceStatus.UP);
        }

        // Add any user-specific metadata information
        for (Map.Entry<String, String> mapEntry : eurekaInstanceConfig.getMetadataMap().entrySet()) {
            String key = mapEntry.getKey();
            String value = mapEntry.getValue();
            // only add the metadata if the value is present
            if (StringUtils.isNotEmpty(value)) {
                builder.add(key, value);
            }
        }
        return builder;
    }
}
