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
import org.apache.commons.lang.StringUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.netflix.eureka.http.DefaultEurekaClientHttpRequestFactorySupplier;
import org.springframework.cloud.netflix.eureka.http.RestTemplateDiscoveryClientOptionalArgs;
import org.springframework.cloud.netflix.eureka.http.RestTemplateTransportClientFactories;

import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Join
public class EurekaInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EurekaInstanceRegisterRepository.class);

    private EurekaClient eurekaClient;

    private DefaultEurekaClientConfig eurekaClientConfig;

    private EurekaInstanceConfig eurekaInstanceConfig;

    private RestTemplateDiscoveryClientOptionalArgs restTemplateDiscoveryClientOptionalArgs;

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
        restTemplateDiscoveryClientOptionalArgs
                = new RestTemplateDiscoveryClientOptionalArgs(new DefaultEurekaClientHttpRequestFactorySupplier());
        eurekaClient = new DiscoveryClient(new ApplicationInfoManager(eurekaInstanceConfig,
                new EurekaConfigBasedInstanceInfoProvider(eurekaInstanceConfig).get()), eurekaClientNotRegisterEurekaConfig,
                new RestTemplateTransportClientFactories(restTemplateDiscoveryClientOptionalArgs));
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        InstanceInfo.Builder instanceInfoBuilder = instanceInfoBuilder();
        InstanceInfo instanceInfo = instanceInfoBuilder
                .setAppName(instance.getAppName())
                .setIPAddr(instance.getHost())
                .setHostName(instance.getHost())
                .setPort(instance.getPort())
                .setStatus(InstanceInfo.InstanceStatus.UP)
                .build();
        LeaseInfo.Builder leaseInfoBuilder = LeaseInfo.Builder.newBuilder()
                .setRenewalIntervalInSecs(eurekaInstanceConfig.getLeaseRenewalIntervalInSeconds())
                .setDurationInSecs(eurekaInstanceConfig.getLeaseExpirationDurationInSeconds());
        instanceInfo.setLeaseInfo(leaseInfoBuilder.build());
        ApplicationInfoManager applicationInfoManager = new ApplicationInfoManager(eurekaInstanceConfig, instanceInfo);
        new DiscoveryClient(applicationInfoManager, eurekaClientConfig,
                new RestTemplateTransportClientFactories(restTemplateDiscoveryClientOptionalArgs));
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

    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        return getInstances(selectKey);
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
        Optional.ofNullable(eurekaClient).ifPresent(EurekaClient::shutdown);
    }
}
