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

package org.apache.shenyu.sdk.springcloud;

import java.util.Collections;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.core.ShenyuInstanceRegisterRepositoryFactory;
import org.springframework.cloud.client.DefaultServiceInstance;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;

import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ShenyuDiscoveryClient implements DiscoveryClient {

    private ShenyuInstanceRegisterRepository registerRepository;

    private RegisterConfig registerConfig;

    private String algorithm;

    private String scheme;

    public ShenyuDiscoveryClient(final ShenyuInstanceRegisterRepository registerRepository) {
        this.registerRepository = registerRepository;
        this.algorithm = "roundRobin";
        this.scheme = "http";
    }

    public ShenyuDiscoveryClient(final RegisterConfig registerConfig) {
        this.registerRepository = ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(registerConfig);
        this.registerConfig = registerConfig;
        Properties props = registerConfig.getProps();
        this.algorithm = props.getProperty("algorithm", "roundRobin");
        this.scheme = props.getProperty("scheme", "http");
    }

    /**
     * A human-readable description of the implementation, used in HealthIndicator.
     * @return The description.
     */
    @Override
    public String description() {
        return "shenyu gateway sdk client discovery client";
    }

    /**
     * Gets all ServiceInstances associated with a particular serviceId.
     * @param serviceId The serviceId to query.
     * @return A List of ServiceInstance.
     */
    @Override
    public List<ServiceInstance> getInstances(final String serviceId) {
        final List<Upstream> upstreams;
        if (Objects.isNull(registerRepository)) {
            List<String> serverList = Arrays.asList(registerConfig.getServerLists().split(","));
            if (serverList.isEmpty()) {
                throw new ShenyuException("illegal param, serverLists configuration required if registerType equals local.");
            }
            upstreams = serverList.stream().map(serverAddress -> Upstream.builder().url(UriUtils.appendScheme(serverAddress, scheme)).build()).collect(Collectors.toList());
        } else {
            List<InstanceEntity> instanceRegisters = registerRepository.selectInstances(serviceId);
            if (ObjectUtils.isEmpty(instanceRegisters)) {
                throw new ShenyuException("Gateway address not found from registry.");
            }
            upstreams = instanceRegisters.stream().map(instanceRegister -> {
                final String instanceUrl = String.join(Constants.COLONS, instanceRegister.getHost(), Integer.toString(instanceRegister.getPort()));
                return Upstream.builder().url(UriUtils.appendScheme(instanceUrl, scheme)).build();
            }).collect(Collectors.toList());
        }
        // loadBalancer upstreams
        final Upstream upstream = LoadBalancerFactory.selector(upstreams, algorithm, "");
        return Stream.of(upstream).map(stream -> {
            final URI uri = UriUtils.createUri(stream.getUrl());
            return new DefaultServiceInstance(stream.buildDomain(), serviceId, uri.getHost(), uri.getPort(), "https".equals(scheme));
        }).collect(Collectors.toList());
    }

    /**
     * get discovery client support serviceIds.
     * @return All known service IDs.
     */
    @Override
    public List<String> getServices() {
        return Collections.singletonList(ShenyuServiceInstanceLoadBalancer.getShenyuServiceId());
    }

}
