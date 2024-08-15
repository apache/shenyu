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

package org.apache.shenyu.sdk.feign;

import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.HttpSchemeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.DefaultServiceInstance;
import org.springframework.cloud.client.ServiceInstance;

public class ShenyuDiscoveryClient {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuDiscoveryClient.class);

    private final ShenyuInstanceRegisterRepository registerRepository;

    private final RegisterConfig registerConfig;

    private final String algorithm;

    private final String scheme;

    public ShenyuDiscoveryClient(final RegisterConfig registerConfig) {
        this(null, registerConfig);
    }

    public ShenyuDiscoveryClient(final ShenyuInstanceRegisterRepository registerRepository, final RegisterConfig registerConfig) {
        this.registerRepository = registerRepository;
        this.registerConfig = registerConfig;
        Properties props = registerConfig.getProps();
        this.algorithm = props.getProperty("algorithm", "roundRobin");
        this.scheme = props.getProperty("scheme", HttpSchemeEnum.HTTP.getScheme());
    }

    /**
     * Gets all ServiceInstances associated with a particular serviceId.
     * @param serviceId The serviceId to query.
     * @return A List of ServiceInstance.
     */
    public ServiceInstance getInstance(final String serviceId) {
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
        if (CollectionUtils.isEmpty(upstreams)) {
            LOG.error("The serviceId that named {} could not load balanced to at least one upstream.", serviceId);
        }
        Upstream upstream = upstreams.get(0);
        if (CollectionUtils.isNotEmpty(upstreams) && upstreams.size() > 1) {
            upstream = LoadBalancerFactory.selector(upstreams, algorithm, "");
        }

        final URI uri = UriUtils.createUri(upstream.getUrl());
        if (Objects.isNull(uri)) {
            throw new ShenyuException("Gateway address uri is not invalid.");
        }
        return new DefaultServiceInstance(upstream.getUrl(), serviceId, uri.getHost(), uri.getPort(), HttpSchemeEnum.HTTPS.getScheme().equals(scheme));
    }

}
