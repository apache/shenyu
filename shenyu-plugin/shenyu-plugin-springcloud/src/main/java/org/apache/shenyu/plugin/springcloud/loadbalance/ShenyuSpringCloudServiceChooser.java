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

package org.apache.shenyu.plugin.springcloud.loadbalance;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.cloud.client.loadbalancer.Request;
import org.springframework.cloud.client.loadbalancer.ServiceInstanceChooser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

import static org.springframework.cloud.client.loadbalancer.reactive.ReactiveLoadBalancer.REQUEST;

/**
 * spring cloud plugin loadbalancer.
 */
public final class ShenyuSpringCloudServiceChooser implements ServiceInstanceChooser {

    private final static String UPSTREAM_URL = "upstreamUrl";

    private final static String PROTOCOL_PREFIX = "protocolPrefix";

    private final DiscoveryClient discoveryClient;

    public ShenyuSpringCloudServiceChooser(final DiscoveryClient discoveryClient) {
        this.discoveryClient = discoveryClient;
    }

    @Override
    public ServiceInstance choose(final String serviceId) {
        return choose(serviceId, REQUEST);
    }

    @Override
    public <T> ServiceInstance choose(final String serviceId, final Request<T> request) {
        // load service instance by serviceId
        List<ServiceInstance> available = this.getServiceInstance(serviceId);
        if (CollectionUtils.isEmpty(available)) {
            return null;
        }
        final LoadBalanceKey loadBalanceKey = LoadBalanceKeyHolder.getLoadBalanceKey();
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudPluginDataHandler.SELECTOR_CACHED.get().obtainHandle(loadBalanceKey.getSelectorId());
        // not gray flow
        if (!springCloudSelectorHandle.getGray()) {
            return this.doSelect(serviceId);
        }
        List<Upstream> divideUpstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(loadBalanceKey.getSelectorId());
        // gray flow,but upstream is null
        if (CollectionUtils.isEmpty(divideUpstreams)) {
            return this.doSelect(serviceId);
        }
        //select server from available to choose
        final List<Upstream> choose = new ArrayList<>(available.size());
        for (ServiceInstance serviceInstance : available) {
            divideUpstreams.stream()
                    .filter(Upstream::isStatus)
                    .filter(upstream -> Objects.equals(upstream.getUrl(),
                            splitUrl(String.valueOf(serviceInstance.getUri())).getProperty(UPSTREAM_URL)))
                    .findFirst().ifPresent(choose::add);
        }
        if (CollectionUtils.isEmpty(choose)) {
            return this.doSelect(serviceId);
        }
        // select by divideUpstreams
        return this.doSelect(serviceId, choose);
    }

    /**
     * select serviceInstance by shenyu loadbalancer.
     *
     * @param serviceId serviceId
     * @return ServiceInstance
     */
    private ServiceInstance doSelect(final String serviceId) {
        List<Upstream> choose = this.buildUpstream(serviceId);
        return this.doSelect(serviceId, choose);
    }

    /**
     * execute loadbalancer by shenyu loadbalancer.
     *
     * @param serviceId serviceId
     * @param upstreamList upstream list
     * @return ServiceInstance
     */
    private ServiceInstance doSelect(final String serviceId, final List<Upstream> upstreamList) {
        final LoadBalanceKey loadBalanceKey = LoadBalanceKeyHolder.getLoadBalanceKey();
        // default loadbalancer
        if (StringUtils.isEmpty(loadBalanceKey.getLoadBalance())) {
            loadBalanceKey.setLoadBalance("roundRobin");
        }
        Upstream upstream = LoadBalancerFactory.selector(upstreamList, loadBalanceKey.getLoadBalance(), loadBalanceKey.getIp());
        List<ServiceInstance> instances = this.getServiceInstance(serviceId);

        Optional<ServiceInstance> serviceInstance = instances.stream().filter(x -> {
            // check serviceInstance ip:port equal upstream ip:port
            Properties props = splitUrl(String.valueOf(x.getUri()));
            return Objects.equals(upstream.getUrl(), props.getProperty(UPSTREAM_URL));
        }).findFirst();
        return serviceInstance.orElse(null);
    }

    /**
     * get service instance by serviceId.
     *
     * @param serviceId serviceId
     * @return {@linkplain ServiceInstance}
     */
    private List<ServiceInstance> getServiceInstance(final String serviceId) {
        List<String> serviceNames = discoveryClient.getServices().stream().map(String::toUpperCase).collect(Collectors.toList());
        if (!serviceNames.contains(serviceId.toUpperCase())) {
            return Collections.emptyList();
        }
        return discoveryClient.getInstances(serviceId);
    }

    /**
     * build upstream by service instance.
     *
     * @param serviceId serviceId
     * @return Upstream List
     */
    private List<Upstream> buildUpstream(final String serviceId) {
        List<ServiceInstance> serviceInstanceList = this.getServiceInstance(serviceId);
        if (serviceInstanceList.isEmpty()) {
            return Collections.emptyList();
        }
        return serviceInstanceList.stream().map(x -> {
            String uri = x.getUri().toString();
            Properties props = splitUrl(uri);
            String upstreamUrl = props.getProperty(UPSTREAM_URL);
            String protocolPrefix = props.getProperty(PROTOCOL_PREFIX);
            String protocol = protocolPrefix + "://";
            return buildDefaultSpringCloudUpstream(upstreamUrl, protocol);
        }).distinct().collect(Collectors.toList());
    }

    /**
     * split url.
     *
     * @param url url
     * @return properties
     */
    private static Properties splitUrl(final String url) {
        String[] urlPart = url.split("\\:\\//");
        String protocolPrefix = urlPart[0];
        String upstreamUrl = urlPart[1];
        Properties properties = new Properties();
        properties.setProperty(UPSTREAM_URL, upstreamUrl);
        properties.setProperty(PROTOCOL_PREFIX, protocolPrefix);
        return properties;
    }

    /**
     * build default spring cloud upstream.
     *
     * @param upstreamUrl url
     * @param protocol protocol
     * @return Upstream
     */
    private static Upstream buildDefaultSpringCloudUpstream(final String upstreamUrl, final String protocol) {
        return Upstream.builder().url(upstreamUrl)
                .protocol(protocol)
                .weight(50)
                .warmup(10)
                .timestamp(System.currentTimeMillis())
                .build();
    }
}
