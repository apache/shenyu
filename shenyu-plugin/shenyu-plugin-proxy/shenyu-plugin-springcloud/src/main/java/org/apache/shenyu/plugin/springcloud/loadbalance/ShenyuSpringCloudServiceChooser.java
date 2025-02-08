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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.springcloud.cache.ServiceInstanceCache;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.ServiceInstance;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * spring cloud plugin loadbalancer.
 */
public final class ShenyuSpringCloudServiceChooser {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuSpringCloudServiceChooser.class);

    public ShenyuSpringCloudServiceChooser() {
    }

    /**
     * choose service instance.
     *
     * @param serviceId service id
     * @param selectorId selector id
     * @param ip ip
     * @param loadbalancer load balancer
     * @return Upstream
     */
    public Upstream choose(final String serviceId, final String selectorId,
                           final String ip, final String loadbalancer) {
        // load service instance by serviceId
        List<InstanceEntity> available = this.getServiceInstance(serviceId);
        if (CollectionUtils.isEmpty(available)) {
            LOG.info("choose return 1");
            return null;
        }
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudPluginDataHandler.SELECTOR_CACHED.get().obtainHandle(selectorId);
        // not gray flow
        if (!springCloudSelectorHandle.getGray()) {
            // load service from register center
            return this.doSelect(serviceId, ip, loadbalancer);
        }
        List<Upstream> divideUpstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId);
        // gray flow,but upstream is null
        if (CollectionUtils.isEmpty(divideUpstreams)) {
            return this.doSelect(serviceId, ip, loadbalancer);
        }
        // select server from available to choose
        final List<Upstream> choose = new ArrayList<>(available.size());
        for (InstanceEntity serviceInstance : available) {
            divideUpstreams.stream()
                    .filter(Upstream::isStatus)
                    .filter(upstream -> Objects.equals(upstream.getUrl(), serviceInstance.getUri().getRawAuthority()))
                    .findFirst().ifPresent(choose::add);
        }
        if (CollectionUtils.isEmpty(choose)) {
            return this.doSelect(serviceId, ip, loadbalancer);
        }
        // select by divideUpstreams
        return this.doSelect(choose, loadbalancer, ip);
    }

    /**
     * select serviceInstance by shenyu loadbalancer from register center.
     *
     * @param serviceId serviceId
     * @return ServiceInstance
     */
    private Upstream doSelect(final String serviceId, final String ip, final String loadbalancer) {
        List<Upstream> choose = this.buildUpstream(serviceId);
        return this.doSelect(choose, loadbalancer, ip);
    }

    /**
     * execute loadbalancer by shenyu loadbalancer.
     *
     * @param upstreamList upstream list
     * @return ServiceInstance
     */
    private Upstream doSelect(final List<Upstream> upstreamList, final String loadbalancer, final String ip) {
        return LoadBalancerFactory.selector(upstreamList, loadbalancer, ip);
    }

    /**
     * get service instance by serviceId.
     *
     * @param serviceId serviceId
     * @return {@linkplain ServiceInstance}
     */
    private List<InstanceEntity> getServiceInstance(final String serviceId) {
        if (CollectionUtils.isNotEmpty(ServiceInstanceCache.getServiceInstance(serviceId))) {
            return ServiceInstanceCache.getServiceInstance(serviceId);
        }
        List<InstanceEntity> instances = null;
        if (Objects.nonNull(SpringCloudPluginDataHandler.getRepository())) {
            instances = SpringCloudPluginDataHandler.getRepository().selectInstances(serviceId);
        }
        final List<InstanceEntity> instanceEntities = Optional.ofNullable(instances).orElse(Collections.emptyList());
        LOG.info("ShenyuSpringCloudServiceChooser selectInstance size: {}", instanceEntities.size());
        return instanceEntities;
    }

    /**
     * build upstream by service instance.
     *
     * @param serviceId serviceId
     * @return Upstream List
     */
    private List<Upstream> buildUpstream(final String serviceId) {
        List<InstanceEntity> serviceInstanceList = this.getServiceInstance(serviceId);
        if (CollectionUtils.isEmpty(serviceInstanceList)) {
            return Collections.emptyList();
        }
        return serviceInstanceList.stream()
                .map(serviceInstance -> buildDefaultSpringCloudUpstream(serviceInstance.getUri().getRawAuthority(),
                        serviceInstance.getUri().getScheme() + "://"))
                .distinct()
                .collect(Collectors.toList());
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
                .warmup(Constants.WARMUP_TIME)
                .timestamp(0)
                .build();
    }
}
