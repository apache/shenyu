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

import feign.Capability;
import feign.Client;
import feign.Request;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerUriTools;
import org.springframework.cloud.openfeign.loadbalancer.FeignBlockingLoadBalancerClient;
import org.springframework.cloud.openfeign.loadbalancer.RetryableFeignBlockingLoadBalancerClient;
import org.springframework.util.Assert;

import java.net.URI;
import java.util.Objects;

/**
 * custom a shenyu client capability to enrich clients.
 */
public final class ShenyuClientCapability implements Capability {

    public static final ShenyuClientCapability INSTANCE = new ShenyuClientCapability();

    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuClientCapability.class);

    private BeanFactory beanFactory;

    private ShenyuClientCapability() {

    }

    @Override
    public Client enrich(final Client finalDelegate) {
        return (request, options) -> {
            Client delegate = finalDelegate;
            final URI originalUri = URI.create(request.url());
            String serviceId = originalUri.getHost();
            Assert.state(Objects.nonNull(serviceId), "Request URI does not contain a valid hostname: " + originalUri);
            if (finalDelegate instanceof FeignBlockingLoadBalancerClient) {
                delegate = ((FeignBlockingLoadBalancerClient) finalDelegate).getDelegate();
            } else if (finalDelegate instanceof RetryableFeignBlockingLoadBalancerClient) {
                delegate = ((RetryableFeignBlockingLoadBalancerClient) finalDelegate).getDelegate();
            }
            final ShenyuDiscoveryClient shenyuDiscoveryClient = beanFactory.getBean(ShenyuDiscoveryClient.class);
            final ServiceInstance serviceInstance = shenyuDiscoveryClient.getInstance(serviceId);
            String reconstructedUrl = LoadBalancerUriTools.reconstructURI(serviceInstance, originalUri).toString();
            Request newRequest = buildRequest(request, reconstructedUrl);
            LOGGER.info("shenyuClientCapability enrich client execute Uri {} to {}", originalUri, reconstructedUrl);
            return delegate.execute(newRequest, options);
        };
    }

    /**
     * set bean factory.
     * @param beanFactory beanFactory
     */
    public void setBeanFactory(final BeanFactory beanFactory) {
        this.beanFactory = beanFactory;
    }

    private Request buildRequest(final Request request, final String reconstructedUrl) {
        return Request.create(request.httpMethod(), reconstructedUrl, request.headers(), request.body(),
            request.charset(), request.requestTemplate());
    }

}
