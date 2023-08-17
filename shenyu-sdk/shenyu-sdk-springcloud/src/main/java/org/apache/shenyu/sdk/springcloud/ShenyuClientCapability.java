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

import feign.Capability;
import feign.Client;
import feign.Request;
import java.net.URI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerUriTools;
import org.springframework.cloud.openfeign.loadbalancer.FeignBlockingLoadBalancerClient;
import org.springframework.cloud.openfeign.loadbalancer.RetryableFeignBlockingLoadBalancerClient;
import org.springframework.util.Assert;

/**
 * custom a shenyu client capability to enrich clients.
 */
public class ShenyuClientCapability implements Capability {

    public static final String SHENYU_SERVICE_ID = "shenyu-gateway";

    private static final Logger LOGGER = LoggerFactory.getLogger(ShenyuClientCapability.class);

    private final ShenyuDiscoveryClient shenyuDiscoveryClient;

    public ShenyuClientCapability(final ShenyuDiscoveryClient shenyuDiscoveryClient) {
        this.shenyuDiscoveryClient = shenyuDiscoveryClient;
    }

    @Override
    public Client enrich(final Client finalDelegate) {
        return (request, options) -> {
            Client delegate = finalDelegate;
            final URI originalUri = URI.create(request.url());
            String serviceId = originalUri.getHost();
            Assert.state(serviceId != null, "Request URI does not contain a valid hostname: " + originalUri);
            if (SHENYU_SERVICE_ID.equals(serviceId)) {
                if (finalDelegate instanceof FeignBlockingLoadBalancerClient) {
                    delegate = ((FeignBlockingLoadBalancerClient) finalDelegate).getDelegate();
                } else if (finalDelegate instanceof RetryableFeignBlockingLoadBalancerClient) {
                    delegate = ((RetryableFeignBlockingLoadBalancerClient) finalDelegate).getDelegate();
                }
                final ServiceInstance serviceInstance = shenyuDiscoveryClient.getInstances(SHENYU_SERVICE_ID).get(0);
                String reconstructedUrl = LoadBalancerUriTools.reconstructURI(serviceInstance, originalUri).toString();
                Request newRequest = buildRequest(request, reconstructedUrl);
                LOGGER.info("shenyuClientCapability enrich client execute Uri {} to {}", originalUri, reconstructedUrl);
                return delegate.execute(newRequest, options);
            }
            return delegate.execute(request, options);
        };
    }

    private Request buildRequest(final Request request, final String reconstructedUrl) {
        return Request.create(request.httpMethod(), reconstructedUrl, request.headers(), request.body(),
            request.charset(), request.requestTemplate());
    }

}
