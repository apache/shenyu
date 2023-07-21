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

import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.cloud.client.loadbalancer.DefaultResponse;
import org.springframework.cloud.client.loadbalancer.Request;
import org.springframework.cloud.client.loadbalancer.Response;
import org.springframework.cloud.loadbalancer.core.ReactorServiceInstanceLoadBalancer;
import reactor.core.publisher.Mono;

public class ShenyuServiceInstanceLoadBalancer implements ReactorServiceInstanceLoadBalancer {

    public static final String SHENYU_SERVICE_ID = "shenyu-gateway";

    private final DiscoveryClient discoveryClient;

    public ShenyuServiceInstanceLoadBalancer(final DiscoveryClient discoveryClient) {
        this.discoveryClient = discoveryClient;
    }

    @Override
    public Mono<Response<ServiceInstance>> choose(final Request request) {
        return Mono.just(new DefaultResponse(discoveryClient.getInstances(SHENYU_SERVICE_ID).get(0)));
    }

    @Override
    public Mono<Response<ServiceInstance>> choose() {
        return ReactorServiceInstanceLoadBalancer.super.choose();
    }
}
