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

package org.apache.shenyu.plugin.springcloud.loadbalance.client;

import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.Request;
import org.springframework.cloud.client.loadbalancer.Response;
import org.springframework.cloud.client.loadbalancer.reactive.ReactiveLoadBalancer;
import org.springframework.cloud.loadbalancer.blocking.client.BlockingLoadBalancerClient;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * blocking loadbalancer client.
 */
public class CustomBlockingLoadBalancerClient extends BlockingLoadBalancerClient {
    private final ReactiveLoadBalancer.Factory<ServiceInstance> loadBalancerClientFactory;

    public CustomBlockingLoadBalancerClient(final ReactiveLoadBalancer.Factory<ServiceInstance> loadBalancerClientFactory) {
        super(loadBalancerClientFactory);
        this.loadBalancerClientFactory = loadBalancerClientFactory;
    }

    @Override
    public <T> ServiceInstance choose(final String serviceId, final Request<T> request) {
        ReactiveLoadBalancer<ServiceInstance> loadBalancer = loadBalancerClientFactory.getInstance(serviceId);
        if (loadBalancer == null) {
            return null;
        }
        CompletableFuture<Response<ServiceInstance>> f = CompletableFuture.supplyAsync(() -> Mono.from(loadBalancer.choose(request)).block());
        Response<ServiceInstance> loadBalancerResponse = null;
        try {
            loadBalancerResponse = f.get(3, TimeUnit.SECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            e.printStackTrace();
        }
        if (loadBalancerResponse == null) {
            return null;
        }
        return loadBalancerResponse.getServer();
    }
}
