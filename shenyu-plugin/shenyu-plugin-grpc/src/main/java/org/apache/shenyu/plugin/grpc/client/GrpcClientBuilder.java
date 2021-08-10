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

package org.apache.shenyu.plugin.grpc.client;

import io.grpc.LoadBalancerRegistry;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.NameResolverRegistry;
import org.apache.shenyu.plugin.grpc.loadbalance.LoadBalancerStrategy;
import org.apache.shenyu.plugin.grpc.loadbalance.RandomLoadBalancerProvider;
import org.apache.shenyu.plugin.grpc.loadbalance.RoundRobinLoadBalancerProvider;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuNameResolverProvider;

/**
 * Grpc client Builder.
 */
public class GrpcClientBuilder {

    static {
        LoadBalancerRegistry.getDefaultRegistry().register(new RandomLoadBalancerProvider());
        LoadBalancerRegistry.getDefaultRegistry().register(new RoundRobinLoadBalancerProvider());
        NameResolverRegistry.getDefaultRegistry().register(new ShenyuNameResolverProvider());
    }

    /**
     * Build the client.
     *
     * @param contextPath contextPath
     * @return ShenyuGrpcClient  shenyuGrpcClient
     */
    public static ShenyuGrpcClient buildClient(final String contextPath) {
        ManagedChannelBuilder<?> builder = ManagedChannelBuilder.forTarget(contextPath)
                .defaultLoadBalancingPolicy(LoadBalancerStrategy.RANDOM.getStrategy())
                .usePlaintext()
                .maxInboundMessageSize(100 * 1024 * 1024)
                .disableRetry();
        ManagedChannel channel = builder.build();
        channel.getState(true);
        return new ShenyuGrpcClient(channel);
    }
}
