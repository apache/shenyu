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
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.plugin.GrpcRegisterConfig;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.grpc.intercept.ContextClientInterceptor;
import org.apache.shenyu.plugin.grpc.loadbalance.LoadBalancerStrategy;
import org.apache.shenyu.plugin.grpc.loadbalance.RandomLoadBalancerProvider;
import org.apache.shenyu.plugin.grpc.loadbalance.RoundRobinLoadBalancerProvider;
import org.apache.shenyu.plugin.grpc.resolver.ShenyuNameResolverProvider;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;

import java.util.Optional;
import java.util.concurrent.Executor;

/**
 * Grpc client Builder.
 */
public final class GrpcClientBuilder {

    static {
        LoadBalancerRegistry.getDefaultRegistry().register(new RandomLoadBalancerProvider());
        LoadBalancerRegistry.getDefaultRegistry().register(new RoundRobinLoadBalancerProvider());
        NameResolverRegistry.getDefaultRegistry().register(new ShenyuNameResolverProvider());
    }

    private GrpcClientBuilder() {
    }

    /**
     * Build the client.
     *
     * @param contextPath contextPath
     * @return ShenyuGrpcClient  shenyuGrpcClient
     */
    public static ShenyuGrpcClient buildClient(final String contextPath) {
        ManagedChannelBuilder<?> builder = ManagedChannelBuilder.forTarget(contextPath)
                .intercept(new ContextClientInterceptor())
                .defaultLoadBalancingPolicy(LoadBalancerStrategy.RANDOM.getStrategy())
                .usePlaintext()
                .maxInboundMessageSize(100 * 1024 * 1024)
                .executor(buildExecutor())
                .disableRetry();
        ManagedChannel channel = builder.build();
        channel.getState(true);
        return new ShenyuGrpcClient(channel);
    }

    /**
     * get thread pool, just for integrated test.
     *
     * @return the thread pool
     */
    public static Executor buildExecutor() {
        GrpcRegisterConfig config = Singleton.INST.get(GrpcRegisterConfig.class);
        if (null == config) {
            return null;
        }
        final String threadpool = Optional.ofNullable(config.getThreadpool()).orElse(Constants.CACHED);
        switch (threadpool) {
            case Constants.SHARED:
                try {
                    return SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class);
                } catch (NoSuchBeanDefinitionException t) {
                    throw new ShenyuException("shared thread pool is not enable, config ${shenyu.sharedPool.enable} in your xml/yml !", t);
                }
            case Constants.FIXED:
            case Constants.EAGER:
            case Constants.LIMITED:
                throw new UnsupportedOperationException();
            case Constants.CACHED:
            default:
                return null;
        }
    }
}
