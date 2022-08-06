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

package org.apache.springboot.starter.client.grpc;

import org.apache.shenyu.client.grpc.GrpcClientEventListener;
import org.apache.shenyu.client.grpc.GrpcContextRefreshedEventListener;
import org.apache.shenyu.client.grpc.server.GrpcServerBuilder;
import org.apache.shenyu.client.grpc.server.GrpcServerRunner;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Grpc type client bean postprocessor.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuGrpcClientConfiguration {
    
    /**
     * Grpc client event listener.
     *
     * @param clientConfig the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     * @return the grpc client bean post processor
     */
    @Bean
    public GrpcClientEventListener grpcClientEventListener(final ShenyuClientConfig clientConfig,
                                                                final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        return new GrpcClientEventListener(clientConfig.getClient().get(RpcTypeEnum.GRPC.getName()), shenyuClientRegisterRepository);
    }
    
    /**
     * Grpc context refreshed event listener.
     *
     * @param clientConfig the client config
     * @return the grpc context refreshed event listener
     */
    @Bean
    public GrpcContextRefreshedEventListener grpcContextRefreshedEventListener(final ShenyuClientConfig clientConfig) {
        return new GrpcContextRefreshedEventListener(clientConfig.getClient().get(RpcTypeEnum.GRPC.getName()));
    }
    
    /**
     * Grpc Server.
     *
     * @param grpcServerBuilder grpcServerBuilder
     * @param grpcClientEventListener grpcClientEventListener
     * @return the grpc server
     */
    @Bean
    public GrpcServerRunner grpcServer(final GrpcServerBuilder grpcServerBuilder,
                                       final GrpcClientEventListener grpcClientEventListener) {
        return new GrpcServerRunner(grpcServerBuilder, grpcClientEventListener);
    }
}
