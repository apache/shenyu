/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.springboot.starter.client.grpc;

import org.dromara.soul.client.grpc.GrpcClientBeanPostProcessor;
import org.dromara.soul.client.grpc.common.config.GrpcConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Grpc type client bean postprocessor.
 *
 * @author tydhot
 */
@Configuration
public class SoulGrpcClientConfiguration {
    /**
     * Grpc service bean post processor sofa service bean post processor.
     *
     * @param grpcConfig the sofa config
     * @return the grpc service bean post processor
     */
    @Bean
    public GrpcClientBeanPostProcessor grpcServiceBeanPostProcessor(final GrpcConfig grpcConfig) {
        return new GrpcClientBeanPostProcessor(grpcConfig);
    }

    /**
     * Grpc config sofa config.
     *
     * @return the grpc config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.grpc")
    public GrpcConfig grpcConfig() {
        return new GrpcConfig();
    }
}
