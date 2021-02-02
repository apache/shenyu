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

package org.dromara.soul.spring.boot.starter.plugin.grpc;

import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.grpc.GrpcPlugin;
import org.dromara.soul.plugin.grpc.param.GrpcBodyParamPlugin;
import org.dromara.soul.plugin.grpc.response.GrpcResponsePlugin;
import org.dromara.soul.plugin.grpc.subscriber.GrpcMetaDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type grpc plugin configuration.
 *
 * @author zhanglei
 */
@Configuration
public class GrpcPluginConfiguration {

    /**
     * grpc plugin soul plugin.
     *
     * @return the tars plugin
     */
    @Bean
    public SoulPlugin grpcPlugin() {
        return new GrpcPlugin();
    }

    /**
     * Body param plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin grpcBodyParamPlugin() {
        return new GrpcBodyParamPlugin();
    }

    /**
     * Grpc response plugin soul plugin.
     *
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin grpcResponsePlugin() {
        return new GrpcResponsePlugin();
    }

    /**
     * Grpc meta data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber grpcMetaDataSubscriber() {
        return new GrpcMetaDataSubscriber();
    }
}
