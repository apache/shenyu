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

package org.apache.shenyu.admin.config;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.admin.disruptor.RegisterServerDisruptorPublisher;
import org.apache.shenyu.admin.service.SoulClientRegisterService;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.apache.shenyu.register.server.api.SoulServerRegisterRepository;
import org.apache.shenyu.spi.ExtensionLoader;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Register center configuration.
 *
 * @author xiaoyu
 */
@Slf4j
@Configuration
public class RegisterCenterConfiguration {
    
    /**
     * Soul register center config soul register center config.
     *
     * @return the soul register center config
     */
    @Bean
    @ConfigurationProperties(prefix = "soul.register")
    public SoulRegisterCenterConfig soulRegisterCenterConfig() {
        return new SoulRegisterCenterConfig();
    }
    
    /**
     * Soul server register repository soul server register repository.
     *
     * @param soulRegisterCenterConfig the soul register center config
     * @param soulClientRegisterService the soul client register service
     * @return the soul server register repository
     */
    @Bean
    public SoulServerRegisterRepository soulServerRegisterRepository(final SoulRegisterCenterConfig soulRegisterCenterConfig, 
                                                                     final SoulClientRegisterService soulClientRegisterService) {
        String registerType = soulRegisterCenterConfig.getRegisterType();
        SoulServerRegisterRepository registerRepository = ExtensionLoader.getExtensionLoader(SoulServerRegisterRepository.class).getJoin(registerType);
        RegisterServerDisruptorPublisher publisher = RegisterServerDisruptorPublisher.getInstance();
        publisher.start(soulClientRegisterService);
        registerRepository.init(publisher, soulRegisterCenterConfig);
        return registerRepository;
    }
}
