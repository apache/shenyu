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

import org.apache.shenyu.admin.disruptor.RegisterServerDisruptorPublisher;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterServiceFactory;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterRepository;
import org.apache.shenyu.spi.ExtensionLoader;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

/**
 * The type Register center configuration.
 */
@Configuration
public class RegisterCenterConfiguration {

    /**
     * Shenyu register center config shenyu register center config.
     *
     * @return the shenyu register center config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.register")
    public ShenyuRegisterCenterConfig shenyuRegisterCenterConfig() {
        return new ShenyuRegisterCenterConfig();
    }
    
    /**
     * Shenyu server register repository server register repository.
     *
     * @param shenyuRegisterCenterConfig the shenyu register center config
     * @param shenyuClientRegisterService the shenyu client register service
     * @return the shenyu server register repository
     */
    @Bean
    public ShenyuServerRegisterRepository shenyuServerRegisterRepository(final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig,
                                                                         final Map<String, ShenyuClientRegisterServiceFactory> shenyuClientRegisterService) {
        String registerType = shenyuRegisterCenterConfig.getRegisterType();
        ShenyuServerRegisterRepository registerRepository = ExtensionLoader.getExtensionLoader(ShenyuServerRegisterRepository.class).getJoin(registerType);
        RegisterServerDisruptorPublisher publisher = RegisterServerDisruptorPublisher.getInstance();
        publisher.start(shenyuClientRegisterService);
        registerRepository.init(publisher, shenyuRegisterCenterConfig);
        return registerRepository;
    }
}
