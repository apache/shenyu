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

package org.dromara.soul.admin.config;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.admin.disruptor.RegisterServerDisruptorPublisher;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.server.api.SoulServerRegisterRepository;
import org.dromara.soul.register.server.zookeeper.ZookeeperServerRegisterRepository;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Register center configuration.
 *
 * @author lw1243925457
 */
@Slf4j
@Configuration
public class RegisterCenterConfiguration {
    
    /**
     * The type Zookeeper register center.
     */
    @Configuration
    @ConditionalOnProperty(name = "soul.register.registerType", havingValue = "zookeeper")
    static class ZookeeperRegisterCenter {
    
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
                                                                       
            log.info("you use zookeeper register center");
            RegisterServerDisruptorPublisher publisher = RegisterServerDisruptorPublisher.getInstance();
            publisher.start(soulClientRegisterService);
            return new ZookeeperServerRegisterRepository(publisher, soulRegisterCenterConfig);
        }
    }
}
