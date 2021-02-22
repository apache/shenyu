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
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.disruptor.SoulServerMetaDataRegisterEventPublisher;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.register.server.api.SoulServerRegisterRepository;
import org.dromara.soul.register.server.zookeeper.ZookeeperConfiguration;
import org.dromara.soul.register.server.zookeeper.ZookeeperServerRegisterRepository;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * @author lw1243925457
 */
@Slf4j
@Configuration
public class RegisterCenterConfiguration {

    @Configuration
    @ConditionalOnProperty(name = "soul.register.registerType.zookeeper", havingValue = "true")
    @Import(ZookeeperConfiguration.class)
    static class ZookeeperRegisterCenter {

        @Bean
        public SoulServerRegisterRepository soulServerRegisterRepository(final ZkClient zkClient,
                                                                         final SoulClientRegisterService soulClientRegisterService,
                                                                         final SelectorService selectorService,
                                                                         final SelectorMapper selectorMapper,
                                                                         final ApplicationEventPublisher eventPublisher) {
            log.info("you use zookeeper register center");
            SoulServerMetaDataRegisterEventPublisher publisher = SoulServerMetaDataRegisterEventPublisher.getInstance();
            publisher.start(soulClientRegisterService, selectorService, selectorMapper, eventPublisher);
            return new ZookeeperServerRegisterRepository(publisher, zkClient);
        }
    }
}
