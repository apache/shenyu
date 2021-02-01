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

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.register.zookeeper.ZookeeperRegisterProperties;
import org.dromara.soul.admin.register.zookeeper.ZookeeperRegisterService;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * register center configuration.
 *
 * @author lw1243925457
 */
@Configuration
public class RegisterCenterConfiguration {

    @Configuration
    @ConditionalOnProperty(name = "soul.register.registerType.zookeeper", havingValue = "true")
    @Import(ZookeeperRegisterProperties.class)
    static class ZookeeperRegister {

        @Bean
        public ZookeeperRegisterService zookeeperRegisterService(final ZookeeperRegisterProperties properties,
                                                                 final SoulClientRegisterService soulClientRegisterService) {
            ZkClient zkClient = new ZkClient(properties.getUrl(), properties.getConnectionTimeout(),
                    properties.getSessionTimeout());
            return new ZookeeperRegisterService(zkClient, soulClientRegisterService);
        }
    }
}
