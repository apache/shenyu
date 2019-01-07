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

package org.dromara.soul.configuration.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.configuration.zookeeper.serializer.ZkSerializerFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * ZookeeperConfiguration .
 *
 * @author xiaoyu(Myth)
 */
@Configuration
public class ZookeeperConfiguration {

    /**
     * Zookeeper config zookeeper config.
     *
     * @return the zookeeper config
     */
    @Bean
    @ConfigurationProperties(prefix = "spring.zookeeper")
    public ZookeeperConfig zookeeperConfig() {
        return new ZookeeperConfig();
    }

    /**
     * register zkClient in spring ioc.
     *
     * @param zookeeperConfig the zookeeper config
     * @return ZkClient {@linkplain ZkClient}
     */
    @Bean
    public ZkClient zkClient(ZookeeperConfig zookeeperConfig) {
        return new ZkClient(zookeeperConfig.getUrl(),
                zookeeperConfig.getSessionTimeout(),
                zookeeperConfig.getConnectionTimeout(),
                ZkSerializerFactory.of(zookeeperConfig.getSerializer()));
    }
}
