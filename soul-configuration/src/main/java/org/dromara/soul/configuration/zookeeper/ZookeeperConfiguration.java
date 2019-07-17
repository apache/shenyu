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
import org.I0Itec.zkclient.serialize.ZkSerializer;
import org.dromara.soul.configuration.zookeeper.serializer.ZkSerializerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

/**
 * ZookeeperConfiguration .
 *
 * @author xiaoyu(Myth)
 */
@EnableConfigurationProperties(ZookeeperConfig.class)
public class ZookeeperConfiguration {

    /**
     * Zk serializer zk serializer.
     *
     * @param zookeeperConfig the zookeeper configuration
     * @return the zk serializer
     */
    @Bean
    @ConditionalOnMissingBean(value = ZkSerializer.class, search = SearchStrategy.ALL)
    public ZkSerializer zkSerializer(ZookeeperConfig zookeeperConfig) {
        return ZkSerializerFactory.of(zookeeperConfig.getSerializer());
    }

    /**
     * register zkClient in spring ioc.
     *
     * @param zookeeperConfig the zookeeper configuration
     * @param zkSerializer    the zk serializer
     * @return ZkClient {@linkplain ZkClient}
     */
    @Bean
    public ZkClient zkClient(ZookeeperConfig zookeeperConfig, ZkSerializer zkSerializer) {
        return new ZkClient(zookeeperConfig.getUrl(),
                zookeeperConfig.getSessionTimeout(),
                zookeeperConfig.getConnectionTimeout(),
                zkSerializer);
    }
}
