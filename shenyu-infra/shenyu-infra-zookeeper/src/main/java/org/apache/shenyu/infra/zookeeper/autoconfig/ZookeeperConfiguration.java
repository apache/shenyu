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

package org.apache.shenyu.infra.zookeeper.autoconfig;

import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
import org.apache.shenyu.infra.zookeeper.config.ZookeeperConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Objects;

@Configuration
@ConditionOnSyncZk
@EnableConfigurationProperties(ZookeeperProperties.class)
@ConditionalOnClass(CuratorFrameworkFactory.Builder.class)
public class ZookeeperConfiguration {

    private static final Logger log = LoggerFactory.getLogger(ZookeeperConfiguration.class);

    /**
     * register ZookeeperClient in spring ioc.
     *
     * @param zookeeperProp the zookeeper configuration
     * @return ZookeeperClient {@linkplain ZookeeperClient}
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperClient.class)
    public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProp) {

        int sessionTimeout = Objects.isNull(zookeeperProp.getSessionTimeout()) ? 60 * 1000 : zookeeperProp.getSessionTimeout();
        int connectionTimeout = Objects.isNull(zookeeperProp.getConnectionTimeout()) ? 15 * 1000 : zookeeperProp.getConnectionTimeout();


        ZookeeperClient client = ZookeeperClient.builder()
                .config(ZookeeperConfig.builder()
                        .serverLists(zookeeperProp.getUrl())
                        .sessionTimeoutMilliseconds(sessionTimeout)
                        .connectionTimeoutMilliseconds(connectionTimeout)
                        .build()
                ).build();

        log.info("zk client init: {}, url: {}, sessionTimeOut: {}, connectionTimeout: {}", client, zookeeperProp.getUrl(), sessionTimeout, connectionTimeout);
        client.start();

        return client;
    }

}
