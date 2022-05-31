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

import org.apache.shenyu.admin.config.properties.ZookeeperProperties;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import java.util.Objects;

/**
 * ZookeeperConfiguration.
 *
 * @deprecated {@link DataSyncConfiguration.ZookeeperListener#zookeeperClient}
 */
@Deprecated
@EnableConfigurationProperties(ZookeeperProperties.class)
public class ZookeeperConfiguration {

    /**
     * register ZookeeperClient in spring ioc.
     *
     * @param zookeeperProp the zookeeper configuration
     * @return ZookeeperClient {@linkplain ZookeeperClient}
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperClient.class)
    public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProp) {
        int sessionTimeout = Objects.isNull(zookeeperProp.getSessionTimeout()) ? 3000 : zookeeperProp.getSessionTimeout();
        int connectionTimeout = Objects.isNull(zookeeperProp.getConnectionTimeout()) ? 3000 : zookeeperProp.getConnectionTimeout();
        ZookeeperConfig zkConfig = new ZookeeperConfig(zookeeperProp.getUrl());
        zkConfig.setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);
        ZookeeperClient client = new ZookeeperClient(zkConfig);
        client.start();
        return client;
    }
}
