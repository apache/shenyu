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

import org.apache.shenyu.admin.listener.DataChangedInit;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedInit;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.apache.shenyu.infra.zookeeper.autoconfig.ZookeeperConfiguration;
import org.apache.shenyu.infra.zookeeper.autoconfig.ZookeeperProperties;
import org.apache.shenyu.infra.zookeeper.autoconfig.ConditionOnSyncZookeeper;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
import org.apache.shenyu.infra.zookeeper.config.ZookeeperConfig;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Objects;

/**
 * The type Zookeeper listener.
 */

@Configuration
@ConditionOnSyncZookeeper
@AutoConfiguration(after = {ZookeeperConfiguration.class})
@ImportAutoConfiguration(ZookeeperConfiguration.class)
public class ZookeeperSyncConfiguration {

    /**
     * register ZookeeperClient in spring ioc.
     *
     * @param zookeeperProp the zookeeper configuration
     * @return ZookeeperClient {@linkplain ZookeeperClient}
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperClient.class)
    public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProp) {

        int sessionTimeout = Objects.isNull(zookeeperProp.getZookeeper().getSessionTimeoutMilliseconds()) ? 3000 : zookeeperProp.getZookeeper().getSessionTimeoutMilliseconds();
        int connectionTimeout = Objects.isNull(zookeeperProp.getZookeeper().getConnectionTimeoutMilliseconds()) ? 3000 : zookeeperProp.getZookeeper().getConnectionTimeoutMilliseconds();

        ZookeeperConfig zkConfig = ZookeeperConfig.builder()
                .url(zookeeperProp.getZookeeper().getUrl())
                .sessionTimeoutMilliseconds(sessionTimeout)
                .connectionTimeoutMilliseconds(connectionTimeout)
                .build();
        return new ZookeeperClient(zkConfig);
    }

    /**
     * Config event listener data changed listener.
     *
     * @param zkClient the zk client
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperDataChangedListener.class)
    public DataChangedListener zookeeperDataChangedListener(final ZookeeperClient zkClient) {
        return new ZookeeperDataChangedListener(zkClient);
    }

    /**
     * Zookeeper data init zookeeper data init.
     *
     * @param zkClient the zk client
     * @return the zookeeper data init
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperDataChangedInit.class)
    public DataChangedInit zookeeperDataChangedInit(final ZookeeperClient zkClient) {
        return new ZookeeperDataChangedInit(zkClient);
    }
}
