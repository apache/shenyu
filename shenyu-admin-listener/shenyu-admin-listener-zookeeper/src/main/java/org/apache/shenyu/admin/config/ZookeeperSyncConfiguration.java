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
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.infra.zookeeper.autoconfig.ConditionOnSyncZk;
import org.apache.shenyu.infra.zookeeper.autoconfig.ZookeeperConfiguration;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
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
@ConditionOnSyncZk
@AutoConfiguration(after = { ZookeeperConfiguration.class })
@ImportAutoConfiguration(ZookeeperConfiguration.class)
public class ZookeeperSyncConfiguration {

    /**
     * Config event listener data changed listener.
     *
     * @param zkClient the zk client
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperDataChangedListener.class)
    public DataChangedListener zookeeperDataChangedListener(final ZookeeperClient zkClient) {

        if (Objects.isNull(zkClient)) {
            throw new ShenyuException("ZookeeperClient must not be null");
        }
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

        if (Objects.isNull(zkClient) || zkClient.isConnection()) {
            throw new ShenyuException("ZookeeperClient must not be null");
        }

        return new ZookeeperDataChangedInit(zkClient);
    }

}
