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
import org.apache.shenyu.admin.listener.etcd.EtcdDataChangedInit;
import org.apache.shenyu.admin.listener.etcd.EtcdDataDataChangedListener;
import org.apache.shenyu.infra.etcd.autoconfig.ConditionOnSyncEtcd;
import org.apache.shenyu.infra.etcd.autoconfig.EtcdConfiguration;
import org.apache.shenyu.infra.etcd.client.EtcdClient;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Etcd listener.
 */

@Configuration
@ConditionOnSyncEtcd
@AutoConfiguration(after = { EtcdConfiguration.class })
@ImportAutoConfiguration(EtcdConfiguration.class)
public class EtcdSyncConfiguration {

    /**
     * Config event listener data changed listener.
     *
     * @param etcdClient the etcd client
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(EtcdDataDataChangedListener.class)
    public DataChangedListener etcdDataChangedListener(final EtcdClient etcdClient) {

        return new EtcdDataDataChangedListener(etcdClient);
    }

    /**
     * data init.
     *
     * @param etcdClient the etcd client
     * @return the etcd data init
     */
    @Bean
    @ConditionalOnMissingBean(EtcdDataChangedInit.class)
    public DataChangedInit etcdDataChangedInit(final EtcdClient etcdClient) {

        return new EtcdDataChangedInit(etcdClient);
    }
}
