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

import org.apache.shenyu.admin.cluster.ShenyuClusterSelectMasterJdbcService;
import org.apache.shenyu.admin.cluster.ShenyuClusterSelectMasterService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;

/**
 * The type Cluster configuration.
 */
@Configuration(proxyBeanMethods = false)
public class ClusterConfiguration {

    /**
     * Shenyu select master service.
     * @param jdbcLockRegistry the jdbc lock registry
     * @return the shenyu select master service
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.cluster.select-type"}, havingValue = "jdbc", matchIfMissing = true)
    public ShenyuClusterSelectMasterService shenyuClusterSelectMasterService(final JdbcLockRegistry jdbcLockRegistry) {
        return new ShenyuClusterSelectMasterJdbcService(jdbcLockRegistry);
    }

}
