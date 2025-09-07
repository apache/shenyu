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

package org.apache.shenyu.infra.etcd.autoconfig;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.infra.common.InfraConstants;
import org.apache.shenyu.infra.common.InfraParentProperties;
import org.apache.shenyu.infra.etcd.config.EtcdConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

@ConfigurationProperties(InfraParentProperties.PARENT_CONFIG_PREFIX)
public class EtcdProperties extends InfraParentProperties {

    public static final String CONFIG_PREFIX = PARENT_CONFIG_PREFIX + Constants.DOT + InfraConstants.SHENYU_ETCD;

    private static final Integer DEFAULT_SESSION_TIMEOUT = 30 * 1000;

    private static final Integer DEFAULT_CONNECT_TIMEOUT = 30 * 1000;

    @NestedConfigurationProperty
    private EtcdConfig etcd = EtcdConfig.builder()
            .connectionTimeout(DEFAULT_CONNECT_TIMEOUT)
            .sessionTimeout(DEFAULT_SESSION_TIMEOUT)
            .build();

    public EtcdConfig getEtcd() {
        return etcd;
    }

    public void setEtcd(final EtcdConfig etcd) {
        this.etcd = etcd;
    }

}
