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

package org.apache.shenyu.infra.nacos.autoconfig;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.infra.common.InfraConstants;
import org.apache.shenyu.infra.common.InfraParentProperties;
import org.apache.shenyu.infra.nacos.config.NacosConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

import static org.apache.shenyu.infra.nacos.autoconfig.NacosProperties.CONFIG_PREFIX;

/**
 * The type Nacos config.
 */
@ConfigurationProperties(InfraParentProperties.PARENT_CONFIG_PREFIX)
public class NacosProperties extends InfraParentProperties {

    public static final String CONFIG_PREFIX = PARENT_CONFIG_PREFIX + Constants.DOT + InfraConstants.SHENYU_NACOS;

    @NestedConfigurationProperty
    private NacosConfig nacos = NacosConfig.builder().build();

    public NacosConfig getNacos() {
        return nacos;
    }

    public void setNacos(final NacosConfig nacos) {
        this.nacos = nacos;
    }

}
