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

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedInit;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedInit;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedListener;
import org.apache.shenyu.infra.nacos.autoconfig.ConditionOnSyncNacos;
import org.apache.shenyu.infra.nacos.autoconfig.NacosProperties;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Objects;
import java.util.Properties;

/**
 * The type Nacos listener.
 */

@Configuration
@ConditionOnSyncNacos
@EnableConfigurationProperties(NacosProperties.class)
public class NacosSyncConfiguration {

    /**
     * register configService in spring ioc.
     *
     * @param nacosProp the nacos configuration
     * @return ConfigService {@linkplain ConfigService}
     * @throws Exception the exception
     */
    @Bean
    @ConditionalOnMissingBean(ConfigService.class)
    public ConfigService nacosConfigService(final NacosProperties nacosProp) throws Exception {
        Properties properties = new Properties();
        if (Objects.nonNull(nacosProp.getNacos().getAcm()) && nacosProp.getNacos().getAcm().isEnabled()) {
            // Use aliyun ACM service
            properties.put(PropertyKeyConst.ENDPOINT, nacosProp.getNacos().getAcm().getEndpoint());
            properties.put(PropertyKeyConst.NAMESPACE, nacosProp.getNacos().getAcm().getNamespace());
            // Use subaccount ACM administrative authority
            properties.put(PropertyKeyConst.ACCESS_KEY, nacosProp.getNacos().getAcm().getAccessKey());
            properties.put(PropertyKeyConst.SECRET_KEY, nacosProp.getNacos().getAcm().getSecretKey());
        } else {
            properties.put(PropertyKeyConst.SERVER_ADDR, nacosProp.getNacos().getUrl());
            if (StringUtils.isNotBlank(nacosProp.getNacos().getNamespace())) {
                properties.put(PropertyKeyConst.NAMESPACE, nacosProp.getNacos().getNamespace());
            }
            if (StringUtils.isNotBlank(nacosProp.getNacos().getUsername())) {
                properties.put(PropertyKeyConst.USERNAME, nacosProp.getNacos().getUsername());
            }
            if (StringUtils.isNotBlank(nacosProp.getNacos().getPassword())) {
                properties.put(PropertyKeyConst.PASSWORD, nacosProp.getNacos().getPassword());
            }
            if (StringUtils.isNotBlank(nacosProp.getNacos().getContextPath())) {
                properties.put(PropertyKeyConst.CONTEXT_PATH, nacosProp.getNacos().getContextPath());
            }
        }
        return NacosFactory.createConfigService(properties);
    }

    /**
     * Data changed listener data changed listener.
     *
     * @param configService the config service
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(NacosDataChangedListener.class)
    public DataChangedListener nacosDataChangedListener(final ConfigService configService) {
        return new NacosDataChangedListener(configService);
    }

    /**
     * Nacos data init nacos data init.
     *
     * @param configService the config service
     * @return the nacos data init
     */
    @Bean
    @ConditionalOnMissingBean(NacosDataChangedInit.class)
    public DataChangedInit nacosDataChangedInit(final ConfigService configService) {
        return new NacosDataChangedInit(configService);
    }
}
