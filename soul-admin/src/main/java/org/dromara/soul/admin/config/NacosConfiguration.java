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

package org.dromara.soul.admin.config;

import java.util.Properties;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;

/**
 * Nacos configuration.
 *
 * @author xiaoyu
 */
@EnableConfigurationProperties(NacosConfig.class)
public class NacosConfiguration {
    
    /**
     * register configService in spring ioc.
     *
     * @param nacosConfig the nacos configuration
     * @return ConfigService {@linkplain ConfigService}
     * @throws Exception the exception
     */
    @Bean
    public ConfigService nacosConfigService(final NacosConfig nacosConfig) throws Exception {
        Properties properties = new Properties();
        if (nacosConfig.getAcm() != null && nacosConfig.getAcm().isEnabled()) {
            //使用阿里云ACM服务
            properties.put(PropertyKeyConst.ENDPOINT, nacosConfig.getAcm().getEndpoint());
            properties.put(PropertyKeyConst.NAMESPACE, nacosConfig.getAcm().getNamespace());
            //使用子账户ACM管理权限
            properties.put(PropertyKeyConst.ACCESS_KEY, nacosConfig.getAcm().getAccessKey());
            properties.put(PropertyKeyConst.SECRET_KEY, nacosConfig.getAcm().getSecretKey());
        } else {
            properties.put(PropertyKeyConst.SERVER_ADDR, nacosConfig.getUrl());
            properties.put(PropertyKeyConst.NAMESPACE, nacosConfig.getNamespace());
        }
        return NacosFactory.createConfigService(properties);
    }
}
