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

package org.dromara.soul.admin.config;

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;
import org.dromara.soul.admin.AbstractConfigurationTest;
import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import java.util.Properties;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

/**
 * Test cases for NacosConfiguration.
 *
 * @author caimeijiao
 */
public final class NacosConfigurationTest extends AbstractConfigurationTest {

    private final String[] inlinedProperties = new String[]{
        "soul.sync.nacos.url=localhost:8848",
        "soul.sync.nacos.namespace=1c10d748-af86-43b9-8265-75f487d20c6c",
        "soul.sync.nacos.acm.enabled=false",
        "soul.sync.nacos.acm.endpoint=acm.aliyun.com",
    };

    @Test
    public void testNacosConfigServiceMissingBean() {
        load(NacosConfiguration.class, inlinedProperties);
        ConfigService configService = (ConfigService) getContext().getBean("nacosConfigService");
        assertNotNull(configService);
    }

    @Test(expected = BeanCreationException.class)
    public void testNacosConfigService() {
        String[] inlinedProperties = new String[]{
            "soul.sync.nacos.url=localhost:8848",
            "soul.sync.nacos.namespace=1c10d748-af86-43b9-8265-75f487d20c6c",
            "soul.sync.nacos.acm.enabled=true",
            "soul.sync.nacos.acm.endpoint=localhost:8849",
            "soul.sync.nacos.acm.namespace=namespace",
            "soul.sync.nacos.acm.accessKey=accessKey",
            "soul.sync.nacos.acm.secretKey=secretKey",
        };
        load(NacosConfiguration.class, inlinedProperties);
        ConfigService configService = (ConfigService) getContext().getBean("nacosConfigService");
        assertNotNull(configService);
    }

    @Test
    public void testNacosConfigServiceExistBean() {
        load(CustomNacosConfiguration.class, inlinedProperties);
        boolean isExist = getContext().containsBean("nacosConfigService");
        assertFalse(isExist);

        ConfigService customConfigService = (ConfigService) getContext().getBean("customNacosConfigService");
        assertNotNull(customConfigService);
    }

    @EnableConfigurationProperties(NacosProperties.class)
    static class CustomNacosConfiguration {

        @Bean
        public ConfigService customNacosConfigService(final NacosProperties nacosProp) throws Exception {
            Properties properties = new Properties();
            if (nacosProp.getAcm() != null && nacosProp.getAcm().isEnabled()) {
                //use aliyun ACM service
                properties.put(PropertyKeyConst.ENDPOINT, nacosProp.getAcm().getEndpoint());
                properties.put(PropertyKeyConst.NAMESPACE, nacosProp.getAcm().getNamespace());
                //use children count ACM manager privilege
                properties.put(PropertyKeyConst.ACCESS_KEY, nacosProp.getAcm().getAccessKey());
                properties.put(PropertyKeyConst.SECRET_KEY, nacosProp.getAcm().getSecretKey());
            } else {
                properties.put(PropertyKeyConst.SERVER_ADDR, nacosProp.getUrl());
                properties.put(PropertyKeyConst.NAMESPACE, nacosProp.getNamespace());
            }
            return NacosFactory.createConfigService(properties);
        }
    }
}
