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
import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.NacosProperties;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import java.util.Properties;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Test cases for NacosConfiguration.
 */
public final class NacosConfigurationTest extends AbstractConfigurationTest {

    private final String[] inlinedProperties = new String[]{
        "shenyu.sync.nacos.url=localhost:8848",
        "shenyu.sync.nacos.namespace=1c10d748-af86-43b9-8265-75f487d20c6c",
        "shenyu.sync.nacos.username=nacos",
        "shenyu.sync.nacos.password=nacos",
        "shenyu.sync.nacos.acm.enabled=false",
        "shenyu.sync.nacos.acm.endpoint=acm.aliyun.com",
    };

    @Test
    public void testNacosConfigServiceMissingBean() {
        load(NacosConfiguration.class, inlinedProperties);
        ConfigService configService = (ConfigService) getContext().getBean("nacosConfigService");
        assertNotNull(configService);
    }

    @Test
    public void testNacosConfigService() {
        String[] inlinedProperties = new String[]{
            "shenyu.sync.nacos.url=localhost:8848",
            "shenyu.sync.nacos.namespace=1c10d748-af86-43b9-8265-75f487d20c6c",
            "shenyu.sync.nacos.acm.enabled=true",
            "shenyu.sync.nacos.acm.endpoint=localhost:8849",
            "shenyu.sync.nacos.acm.namespace=namespace",
            "shenyu.sync.nacos.acm.accessKey=accessKey",
            "shenyu.sync.nacos.acm.secretKey=secretKey",
        };
        try (MockedStatic<NacosFactory> nacosFactoryMockedStatic = Mockito.mockStatic(NacosFactory.class)) {
            final ConfigService configServiceMock = Mockito.mock(ConfigService.class);
            ArgumentCaptor argument = ArgumentCaptor.forClass(Properties.class);
            nacosFactoryMockedStatic
                    .when(() -> NacosFactory.createConfigService((Properties) argument.capture()))
                    .thenReturn(configServiceMock);
            load(NacosConfiguration.class, inlinedProperties);
            assertTrue(((Properties) argument.getValue()).containsKey(PropertyKeyConst.ENDPOINT));
        }
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
                properties.put(PropertyKeyConst.USERNAME, nacosProp.getUsername());
                properties.put(PropertyKeyConst.PASSWORD, nacosProp.getPassword());
            }
            return NacosFactory.createConfigService(properties);
        }
    }
}
