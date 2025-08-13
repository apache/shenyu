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
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.client.config.NacosConfigService;
import org.apache.shenyu.infra.nacos.autoconfig.NacosProperties;
import org.apache.shenyu.infra.nacos.config.NacosACMConfig;
import org.apache.shenyu.infra.nacos.config.NacosConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Properties;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

public class NacosSyncConfigurationTest {
    @Test
    public void testNacosDataChangedListener() {
        NacosSyncConfiguration nacosListener = new NacosSyncConfiguration();
        NacosConfigService configService = mock(NacosConfigService.class);
        assertNotNull(nacosListener.nacosDataChangedListener(configService));
    }
    
    @Test
    public void testNacosDataInit() {
        NacosSyncConfiguration nacosListener = new NacosSyncConfiguration();
        NacosConfigService configService = mock(NacosConfigService.class);
        assertNotNull(nacosListener.nacosDataChangedInit(configService));
    }
    
    @Test
    public void nacosConfigServiceTest() {
        try (MockedStatic<NacosFactory> nacosFactoryMockedStatic = mockStatic(NacosFactory.class)) {

            var nacosProperties = new NacosProperties();
            nacosProperties.setNacos(NacosConfig.builder()
                            .namespace("url")
                            .username("username")
                            .password("password")
                            .acm(NacosACMConfig.builder()
                                    .endpoint("acm.aliyun.com")
                                    .accessKey("accessKey")
                                    .secretKey("secretKey")
                                    .namespace("namespace")
                                    .build())
                    .build());

            nacosFactoryMockedStatic.when(() -> NacosFactory.createConfigService(any(Properties.class))).thenReturn(mock(ConfigService.class));
            NacosSyncConfiguration nacosListener = new NacosSyncConfiguration();

            nacosProperties.getNacos().setUrl("url");
            Assertions.assertDoesNotThrow(() -> nacosListener.nacosConfigService(nacosProperties));

            nacosProperties.getNacos().setNamespace("url");
            nacosProperties.getNacos().setUsername("username");
            nacosProperties.getNacos().setPassword("password");
            Assertions.assertDoesNotThrow(() -> nacosListener.nacosConfigService(nacosProperties));
            Assertions.assertDoesNotThrow(() -> nacosListener.nacosConfigService(nacosProperties));
        }
    }
}
