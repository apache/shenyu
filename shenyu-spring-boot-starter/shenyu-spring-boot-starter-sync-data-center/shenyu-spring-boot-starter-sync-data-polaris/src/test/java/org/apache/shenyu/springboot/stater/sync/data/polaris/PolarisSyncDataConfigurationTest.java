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

package org.apache.shenyu.springboot.stater.sync.data.polaris;

import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.client.DefaultConfigFileService;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.springboot.starter.sync.data.polaris.PolarisSyncDataConfiguration;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.polaris.config.PolarisConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Answers.CALLS_REAL_METHODS;

/**
 * The test case for {@link PolarisSyncDataConfiguration}.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(
        classes = PolarisSyncDataConfiguration.class,
        properties = {
            "shenyu.sync.polaris.url=" + PolarisSyncDataConfigurationTest.URL,
            "shenyu.sync.polaris.namespace=default",
            "shenyu.sync.polaris.fileGroup=fileGroup"
        })
@EnableAutoConfiguration
@MockBean(name = "shenyuConfig", value = ShenyuConfig.class, answer = CALLS_REAL_METHODS)
public final class PolarisSyncDataConfigurationTest {

    public static final String URL = "127.0.0.1:8093";

    @Autowired
    private SyncDataService syncDataService;

    @Autowired
    private PolarisConfig polarisConfig;

    @Test
    public void polarisConfigServiceTest() {
        assertNotNull(syncDataService);
        assertNotNull(polarisConfig);
        final PolarisSyncDataConfiguration polarisSyncDataConfiguration = new PolarisSyncDataConfiguration();

        final ConfigFileService configFileService = Assertions.assertDoesNotThrow(() -> polarisSyncDataConfiguration.polarisConfigServices(polarisConfig));
        assertInstanceOf(DefaultConfigFileService.class, configFileService);
        DefaultConfigFileService defaultConfigFileService = (DefaultConfigFileService) configFileService;
        final SDKContext sdkContext = defaultConfigFileService.getSDKContext();
        assertTrue(sdkContext.getConfig().getConfigFile().getServerConnector().getAddresses().contains(PolarisSyncDataConfigurationTest.URL));
    }
}
