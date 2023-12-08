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

import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.shenyu.admin.config.properties.PolarisProperties;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

public class PolarisSyncConfigurationTest {
    
    @Test
    public void testPolarisDataChangedListener() {
        PolarisSyncConfiguration polarisListener = new PolarisSyncConfiguration();
        PolarisProperties polarisProperties = mock(PolarisProperties.class);
        ConfigFileService polarisConfigFileService = mock(ConfigFileService.class);
        ConfigFilePublishService polarisConfigFilePublishService = mock(ConfigFilePublishService.class);
        assertNotNull(polarisListener.polarisDataChangedListener(polarisProperties, polarisConfigFileService, polarisConfigFilePublishService));
    }
    
    @Test
    public void testPolarisDataInit() {
        PolarisSyncConfiguration polarisListener = new PolarisSyncConfiguration();
        PolarisProperties polarisProperties = mock(PolarisProperties.class);
        ConfigFileService polarisConfigFileService = mock(ConfigFileService.class);
        assertNotNull(polarisListener.polarisDataChangedInit(polarisProperties, polarisConfigFileService));
    }
    
    @Test
    public void polarisConfigServiceTest() {
        final PolarisProperties polarisProperties = new PolarisProperties();
        polarisProperties.setUrl("127.0.0.1:8093");
        polarisProperties.setNamespace("namespace");
        PolarisSyncConfiguration polarisListener = new PolarisSyncConfiguration();
        assertNotNull(polarisListener.polarisConfigFileService(polarisProperties));
        assertNotNull(polarisListener.polarisConfigFilePublishService(polarisProperties));
    }
}
