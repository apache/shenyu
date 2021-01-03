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

package org.dromara.soul.client.tars;

import org.dromara.soul.client.tars.common.annotation.SoulTarsClient;
import org.dromara.soul.client.tars.common.annotation.SoulTarsService;
import org.dromara.soul.client.tars.common.config.TarsConfig;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Test case for {@link TarsServiceBeanPostProcessor}.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class TarsServiceBeanPostProcessorTest {
    private static TarsServiceBeanPostProcessor tarsServiceBeanPostProcessor;

    @BeforeClass
    public static void init() {
        TarsConfig mockTarsConfig = new TarsConfig();
        mockTarsConfig.setAdminUrl("http://localhost:58080");
        mockTarsConfig.setAppName("tars");
        mockTarsConfig.setContextPath("/tars");
        mockTarsConfig.setIpAndPort("localhost:58080");
        tarsServiceBeanPostProcessor = new TarsServiceBeanPostProcessor(mockTarsConfig);
    }

    @Test
    public void testPostProcessAfterInitialization() {
        TarsDemoService serviceFactoryBean = new TarsDemoService();
        tarsServiceBeanPostProcessor
                .postProcessAfterInitialization(serviceFactoryBean, "SoulTarsTest");
    }

    @Test
    public void testPostProcessNormalBean() {
        tarsServiceBeanPostProcessor
                .postProcessAfterInitialization(new Object(), "normalBean");
    }

    @SoulTarsService(serviceName = "testObj")
    static class TarsDemoService {
        @SoulTarsClient(path = "hello")
        public String test(final String hello) {
            return hello + "";
        }
    }
}
