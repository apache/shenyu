/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
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

import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.server.consul.ShenyuConsulConfigWatch;
import org.junit.Test;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Properties;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The TestCase for ConsulServerConfiguration.
 */
public final class ConsulServerConfigurationTest {

    @Test
    public void testShenyuConsulConfigWatch() {
        ConsulServerConfiguration configuration = new ConsulServerConfiguration();
        ShenyuRegisterCenterConfig config = mock(ShenyuRegisterCenterConfig.class);
        Properties properties = mock(Properties.class);
        when(config.getProps()).thenReturn(properties);
        when(config.getProps().getProperty(any(), any())).thenReturn("1", "30", "mocked valued");
        ApplicationEventPublisher publisher = mock(ApplicationEventPublisher.class);
        ShenyuConsulConfigWatch shenyuConsulConfigWatch = configuration.shenyuConsulConfigWatch(config, publisher);
        assertNotNull(shenyuConsulConfigWatch);
    }
}
