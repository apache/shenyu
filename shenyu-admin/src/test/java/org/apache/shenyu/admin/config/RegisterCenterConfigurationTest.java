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

import org.apache.shenyu.admin.disruptor.RegisterClientServerDisruptorPublisher;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.register.ClientRegisterProvisioner;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link RegisterCenterConfiguration}.
 */
@ExtendWith(MockitoExtension.class)
public class RegisterCenterConfigurationTest {
    
    @InjectMocks
    private RegisterCenterConfiguration registerCenterConfiguration;

    private AnnotationConfigApplicationContext applicationContext;

    @AfterEach
    public void tearDown() {
        if (Objects.nonNull(applicationContext)) {
            applicationContext.close();
        }
    }
    
    @Test
    public void testShenyuRegisterCenterConfig() {
        assertEquals(ShenyuRegisterCenterConfig.class, registerCenterConfiguration.shenyuRegisterCenterConfig().getClass());
    }
    
    @Test
    public void testRegisterClientServerDisruptorPublisher() {
        DiscoveryService discoveryService = mock(DiscoveryService.class);
        List<ShenyuClientRegisterService> shenyuClientRegisterService = new ArrayList<>();
        ObjectProvider<ClientRegisterProvisioner> clientRegisterProvisioners = mock(ObjectProvider.class);
        when(clientRegisterProvisioners.orderedStream()).thenReturn(Stream.empty());
        RegisterClientServerDisruptorPublisher publisher = registerCenterConfiguration
                .registerClientServerDisruptorPublisher(shenyuClientRegisterService,
                        clientRegisterProvisioners, discoveryService);
        assertNotNull(publisher);
        publisher.close();
    }

    @Test
    public void testStartsWithoutProvisioners() {
        applicationContext = new AnnotationConfigApplicationContext();
        applicationContext.register(RegisterCenterConfiguration.class);
        applicationContext.registerBean("discoveryService", DiscoveryService.class, () -> mock(DiscoveryService.class));
        applicationContext.registerBean("shenyuClientRegisterService", ShenyuClientRegisterService.class, () -> mock(ShenyuClientRegisterService.class));
        applicationContext.refresh();
        assertNotNull(applicationContext.getBean(RegisterClientServerDisruptorPublisher.class));
    }
}
