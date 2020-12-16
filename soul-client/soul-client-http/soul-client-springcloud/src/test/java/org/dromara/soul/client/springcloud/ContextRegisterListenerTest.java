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

package org.dromara.soul.client.springcloud;

import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.init.ContextRegisterListener;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * ContextRegisterListenerTest.
 *
 * @author kaitoShy
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ContextRegisterListenerTest {
    @Mock
    private static Environment env;

    @Test
    public void testNotFullRegister() throws InterruptedException {
        SoulSpringCloudConfig soulSpringCloudConfig = new SoulSpringCloudConfig();
        soulSpringCloudConfig.setAdminUrl("http://127.0.0.1:58080");
        soulSpringCloudConfig.setContextPath("test");
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringCloudConfig, env);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
    }

    @Test
    public void testFullRegister() throws InterruptedException {
        SoulSpringCloudConfig soulSpringCloudConfig = new SoulSpringCloudConfig();
        soulSpringCloudConfig.setAdminUrl("http://127.0.0.1:8080");
        soulSpringCloudConfig.setContextPath("test");
        soulSpringCloudConfig.setFull(true);
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringCloudConfig, env);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
    }
}
