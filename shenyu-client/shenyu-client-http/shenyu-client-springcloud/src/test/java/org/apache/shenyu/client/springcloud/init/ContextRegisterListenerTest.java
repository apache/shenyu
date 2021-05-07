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

package org.apache.shenyu.client.springcloud.init;

import org.apache.shenyu.client.core.register.SoulClientRegisterRepositoryFactory;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.junit.FixMethodOrder;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;

import java.util.Properties;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ContextRegisterListener}.
 *
 * @author kaitoShy
 * @author dengliming
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ContextRegisterListenerTest {
    
    @Mock
    private static Environment env;

    @Test
    @Ignore
    public void testNotFullRegister() {
        Properties properties = new Properties();
        SoulRegisterCenterConfig mockRegisterCenter = new SoulRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:58080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(mockRegisterCenter, env, SoulClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
    }

//    @Test
//    public void testFullRegister() {
//        try (MockedStatic mocked = mockStatic(RegisterUtils.class)) {
//            Properties properties = new Properties();
//            properties.setProperty("contextPath", "/test");
//            properties.setProperty("isFull", "true");
//            SoulRegisterCenterConfig mockRegisterCenter = new SoulRegisterCenterConfig();
//            mockRegisterCenter.setServerLists("http://127.0.0.1:8080");
//            mockRegisterCenter.setRegisterType("http");
//            mockRegisterCenter.setProps(properties);
//            when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
//            ContextRegisterListener contextRegisterListener = new ContextRegisterListener(mockRegisterCenter, env);
//            ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
//            contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
//            mocked.verify(() -> RegisterUtils.doRegister(anyString(), eq("http://127.0.0.1:8080/soul-client/springcloud-register"), eq(RpcTypeEnum.SPRING_CLOUD)));
//        }
//    }
}
