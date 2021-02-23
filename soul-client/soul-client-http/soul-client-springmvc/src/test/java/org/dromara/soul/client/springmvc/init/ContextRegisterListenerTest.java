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

package org.dromara.soul.client.springmvc.init;

import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import org.springframework.context.event.ContextRefreshedEvent;

/**
 * Test case for {@link ContextRegisterListenerTest}.
 *
 * @author tydhot
 * @author dengliming
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ContextRegisterListenerTest {
    @Test
    public void testNotFullRegister() {
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:8080");
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setPort(58889);
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
    }

    @Test
    public void testFullRegister() {
        try (MockedStatic mocked = mockStatic(RegisterUtils.class)) {
            SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
            soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:8080");
            soulSpringMvcConfig.setAppName("test-mvc");
            soulSpringMvcConfig.setContextPath("test");
            soulSpringMvcConfig.setFull(true);
            soulSpringMvcConfig.setPort(58889);
            ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
            ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
            contextRegisterListener.onApplicationEvent(contextRefreshedEvent);

            mocked.verify(() -> RegisterUtils.doRegister(anyString(), eq("http://127.0.0.1:8080/soul-client/springmvc-register"), eq(RpcTypeEnum.HTTP)));
        }
    }
}
