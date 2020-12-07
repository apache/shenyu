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

import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.Mockito.mock;

import org.springframework.context.event.ContextRefreshedEvent;

/**
 * ContextRegisterListenerTest.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
public final class ContextRegisterListenerTest {

    private ContextRegisterListener contextRegisterListener;

    @Before
    public void init() {
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("localhost");
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setPort(8080);
        contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
    }

    @Test
    public void testRegister() {
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
    }
}
