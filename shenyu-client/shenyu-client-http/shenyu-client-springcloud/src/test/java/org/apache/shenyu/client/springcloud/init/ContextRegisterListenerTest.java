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

import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;

import java.util.Properties;

import static org.mockito.Mockito.mock;

/**
 * Test case for {@link ContextRegisterListener}.
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ContextRegisterListenerTest {

    @Mock
    private static Environment env;

    @Test
    public void testNotFullRegister() {
        Properties properties = new Properties();
        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:58080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(mockRegisterCenter, env, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
    }
}
