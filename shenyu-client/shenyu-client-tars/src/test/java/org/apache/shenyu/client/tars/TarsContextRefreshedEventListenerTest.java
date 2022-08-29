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

package org.apache.shenyu.client.tars;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.Properties;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;

/**
 * Test case for {@link TarsContextRefreshedEventListener}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class TarsContextRefreshedEventListenerTest {

    private TarsContextRefreshedEventListener tarsContextRefreshedEventListener;

    @Mock
    private PropertiesConfig clientConfig;

    @BeforeEach
    public void beforeEach() {
        Properties properties = mock(Properties.class);
        when(clientConfig.getProps()).thenReturn(properties);
        // hit error
        Assert.assertThrows(ShenyuClientIllegalArgumentException.class, () -> new TarsContextRefreshedEventListener(clientConfig));

        when(properties.getProperty(ShenyuClientConstants.CONTEXT_PATH)).thenReturn(ShenyuClientConstants.CONTEXT_PATH);
        when(properties.getProperty(ShenyuClientConstants.HOST)).thenReturn(ShenyuClientConstants.HOST);
        when(properties.getProperty(ShenyuClientConstants.PORT)).thenReturn("8899");
        tarsContextRefreshedEventListener = new TarsContextRefreshedEventListener(clientConfig);
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(mock(ShenyuClientRegisterRepository.class));
    }

    @Test
    public void testOnApplicationEvent() {
        tarsContextRefreshedEventListener.onApplicationEvent(mock(ContextRefreshedEvent.class));
        // hit `registered.compareAndSet(false, true)`
        tarsContextRefreshedEventListener.onApplicationEvent(mock(ContextRefreshedEvent.class));
    }
}

