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

package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;

public class ShenyuClientRegisterEventPublisherTest {
    @Mock
    private DataTypeParent testData;

    @Mock
    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;

    @Test
    public void testGetInstanceReturnsSameInstance() {
        ShenyuClientRegisterEventPublisher instance1 = ShenyuClientRegisterEventPublisher.getInstance();
        ShenyuClientRegisterEventPublisher instance2 = ShenyuClientRegisterEventPublisher.getInstance();
        assertSame(instance1, instance2);
    }

    @Test
    public void testStart() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        Assertions.assertNotNull(publisher.getProviderManage());
        assertDoesNotThrow(() -> publisher.getProviderManage().startup());
    }

    @Test
    public void testPublishEvent() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        assertDoesNotThrow(() -> publisher.publishEvent(testData));
    }

    @Test
    public void testPublishEventWithNullData() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        assertDoesNotThrow(() -> publisher.publishEvent(null));
    }
}
