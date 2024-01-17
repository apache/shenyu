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

 package org.apache.shenyu.client.core.disruptor.executor;

 import com.google.common.collect.Lists;
 import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
 import org.apache.shenyu.register.common.subsriber.AbstractQueueConsumerFactory;
 import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
 import org.apache.shenyu.register.common.type.DataType;
 import org.apache.shenyu.register.common.type.DataTypeParent;
 import org.junit.jupiter.api.BeforeEach;
 import org.junit.jupiter.api.Test;

 import java.util.EnumMap;
 import java.util.Map;
 import java.util.stream.Collectors;

 import static org.junit.jupiter.api.Assertions.assertEquals;
 import static org.mockito.Mockito.mock;
 import static org.mockito.Mockito.verify;
 import static org.mockito.Mockito.never;
 import static org.mockito.Mockito.any;
 import static org.mockito.Mockito.times;
 
/**
 * Test for {@link RegisterClientConsumerExecutor}.
 */
public class RegisterClientConsumerExecutorTest {

    private Map<DataType, ExecutorTypeSubscriber<DataTypeParent>> subscribers;
    private RegisterClientConsumerExecutor<DataTypeParent> consumerExecutor;

    @BeforeEach
    public void setUp() {
        subscribers = new EnumMap<>(DataType.class);

        ExecutorTypeSubscriber<DataTypeParent> subscriber1 = mock(ExecutorTypeSubscriber.class);
        when(subscriber1.getType()).thenReturn(DataType.TYPE1);
        subscribers.put(DataType.TYPE1, subscriber1);

        ExecutorTypeSubscriber<DataTypeParent> subscriber2 = mock(ExecutorTypeSubscriber.class);
        when(subscriber2.getType()).thenReturn(DataType.TYPE2);
        subscribers.put(DataType.TYPE2, subscriber2);

        consumerExecutor = new RegisterClientConsumerExecutor<>(subscribers);
    }

    @Test
    public void testRunWithValidData() {
        DataTypeParent data = mock(DataTypeParent.class);
        when(data.getType()).thenReturn(DataType.TYPE1);
        consumerExecutor.setData(data);

        consumerExecutor.run();

        verify(subscribers.get(DataType.TYPE1), times(1)).executor(eq(Lists.newArrayList(data)));
        verify(subscribers.get(DataType.TYPE2), never()).executor(any());
    }

    @Test
    public void testRunWithInvalidData() {
        DataTypeParent data = mock(DataTypeParent.class);
        when(data.getType()).thenReturn(DataType.INVALID_TYPE);
        consumerExecutor.setData(data);

        consumerExecutor.run();

        verify(subscribers.get(DataType.INVALID_TYPE), never()).executor(any());
    }

    /**
     * Test for {@link RegisterClientConsumerExecutor.RegisterClientExecutorFactory}.
     */
    public static class RegisterClientExecutorFactoryTest {

        private List<ExecutorTypeSubscriber<DataTypeParent>> subscriberList;
        private RegisterClientConsumerExecutor.RegisterClientExecutorFactory<DataTypeParent> executorFactory;

        @BeforeEach
        public void setUp() {
            subscriberList = new ArrayList<>();

            // Mock subscribers
            ExecutorTypeSubscriber<DataTypeParent> subscriber1 = mock(ExecutorTypeSubscriber.class);
            when(subscriber1.getType()).thenReturn(DataType.TYPE1);
            subscriberList.add(subscriber1);

            ExecutorTypeSubscriber<DataTypeParent> subscriber2 = mock(ExecutorTypeSubscriber.class);
            when(subscriber2.getType()).thenReturn(DataType.TYPE2);
            subscriberList.add(subscriber2);

            executorFactory = new RegisterClientConsumerExecutor.RegisterClientExecutorFactory<>(subscriberList);
        }

        @Test
        public void testCreate() {
            RegisterClientConsumerExecutor<DataTypeParent> executor = executorFactory.create();

            assertNotNull(executor);
            assertEquals(subscriberList.size(), executor.getSubscribers().size());
        }

        @Test
        public void testFixName() {
            String expectedName = "shenyu_register_client";
            String actualName = executorFactory.fixName();

            assertEquals(expectedName, actualName);
        }
    }
}
