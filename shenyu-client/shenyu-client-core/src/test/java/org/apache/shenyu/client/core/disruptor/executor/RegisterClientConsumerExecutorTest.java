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
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.util.EnumMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.anyList;

public class RegisterClientConsumerExecutorTest {

    private Map<DataType, ExecutorTypeSubscriber<DataTypeParent>> subscribers;

    private RegisterClientConsumerExecutor<DataTypeParent> consumerExecutor;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        subscribers = new EnumMap<>(DataType.class);

        // Mock subscribers
        ExecutorTypeSubscriber<DataTypeParent> subscriber1 = mock(ExecutorTypeSubscriber.class);
        when(subscriber1.getType()).thenReturn(DataType.META_DATA);
        subscribers.put(DataType.META_DATA, subscriber1);

        ExecutorTypeSubscriber<DataTypeParent> subscriber2 = mock(ExecutorTypeSubscriber.class);
        when(subscriber2.getType()).thenReturn(DataType.API_DOC);
        subscribers.put(DataType.API_DOC, subscriber2);

        consumerExecutor = new RegisterClientConsumerExecutor<>(subscribers);

    }

    @Test
    public void testRunWithValidData() {
        DataTypeParent data = mock(DataTypeParent.class);
        when(data.getType()).thenReturn(DataType.META_DATA);
        consumerExecutor.setData(data);

        consumerExecutor.run();

        verify(subscribers.get(DataType.META_DATA), times(1)).executor(eq(Lists.newArrayList(data)));
        // Verify no calls to other subscribers
        verifyNoInteractions(subscribers.get(DataType.API_DOC));
    }

    @Test
    public void testRunWithUnexpectedDataType() {
        DataTypeParent data = mock(DataTypeParent.class);
        when(data.getType()).thenReturn(DataType.API_DOC);
        consumerExecutor.setData(data);

        consumerExecutor.run();

        // Verify no subscribers are called for the unexpected data type
        verifyNoInteractions(subscribers.values());
    }

    @Nested
    class RegisterClientExecutorFactoryTest {

        @Test
        public void testCreate() {
            Set<ExecutorTypeSubscriber<DataTypeParent>> subscriberSet = new HashSet<>();

            ExecutorTypeSubscriber<DataTypeParent> subscriber1 = mock(ExecutorTypeSubscriber.class);
            when(subscriber1.getType()).thenReturn(DataType.META_DATA);
            subscriberSet.add(subscriber1);

            ExecutorTypeSubscriber<DataTypeParent> subscriber2 = mock(ExecutorTypeSubscriber.class);
            when(subscriber2.getType()).thenReturn(DataType.API_DOC);
            subscriberSet.add(subscriber2);

            RegisterClientConsumerExecutor.RegisterClientExecutorFactory<DataTypeParent> executorFactory =
                    new RegisterClientConsumerExecutor.RegisterClientExecutorFactory<>(subscriberSet);

            RegisterClientConsumerExecutor<DataTypeParent> executor = executorFactory.create();

            assertNotNull(executor);
            assertEquals(subscriberSet.size(), executor.getSubscribers().size());
            // Verify that the executor dispatches data to the correct subscribers
            DataTypeParent data1 = mock(DataTypeParent.class);
            when(data1.getType()).thenReturn(DataType.META_DATA);
            executor.setData(data1);
            executor.run();
            verify(subscriber1, times(1)).executor(anyList());
            verifyNoInteractions(subscriber2);

            DataTypeParent data2 = mock(DataTypeParent.class);
            when(data2.getType()).thenReturn(DataType.API_DOC);
            executor.setData(data2);
            executor.run();
            verify(subscriber2, times(1)).executor(anyList());
            verifyNoInteractions(subscriber1);
        }

        @Test
        public void testFixName() {
            String expectedName = "shenyu_register_client";
            String actualName = new RegisterClientConsumerExecutor.RegisterClientExecutorFactory<>().fixName();
            assertEquals(expectedName, actualName);
        }
    }
}
