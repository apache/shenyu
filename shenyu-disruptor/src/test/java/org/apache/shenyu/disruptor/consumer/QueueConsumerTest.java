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

package org.apache.shenyu.disruptor.consumer;

import org.apache.shenyu.disruptor.event.DataEvent;
import org.apache.shenyu.disruptor.event.OrderlyDataEvent;
import org.apache.shenyu.disruptor.thread.OrderlyExecutor;
import org.apache.shenyu.disruptor.thread.SingletonExecutor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class QueueConsumerTest {

    private OrderlyExecutor mockExecutor;

    private QueueConsumerFactory<String> mockFactory;

    private QueueConsumerExecutor<String> mockConsumerExecutor;

    private QueueConsumer<String> queueConsumer;

    @BeforeEach
    void setUp() {

        mockExecutor = mock(OrderlyExecutor.class);
        mockFactory = mock(QueueConsumerFactory.class);
        mockConsumerExecutor = mock(QueueConsumerExecutor.class);
        queueConsumer = new QueueConsumer<>(mockExecutor, mockFactory);
    }

    @Test
    void testOnEventWithValidDataEvent() {
        DataEvent<String> mockEvent = mock(DataEvent.class);
        when(mockEvent.getData()).thenReturn("testData");
        when(mockFactory.create()).thenReturn(mockConsumerExecutor);

        SingletonExecutor mockSingletonExecutor = mock(SingletonExecutor.class);
        when(mockExecutor.select(null)).thenReturn(mockSingletonExecutor);

        queueConsumer.onEvent(mockEvent);

        verify(mockFactory).create();
        verify(mockConsumerExecutor).setData("testData");
        verify(mockEvent).setData(null);
    }

    @Test
    void testOnEventWithNullDataEvent() {
        queueConsumer.onEvent(null);

        verifyNoInteractions(mockFactory);
        verifyNoInteractions(mockExecutor);
    }

    @Test
    void testOnEventWithOrderlyDataEventWithHash() {
        OrderlyDataEvent<String> mockEvent = mock(OrderlyDataEvent.class);
        when(mockEvent.getData()).thenReturn("testData");
        when(mockEvent.getHash()).thenReturn("testHash");
        when(mockFactory.create()).thenReturn(mockConsumerExecutor);

        SingletonExecutor mockSingletonExecutor = mock(SingletonExecutor.class);
        when(mockExecutor.select("testHash")).thenReturn(mockSingletonExecutor);

        queueConsumer.onEvent(mockEvent);

        verify(mockFactory).create();
        verify(mockConsumerExecutor).setData("testData");
        verify(mockSingletonExecutor).execute(mockConsumerExecutor);
        verify(mockEvent).setData(null);
    }

    @Test
    void testOnEventWithOrderlyDataEventWithoutHash() {
        // Arrange
        OrderlyDataEvent<String> mockEvent = mock(OrderlyDataEvent.class);
        when(mockEvent.getData()).thenReturn("testData");
        when(mockEvent.getHash()).thenReturn("");
        when(mockFactory.create()).thenReturn(mockConsumerExecutor);

        SingletonExecutor mockSingletonExecutor = mock(SingletonExecutor.class);
        when(mockExecutor.select(null)).thenReturn(mockSingletonExecutor);

        // Act
        queueConsumer.onEvent(mockEvent);

        // Assert
        verify(mockFactory).create();
        verify(mockConsumerExecutor).setData("testData");
        verify(mockEvent).setData(null);
    }
}
