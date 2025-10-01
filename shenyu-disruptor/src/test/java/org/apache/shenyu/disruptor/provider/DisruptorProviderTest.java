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

package org.apache.shenyu.disruptor.provider;

import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import org.apache.shenyu.disruptor.event.DataEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

class DisruptorProviderTest {

    private RingBuffer<DataEvent<String>> mockRingBuffer;

    private Disruptor<DataEvent<String>> mockDisruptor;

    private DisruptorProvider<String> disruptorProvider;

    @BeforeEach
    void setUp() {

        mockRingBuffer = mock(RingBuffer.class);
        mockDisruptor = mock(Disruptor.class);
        disruptorProvider = new DisruptorProvider<>(mockRingBuffer, mockDisruptor, false);
    }

    @Test
    void testOnData() {

        String testData = "testData";

        disruptorProvider.onData(testData);

        verify(mockRingBuffer).publishEvent(any(), eq(testData));
    }

    @Test
    void testOnDataThrowsExceptionForOrderlyProvider() {

        disruptorProvider = new DisruptorProvider<>(mockRingBuffer, mockDisruptor, true);

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> disruptorProvider.onData("testData"));
        assertEquals("The current provider is  of orderly type. Please use onOrderlyData() method.", exception.getMessage());
    }

    @Test
    void testOnOrderlyData() {

        disruptorProvider = new DisruptorProvider<>(mockRingBuffer, mockDisruptor, true);
        String testData = "testData";
        String[] hashArray = {"hash1", "hash2"};

        disruptorProvider.onOrderlyData(testData, hashArray);

        verify(mockRingBuffer).publishEvent(any(), eq(testData), eq("hash1:hash2"));
    }

    @Test
    void testOnOrderlyDataThrowsExceptionForNonOrderlyProvider() {

        String testData = "testData";
        String[] hashArray = {"hash1", "hash2"};

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> disruptorProvider.onOrderlyData(testData, hashArray));
        assertEquals("The current provider is not of orderly type. Please use onData() method.", exception.getMessage());
    }

    @Test
    void testShutdown() {

        disruptorProvider.shutdown();

        verify(mockDisruptor).shutdown();
    }

    @Test
    void testShutdownWithNullDisruptor() {

        disruptorProvider = new DisruptorProvider<>(mockRingBuffer, null, false);

        disruptorProvider.shutdown();

        verifyNoInteractions(mockDisruptor);
    }

}
