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

package org.apache.shenyu.admin.disruptor.subscriber;

import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link DiscoveryConfigRegisterExecutorSubscriber}.
 */
@ExtendWith(MockitoExtension.class)
class DiscoveryConfigRegisterExecutorSubscriberTest {

    @Mock
    private DiscoveryService discoveryService;

    private DiscoveryConfigRegisterExecutorSubscriber subscriber;

    @BeforeEach
    void setUp() {
        subscriber = new DiscoveryConfigRegisterExecutorSubscriber(discoveryService);
    }

    @Test
    void testGetType() {
        assertEquals(DataType.DISCOVERY_CONFIG, subscriber.getType());
    }

    @Test
    void testExecutorWithEmptyList() {
        List<DiscoveryConfigRegisterDTO> list = new ArrayList<>();
        subscriber.executor(list);

        await().atMost(Duration.ofSeconds(5))
                .untilAsserted(() -> verify(discoveryService, times(0)).registerDiscoveryConfig(any()));
    }

    @Test
    void testExecutorWithSingleData() {
        List<DiscoveryConfigRegisterDTO> list = new ArrayList<>();
        DiscoveryConfigRegisterDTO dto = new DiscoveryConfigRegisterDTO();
        dto.setName("test-discovery");
        dto.setDiscoveryType("eureka");
        list.add(dto);

        subscriber.executor(list);

        await().atMost(Duration.ofSeconds(5))
                .untilAsserted(() -> verify(discoveryService, times(1)).registerDiscoveryConfig(any(DiscoveryConfigRegisterDTO.class)));
    }

    @Test
    void testExecutorWithMultipleData() {
        List<DiscoveryConfigRegisterDTO> list = new ArrayList<>();
        DiscoveryConfigRegisterDTO dto1 = new DiscoveryConfigRegisterDTO();
        dto1.setName("test-discovery-1");
        dto1.setDiscoveryType("eureka");
        list.add(dto1);

        DiscoveryConfigRegisterDTO dto2 = new DiscoveryConfigRegisterDTO();
        dto2.setName("test-discovery-2");
        dto2.setDiscoveryType("nacos");
        list.add(dto2);

        subscriber.executor(list);

        await().atMost(Duration.ofSeconds(5))
                .untilAsserted(() -> verify(discoveryService, times(2)).registerDiscoveryConfig(any(DiscoveryConfigRegisterDTO.class)));
    }

    @Test
    void testExecutorWithException() {
        List<DiscoveryConfigRegisterDTO> list = new ArrayList<>();
        DiscoveryConfigRegisterDTO dto = new DiscoveryConfigRegisterDTO();
        dto.setName("test-discovery");
        dto.setDiscoveryType("eureka");
        list.add(dto);

        doThrow(new RuntimeException("Test exception")).when(discoveryService).registerDiscoveryConfig(any());

        subscriber.executor(list);

        await().atMost(Duration.ofSeconds(5))
                .untilAsserted(() -> verify(discoveryService, times(1)).registerDiscoveryConfig(any(DiscoveryConfigRegisterDTO.class)));
    }
}
