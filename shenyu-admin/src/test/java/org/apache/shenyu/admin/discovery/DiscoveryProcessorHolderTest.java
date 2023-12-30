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

package org.apache.shenyu.admin.discovery;

import org.apache.commons.lang3.NotImplementedException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class DiscoveryProcessorHolderTest {

    @InjectMocks
    private DiscoveryProcessorHolder processorHolder;

    @Mock
    private DiscoveryProcessor defaultProcessor;

    @Mock
    private DiscoveryProcessor localProcessor;

    @Mock
    private DiscoveryProcessor eurekaProcessor;

    @BeforeEach
    void setUp() {
        processorHolder = new DiscoveryProcessorHolder(defaultProcessor, localProcessor, eurekaProcessor);
    }

    @Test
    void chooseProcessor() {
        // 测试选择本地模式的情况
        assertEquals(localProcessor, processorHolder.chooseProcessor(DiscoveryMode.LOCAL.name()));

        // 测试选择其他模式的情况
        assertEquals(defaultProcessor, processorHolder.chooseProcessor(DiscoveryMode.ZOOKEEPER.name()));
        assertEquals(defaultProcessor, processorHolder.chooseProcessor(DiscoveryMode.ETCD.name()));
        assertEquals(defaultProcessor, processorHolder.chooseProcessor(DiscoveryMode.NACOS.name()));
        assertEquals(defaultProcessor, processorHolder.chooseProcessor(DiscoveryMode.EUREKA.name()));

        // 测试不支持的模式
        assertThrows(NotImplementedException.class, () -> processorHolder.chooseProcessor("UNKNOWN_MODE"));
    }
}
