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

package org.apache.shenyu.web.configuration;

import org.apache.shenyu.common.concurrent.MemoryLimitedTaskQueue;
import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.web.controller.TestObjectProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextClosedEvent;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ShenyuThreadPoolConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class ShenyuThreadPoolConfigurationTest {

    private ShenyuThreadPoolConfiguration shenyuThreadPoolConfiguration;

    @BeforeEach
    public void before() {
        shenyuThreadPoolConfiguration = new ShenyuThreadPoolConfiguration();
    }

    @Test
    public void testMemoryLimitedTaskQueue() {
        ShenyuConfig shenyuConfig = mock(ShenyuConfig.class);
        ShenyuConfig.SharedPool sharedPool = mock(ShenyuConfig.SharedPool.class);
        when(shenyuConfig.getSharedPool()).thenReturn(sharedPool);
        when(sharedPool.getMaxWorkQueueMemory()).thenReturn(1024L);
        assertTrue(shenyuThreadPoolConfiguration.memoryLimitedTaskQueue(shenyuConfig) instanceof MemoryLimitedTaskQueue);
        when(sharedPool.getMaxWorkQueueMemory()).thenReturn(0L);
        assertThrows(ShenyuException.class, () -> shenyuThreadPoolConfiguration.memoryLimitedTaskQueue(shenyuConfig));

    }

    @Test
    public void testMemorySafeTaskQueue() {
        ShenyuConfig shenyuConfig = mock(ShenyuConfig.class);
        ShenyuConfig.SharedPool sharedPool = mock(ShenyuConfig.SharedPool.class);
        when(shenyuConfig.getSharedPool()).thenReturn(sharedPool);
        when(sharedPool.getMaxFreeMemory()).thenReturn(1024);
        assertTrue(shenyuThreadPoolConfiguration.memorySafeTaskQueue(shenyuConfig) instanceof MemorySafeTaskQueue);
        when(sharedPool.getMaxFreeMemory()).thenReturn(0);
        assertThrows(ShenyuException.class, () -> shenyuThreadPoolConfiguration.memorySafeTaskQueue(shenyuConfig));

    }

    @Test
    public void testShenyuThreadPoolExecutor() {
        ShenyuConfig shenyuConfig = mock(ShenyuConfig.class);
        ObjectProvider objectProvider = mock(ObjectProvider.class);
        ShenyuConfig.SharedPool sharedPool = mock(ShenyuConfig.SharedPool.class);
        when(shenyuConfig.getSharedPool()).thenReturn(sharedPool);
        when(sharedPool.getCorePoolSize()).thenReturn(1);
        when(sharedPool.getMaximumPoolSize()).thenReturn(1);
        when(sharedPool.getKeepAliveTime()).thenReturn(60L);
        when(sharedPool.getPrefix()).thenReturn("TEST-");
        when(objectProvider.getIfAvailable()).thenReturn(null);
        assertNotNull(shenyuThreadPoolConfiguration.shenyuThreadPoolExecutor(shenyuConfig, new TestObjectProvider<>(null)));
        assertNotNull(shenyuThreadPoolConfiguration.shenyuThreadPoolExecutor(shenyuConfig, new TestObjectProvider<>(new MemorySafeTaskQueue<>(Constants.THE_256_MB))));
    }

    @Test
    public void testShenyuThreadPoolExecutorDestructor() {
        ShenyuThreadPoolConfiguration.ShenyuThreadPoolExecutorDestructor shenyuThreadPoolExecutorDestructor =
                shenyuThreadPoolConfiguration.shenyuThreadPoolExecutorDestructor();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class))
                .thenReturn(mock(ShenyuThreadPoolExecutor.class));
        shenyuThreadPoolExecutorDestructor.onApplicationEvent(mock(ContextClosedEvent.class));
        assertNotNull(shenyuThreadPoolExecutorDestructor);
    }

}
