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

package org.apache.shenyu.plugin.alibaba.dubbo.threadpool;

import com.alibaba.dubbo.common.URL;
import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


/**
 * The Test Case For SharedThreadPool.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SharedThreadPoolTest {

    @InjectMocks
    private SharedThreadPool sharedThreadPool;

    @Test
    public void testGetExecutor() {
        ShenyuThreadPoolExecutor shenyuThreadPoolExecutor = new ShenyuThreadPoolExecutor(1,
                2, 30, TimeUnit.SECONDS, new MemorySafeTaskQueue<>(100),
                Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy());
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class)).thenReturn(shenyuThreadPoolExecutor);
        assertEquals(sharedThreadPool.getExecutor(URL.valueOf("localhost")), shenyuThreadPoolExecutor);

        when(SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class)).thenThrow(new NoSuchBeanDefinitionException("not bean"));
        assertThrows(ShenyuException.class, () -> sharedThreadPool.getExecutor(URL.valueOf("localhost")));
    }

}
