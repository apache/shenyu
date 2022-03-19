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

import net.bytebuddy.agent.ByteBuddyAgent;
import org.apache.shenyu.common.concurrent.MemoryLimitedTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The type shenyu thread pool configuration.
 */
@Configuration
public class ShenyuThreadPoolConfiguration {

    /**
     * crate shenyu shared thread pool executor.
     *
     * @param shenyuConfig the shenyu config
     * @return the shenyu thread pool executor
     */
    @Bean
    @ConditionalOnProperty(name = "shenyu.sharedPool.enable", havingValue = "true")
    public ShenyuThreadPoolExecutor shenyuThreadPoolExecutor(final ShenyuConfig shenyuConfig) {
        final Instrumentation instrumentation = ByteBuddyAgent.install();
        final ShenyuConfig.SharedPool sharedPool = shenyuConfig.getSharedPool();
        final Integer corePoolSize = sharedPool.getCorePoolSize();
        final Integer maximumPoolSize = sharedPool.getMaximumPoolSize();
        final Long keepAliveTime = sharedPool.getKeepAliveTime();
        final Long maxWorkQueueMemory = sharedPool.getMaxWorkQueueMemory();
        return new ShenyuThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime,
                TimeUnit.MILLISECONDS, new MemoryLimitedTaskQueue<>(maxWorkQueueMemory, instrumentation),
                ShenyuThreadFactory.create(sharedPool.getPrefix(), true),
                new ThreadPoolExecutor.AbortPolicy());
    }
}
