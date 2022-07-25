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
import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.concurrent.TaskQueue;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextClosedEvent;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The type shenyu thread pool configuration.
 */
@Configuration
public class ShenyuThreadPoolConfiguration {

    /**
     * MemoryLimitedTaskQueue.
     *
     * @param shenyuConfig the shenyu config
     * @return instance of {@link MemoryLimitedTaskQueue}
     */
    @Bean
    @ConditionalOnMissingBean(TaskQueue.class)
    @ConditionalOnProperty("shenyu.shared-pool.max-work-queue-memory")
    public TaskQueue<Runnable> memoryLimitedTaskQueue(final ShenyuConfig shenyuConfig) {
        final Instrumentation instrumentation = ByteBuddyAgent.install();
        final ShenyuConfig.SharedPool sharedPool = shenyuConfig.getSharedPool();
        final Long maxWorkQueueMemory = sharedPool.getMaxWorkQueueMemory();
        if (maxWorkQueueMemory <= 0) {
            throw new ShenyuException("${shenyu.sharedPool.maxWorkQueueMemory} must bigger than 0 !");
        }
        return new MemoryLimitedTaskQueue<>(maxWorkQueueMemory, instrumentation);
    }

    /**
     * MemorySafeTaskQueue.
     *
     * @param shenyuConfig the shenyu config
     * @return instance of {@link MemorySafeTaskQueue}
     */
    @Bean
    @ConditionalOnMissingBean(TaskQueue.class)
    @ConditionalOnProperty("shenyu.shared-pool.max-free-memory")
    public TaskQueue<Runnable> memorySafeTaskQueue(final ShenyuConfig shenyuConfig) {
        final ShenyuConfig.SharedPool sharedPool = shenyuConfig.getSharedPool();
        final Integer maxFreeMemory = sharedPool.getMaxFreeMemory();
        if (maxFreeMemory <= 0) {
            throw new ShenyuException("${shenyu.sharedPool.maxFreeMemory} must bigger than 0 !");
        }
        return new MemorySafeTaskQueue<>(maxFreeMemory);
    }

    /**
     * create shenyu shared thread pool executor.
     *
     * @param shenyuConfig the shenyu config
     * @param provider     the queue bean provider
     * @return the shenyu thread pool executor
     */
    @Bean
    @ConditionalOnProperty(name = "shenyu.shared-pool.enable", havingValue = "true", matchIfMissing = true)
    public ShenyuThreadPoolExecutor shenyuThreadPoolExecutor(final ShenyuConfig shenyuConfig,
                                                             final ObjectProvider<TaskQueue<Runnable>> provider) {
        final ShenyuConfig.SharedPool sharedPool = shenyuConfig.getSharedPool();
        final Integer corePoolSize = sharedPool.getCorePoolSize();
        final Integer maximumPoolSize = sharedPool.getMaximumPoolSize();
        final Long keepAliveTime = sharedPool.getKeepAliveTime();
        return new ShenyuThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime,
                TimeUnit.MILLISECONDS, provider.getIfAvailable(() -> new MemorySafeTaskQueue<>(Constants.THE_256_MB)),
                ShenyuThreadFactory.create(sharedPool.getPrefix(), true),
                new ThreadPoolExecutor.AbortPolicy());
    }

    /**
     * destroy the shenyu shared thread pool executor.
     *
     * @return the shenyu thread pool executor destructor
     */
    @Bean
    @ConditionalOnBean(ShenyuThreadPoolExecutor.class)
    public ShenyuThreadPoolExecutorDestructor shenyuThreadPoolExecutorDestructor() {
        return new ShenyuThreadPoolExecutorDestructor();
    }

    /**
     * The type shenyu thread pool executor destructor.
     */
    public static class ShenyuThreadPoolExecutorDestructor implements ApplicationListener<ContextClosedEvent> {

        @Override
        public void onApplicationEvent(final ContextClosedEvent event) {
            SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class).shutdown();
        }
    }
}
