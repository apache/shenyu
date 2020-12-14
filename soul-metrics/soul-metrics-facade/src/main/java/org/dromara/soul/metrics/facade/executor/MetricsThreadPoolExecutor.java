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

package org.dromara.soul.metrics.facade.executor;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Metrics thread pool executor.
 */
@Slf4j
public final class MetricsThreadPoolExecutor extends ThreadPoolExecutor {
    
    /**
     * Instantiates a new Metrics thread pool executor.
     *
     * @param threadCount core and max thread count
     * @param queueSize   queue size
     */
    public MetricsThreadPoolExecutor(final int threadCount, final int queueSize) {
        super(threadCount, threadCount, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>(queueSize),
                SoulThreadFactory.create("metrics", true), new CallerWaitPolicy());
    }
    
    private static class CallerWaitPolicy implements RejectedExecutionHandler {
    
        @Override
        public void rejectedExecution(final Runnable r, final ThreadPoolExecutor executor) {
            try {
                log.warn("queue is full, trigger caller thread : {} wait", Thread.currentThread().getName());
                executor.getQueue().put(r);
            } catch (InterruptedException ex) {
                log.error("InterruptedException, discard {}", r);
            }
        }
    }
}

