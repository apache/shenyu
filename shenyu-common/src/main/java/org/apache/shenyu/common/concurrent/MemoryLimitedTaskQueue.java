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

package org.apache.shenyu.common.concurrent;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * MemoryLimitedTaskQueue in the {@link org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor}.
 * It offer a task if the executor's submittedTaskCount less than currentPoolThreadSize
 * or the currentPoolThreadSize more than executor's maximumPoolSize.
 * That can make the executor create new worker
 * when the task num is bigger than corePoolSize but less than maximumPoolSize.
 */
public class MemoryLimitedTaskQueue<R extends Runnable> extends MemoryLimitedLinkedBlockingQueue<Runnable> {

    private static final long serialVersionUID = -2635853580887179627L;

    private ShenyuThreadPoolExecutor executor;

    public MemoryLimitedTaskQueue(final Instrumentation inst) {
        super(inst);
    }

    public MemoryLimitedTaskQueue(final long memoryLimit, final Instrumentation inst) {
        super(memoryLimit, inst);
    }

    /**
     * set the executor.
     *
     * @param executor executor
     */
    public void setExecutor(final ShenyuThreadPoolExecutor executor) {
        this.executor = executor;
    }

    @Override
    public boolean offer(final Runnable runnable) {
        if (executor == null) {
            throw new RejectedExecutionException("The task queue does not have executor!");
        }

        int currentPoolThreadSize = executor.getPoolSize();
        // have free worker. put task into queue to let the worker deal with task.
        if (executor.getActiveCount() < currentPoolThreadSize) {
            return super.offer(runnable);
        }

        // return false to let executor create new worker.
        if (currentPoolThreadSize < executor.getMaximumPoolSize()) {
            return false;
        }

        // currentPoolThreadSize >= max
        return super.offer(runnable);
    }

    /**
     * retry offer task.
     *
     * @param o       task
     * @param timeout timeout
     * @param unit    timeout unit
     * @return offer success or not
     * @throws java.util.concurrent.RejectedExecutionException if executor is terminated.
     * @throws java.lang.InterruptedException                  if the current thread is interrupted.
     */
    public boolean retryOffer(final Runnable o, final long timeout, final TimeUnit unit) throws InterruptedException {
        if (executor.isShutdown()) {
            throw new RejectedExecutionException("Executor is shutdown!");
        }
        return super.offer(o, timeout, unit);
    }
}
