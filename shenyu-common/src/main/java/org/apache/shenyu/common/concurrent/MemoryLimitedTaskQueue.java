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

/**
 * MemoryLimitedTaskQueue in the {@link org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor}.
 * It offer a task if the executor's submittedTaskCount less than currentPoolThreadSize
 * or the currentPoolThreadSize more than executor's maximumPoolSize.
 * That can make the executor create new worker
 * when the task num is bigger than corePoolSize but less than maximumPoolSize.
 */
public class MemoryLimitedTaskQueue<R extends Runnable> extends MemoryLimitedLinkedBlockingQueue<Runnable> implements TaskQueue<Runnable> {

    private static final long serialVersionUID = -2635853580887179627L;

    private EagerExecutorService executor;

    public MemoryLimitedTaskQueue(final Instrumentation inst) {
        super(inst);
    }

    public MemoryLimitedTaskQueue(final long memoryLimit, final Instrumentation inst) {
        super(memoryLimit, inst);
    }

    @Override
    public EagerExecutorService getExecutor() {
        return executor;
    }

    @Override
    public void setExecutor(final EagerExecutorService executor) {
        this.executor = executor;
    }

    @Override
    public boolean doOffer(final Runnable runnable) {
        return super.offer(runnable);
    }
}
