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

package org.apache.shenyu.disruptor.thread;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Apply backpressure to the Disruptor consumer without running ordered tasks out of order.
 */
public final class BlockWhenFullPolicy implements RejectedExecutionHandler {

    @Override
    public void rejectedExecution(final Runnable task, final ThreadPoolExecutor executor) {
        try {
            while (!executor.isShutdown()) {
                if (executor.getQueue().offer(task, 100, TimeUnit.MILLISECONDS)) {
                    if (executor.isShutdown() && executor.remove(task)) {
                        throw new RejectedExecutionException("Executor shut down during handoff");
                    }
                    return;
                }
            }
            throw new RejectedExecutionException("Executor is shut down");
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new RejectedExecutionException("Interrupted while waiting for consumer capacity", exception);
        }
    }
}
