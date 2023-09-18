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

package org.apache.shenyu.plugin.base.alert;

import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;

import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * ShenyuThreadPoolExecutor for alarm sender send async data.
 */
public final class AlarmThreadPoolExecutor {
    
    /**
     * The thread pool executor.
     */
    private static ShenyuThreadPoolExecutor threadPoolExecutor;
    
    /**
     * Private constructor.
     */
    private AlarmThreadPoolExecutor() {
        initWorkExecutor();
    }
    
    /**
     * Init work executor.
     */
    private void initWorkExecutor() {
        threadPoolExecutor = new ShenyuThreadPoolExecutor(3, 3, 10L,
                TimeUnit.SECONDS, new MemorySafeTaskQueue<>(Constants.THE_256_MB),
                ShenyuThreadFactory.create("alarm-sender", true),
                new ThreadPoolExecutor.CallerRunsPolicy());
    }
    
    /**
     * Execute alarm runnable task.
     * @param runnable task
     */
    public void execute(final Runnable runnable) {
        threadPoolExecutor.execute(runnable);
    }
    
    /**
     * Get AlarmThreadPoolExecutor single instance.
     * @return AlarmThreadPoolExecutor instance
     */
    public static AlarmThreadPoolExecutor getInstance() {
        return SingleInstance.INSTANCE;
    }
    
    /**
     * Single instance for AlarmThreadPoolExecutor.
     */
    private static class SingleInstance {
        private static final AlarmThreadPoolExecutor INSTANCE = new AlarmThreadPoolExecutor();
    }
}
