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

package org.apache.shenyu.common.timer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * AbstractRetryTask .
 */
public abstract class AbstractRetryTask extends TimerTask {
    
    private final Logger logger = LoggerFactory.getLogger(AbstractRetryTask.class);
    
    private final String key;
    
    private final Integer retryCount;
    
    private int tickCount = 1;
    
    private final boolean retryLimit;
    
    /**
     * Instantiates a new Timer task.
     *
     * @param key     the key
     * @param delayMs the delay ms
     */
    public AbstractRetryTask(final String key,
                             final long delayMs) {
        this(key, delayMs, 1);
    }
    
    /**
     * Instantiates a new Abstract retry task.
     *
     * @param key        the key
     * @param delayMs    the delay ms
     * @param retryCount the retry count
     */
    public AbstractRetryTask(final String key,
                             final long delayMs,
                             final Integer retryCount) {
        this(key, delayMs, retryCount, retryCount < 0);
    }
    
    /**
     * Instantiates a new Abstract retry task.
     * The retryCount parameter has no effect when retryLimit is true.
     *
     * @param key        the key
     * @param delayMs    the delay ms
     * @param retryCount the retry count
     * @param retryLimit the retry limit
     */
    public AbstractRetryTask(final String key,
                             final long delayMs,
                             final Integer retryCount,
                             final boolean retryLimit) {
        super(delayMs);
        this.key = key;
        this.retryCount = retryCount;
        this.retryLimit = retryLimit;
    }
    
    protected void again(final TaskEntity taskEntity) {
        Timer timer = taskEntity.getTimer();
        if (timer == null) {
            return;
        }
        TimerTask timerTask = taskEntity.getTimerTask();
        if (timerTask == null) {
            return;
        }
        if (taskEntity.cancelled()) {
            return;
        }
        timer.add(timerTask);
        tickCount++;
    }
    
    @Override
    public void run(final TaskEntity taskEntity) {
        if (taskEntity == null) {
            return;
        }
        if (taskEntity.cancelled()) {
            return;
        }
        if (!retryLimit && tickCount > retryCount) {
            logger.warn("Final failed to execute task, key:{},retried:{},task over.", key, tickCount);
            return;
        }
        try {
            this.doRetry(key, taskEntity.getTimerTask());
        } catch (Throwable ex) {
            logger.warn("Failed to execute task:{},retried：{} ,total retries:{},cause:{}", key, tickCount, retryCount, ex.getMessage());
            this.again(taskEntity);
        }
    }
    
    /**
     * Do retry.
     *
     * @param key       the key
     * @param timerTask the timer task
     */
    protected abstract void doRetry(String key, TimerTask timerTask);
    
}
