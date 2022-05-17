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
 * AbstractRoundTask .
 * A timer that runs periodically, repeatedly.
 */
public abstract class AbstractRoundTask extends AbstractRetryTask {
    
    private final Logger logger = LoggerFactory.getLogger(AbstractRoundTask.class);
    
    /**
     * Instantiates a new Timer task.
     *
     * @param key     the key
     * @param delayMs the delay ms
     */
    public AbstractRoundTask(final String key, final long delayMs) {
        super(key, delayMs, -1);
    }
    
    @Override
    public void run(final TaskEntity taskEntity) {
        try {
            super.run(taskEntity);
        } finally {
            this.again(taskEntity);
        }
    }
    
    /**
     * Do retry.
     *
     * @param key       the key
     * @param timerTask the timer task
     */
    @Override
    protected void doRetry(final String key, final TimerTask timerTask) {
        try {
            this.doRun(key, timerTask);
        } catch (Throwable ex) {
            logger.warn("Failed to execute,but can be ignored");
        }
    }
    
    /**
     * Do timer.
     *
     * @param key       the key
     * @param timerTask the timer task
     */
    public abstract void doRun(String key, TimerTask timerTask);
}
