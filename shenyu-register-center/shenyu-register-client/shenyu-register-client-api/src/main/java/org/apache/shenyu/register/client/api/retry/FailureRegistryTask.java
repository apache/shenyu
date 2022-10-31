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

package org.apache.shenyu.register.client.api.retry;

import org.apache.shenyu.common.timer.AbstractRetryTask;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.register.client.api.FailbackRegistryRepository;

import java.util.concurrent.TimeUnit;

/**
 * FailureRegistryTask .
 * When the registration url in Shenyu Client Register Repository fails.
 * It needs to be re-registered here.
 */
public class FailureRegistryTask extends AbstractRetryTask {
    
    private final FailbackRegistryRepository registerRepository;
    
    /**
     * Instantiates a new Timer task.
     *
     * @param key                the key
     * @param registerRepository the register repository
     */
    public FailureRegistryTask(final String key, final FailbackRegistryRepository registerRepository) {
        //Indicates 10s to retry.
        super(key, TimeUnit.SECONDS.toMillis(10), 18);
        this.registerRepository = registerRepository;
    }
    
    /**
     * Do retry.
     *
     * @param key       the key
     * @param timerTask the timer task
     */
    @Override
    protected void doRetry(final String key, final TimerTask timerTask) {
        this.registerRepository.accept(key);
        //Because accept requires an exception to be thrown. Only normal can remove.
        this.registerRepository.remove(key);
    }
}
