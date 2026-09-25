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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link AbstractRetryTask}.
 */
public final class AbstractRetryTaskTest {

    @Test
    public void testRetryExhaustedCallback() {
        TaskEntity taskEntity = mock(TaskEntity.class);
        Timer timer = mock(Timer.class);
        TestRetryTask retryTask = new TestRetryTask();
        when(taskEntity.getTimer()).thenReturn(timer);
        when(taskEntity.getTimerTask()).thenReturn(retryTask);

        retryTask.run(taskEntity);
        retryTask.run(taskEntity);

        assertTrue(retryTask.retryExhausted);
        verify(timer).add(retryTask);
    }

    private static final class TestRetryTask extends AbstractRetryTask {

        private boolean retryExhausted;

        private TestRetryTask() {
            super("test", 0, 1);
        }

        @Override
        protected void doRetry(final String key, final TimerTask timerTask) {
            throw new IllegalStateException("retry");
        }

        @Override
        protected void onRetryExhausted(final String key) {
            retryExhausted = true;
        }
    }
}
