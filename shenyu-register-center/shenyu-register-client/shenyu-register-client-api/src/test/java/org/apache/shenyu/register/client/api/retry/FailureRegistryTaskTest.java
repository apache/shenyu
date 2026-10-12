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

import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.TaskEntity;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.register.client.api.FailbackRegistryRepository;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public final class FailureRegistryTaskTest {

    @Test
    public void delegatesToAtomicRetry() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        FailureRegistryTask task = new FailureRegistryTask("key", repository);
        task.doRetry("key", mock(TimerTask.class));
        verify(repository).retry("key");
        verifyNoMoreInteractions(repository);
    }

    @Test
    public void propagatesFailureForRescheduling() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        FailureRegistryTask task = new FailureRegistryTask("key", repository);
        doThrow(new IllegalStateException("offline")).when(repository).retry("key");
        assertThrows(IllegalStateException.class, () -> task.doRetry("key", mock(TimerTask.class)));
    }

    @Test
    public void testRetryExhaustedRemovesFailure() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        FailureRegistryTask task = new FailureRegistryTask("key", repository);
        task.onRetryExhausted("key");
        verify(repository).remove("key");
    }

    @Test
    public void repeatedAttemptsKeepDelegatingToTheSameKey() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        FailureRegistryTask task = new FailureRegistryTask("key", repository);
        TimerTask timerTask = mock(TimerTask.class);
        for (int attempt = 0; attempt < 3; attempt++) {
            task.doRetry("key", timerTask);
        }
        verify(repository, times(3)).retry("key");
        verifyNoMoreInteractions(repository);
    }

    @Test
    public void independentTasksUseTheirOwnRegistrationKeys() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        FailureRegistryTask firstTask = new FailureRegistryTask("first", repository);
        FailureRegistryTask secondTask = new FailureRegistryTask("second", repository);
        firstTask.doRetry("first", mock(TimerTask.class));
        secondTask.doRetry("second", mock(TimerTask.class));
        verify(repository).retry("first");
        verify(repository).retry("second");
        verifyNoMoreInteractions(repository);
    }

    @Test
    public void removesFailureAfterRetriesAreExhausted() {
        final FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        final TimerTask timerTask = mock(TimerTask.class);
        final Timer timer = mock(Timer.class);
        TaskEntity entity = mock(TaskEntity.class);
        when(entity.getTimer()).thenReturn(timer);
        when(entity.getTimerTask()).thenReturn(timerTask);
        FailureRegistryTask task = new FailureRegistryTask("key", repository);
        doThrow(new IllegalStateException("registration failed")).when(repository).retry("key");
        for (int attempt = 0; attempt < 19; attempt++) {
            task.run(entity);
        }
        verify(repository, times(18)).retry("key");
        verify(repository).remove("key");
        verify(timer, times(18)).add(timerTask);
    }

    @Test
    public void ownedTaskUsesConditionalRetryAndCleanup() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        FailureRegistryTask task = FailureRegistryTask.createOwned("key", repository);

        task.doRetry("key", mock(TimerTask.class));
        task.onRetryExhausted("key");

        verify(repository).retry("key", task);
        verify(repository).remove("key", task);
        verifyNoMoreInteractions(repository);
    }
}
