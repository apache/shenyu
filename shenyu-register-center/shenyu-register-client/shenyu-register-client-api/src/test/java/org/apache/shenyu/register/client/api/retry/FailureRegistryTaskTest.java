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
import org.apache.shenyu.register.client.api.FailbackRegistryRepository;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

class FailureRegistryTaskTest {

    @Test
    void delegatesToAtomicRetry() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        new FailureRegistryTask("key", repository).doRetry("key", mock(TimerTask.class));
        verify(repository).retry("key");
        verifyNoMoreInteractions(repository);
    }

    @Test
    void propagatesFailureForRescheduling() {
        FailbackRegistryRepository repository = mock(FailbackRegistryRepository.class);
        doThrow(new IllegalStateException("offline")).when(repository).retry("key");
        FailureRegistryTask task = new FailureRegistryTask("key", repository);
        assertThrows(IllegalStateException.class, () -> task.doRetry("key", mock(TimerTask.class)));
    }
}
