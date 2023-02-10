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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test Cases for ThreadUtils.
 */
public final class ThreadUtilsTest {

    private TimeUnit timeUnit;

    @BeforeEach
    public void setUp() {
        timeUnit = mock(TimeUnit.class);
    }

    @Test
    public void testSleep() throws InterruptedException {
        ThreadUtils.sleep(timeUnit, 1);
        verify(timeUnit, times(1)).sleep(eq(1L));
    }

    @Test
    public void testSleepInterrupt() throws InterruptedException {
        doThrow(InterruptedException.class).when(timeUnit).sleep(1);
        ThreadUtils.sleep(timeUnit, 1);
        verify(timeUnit, times(1)).sleep(eq(1L));
        assertTrue(Thread.currentThread().interrupted());
    }
}
