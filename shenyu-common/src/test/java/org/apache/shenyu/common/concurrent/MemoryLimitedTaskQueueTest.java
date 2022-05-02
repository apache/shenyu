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

import net.bytebuddy.agent.ByteBuddyAgent;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.lang.instrument.Instrumentation;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for MemoryLimitedTaskQueue.
 */
public class MemoryLimitedTaskQueueTest {

    private static Instrumentation instrumentation;

    @BeforeAll
    public static void initializeInstrumentation() {
        ByteBuddyAgent.install();
        instrumentation = ByteBuddyAgent.getInstrumentation();
    }

    @Test
    public void testOffer() {
        MemoryLimitedTaskQueue memoryLimitedTaskQueue = new MemoryLimitedTaskQueue<>(instrumentation);
        assertTrue(memoryLimitedTaskQueue.doOffer(() -> { }));
    }

    @Test
    public void testOfferWhenMemoryNotSufficient() {
        MemoryLimitedTaskQueue memoryLimitedTaskQueue = new MemoryLimitedTaskQueue<>(1, instrumentation);
        assertFalse(memoryLimitedTaskQueue.doOffer(() -> { }));
    }
}
