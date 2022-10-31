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
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for ShenyuThreadPoolExecutor.
 */
public class ShenyuThreadPoolExecutorTest {

    private static Instrumentation instrumentation;

    @BeforeAll
    public static void initialExecutor() {
        ByteBuddyAgent.install();
        instrumentation = ByteBuddyAgent.getInstrumentation();
    }

    private ShenyuThreadPoolExecutor getTestExecutor(final TaskQueue<Runnable> taskQueue) {
        return new ShenyuThreadPoolExecutor(5, 10, 100, TimeUnit.SECONDS, taskQueue, ShenyuThreadFactory.create("Test", false), (r, e) -> {
        });
    }

    @Test
    public void testNullCommand() {
        ShenyuThreadPoolExecutor executor = getTestExecutor(new MemoryLimitedTaskQueue<>(instrumentation));
        assertThrows(NullPointerException.class, () -> executor.execute(null));
    }

}
