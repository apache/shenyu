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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Iterator;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TimerTaskListTest {

    private TimerTaskList list;

    private AtomicInteger counter;

    @BeforeEach
    void setUp() {

        counter = new AtomicInteger(0);
        list = new TimerTaskList(counter);
    }

    @Test
    void testAddAndRemove() {

        TestTimerTask task = new TestTimerTask(100L);

        TimerTaskList.TimerTaskEntry entry = new TimerTaskList.TimerTaskEntry(null, task, 100L);

        list.add(entry);
        assertEquals(1, counter.get());

        list.remove(entry);
        assertEquals(0, counter.get());
    }

    @Test
    void testSetAndGetExpiration() {

        assertTrue(list.setExpiration(123L));
        assertEquals(123L, list.getExpiration());
        assertFalse(list.setExpiration(123L));
    }

    @Test
    void testFlush() {

        TestTimerTask task = new TestTimerTask(100L);

        TimerTaskList.TimerTaskEntry entry = new TimerTaskList.TimerTaskEntry(null, task, 100L);

        list.add(entry);
        list.flush(e -> assertNotNull(e));

        assertEquals(-1L, list.getExpiration());
        assertEquals(0, counter.get());
    }

    @Test
    void testIterator() {

        TestTimerTask task = new TestTimerTask(100L);
        TimerTaskList.TimerTaskEntry entry = new TimerTaskList.TimerTaskEntry(null, task, 100L);

        list.add(entry);
        Iterator<TimerTask> it = list.iterator();

        assertTrue(it.hasNext());
        assertEquals(task, it.next());
    }

    @Test
    void testGetDelay() {

        list.setExpiration(System.currentTimeMillis() + 1000);

        long delay = list.getDelay(TimeUnit.MILLISECONDS);

        assertTrue(delay >= 0);
    }

    static final class TestTimerTask extends TimerTask {

        private TestTimerTask(final long delayMs) {
            super(delayMs);
        }

        @Override
        public void run(final TaskEntity taskEntity) {
            boolean executed = true;
        }
    }

}
