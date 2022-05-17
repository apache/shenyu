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

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * HierarchicalWheelTimerTest .
 */
public class HierarchicalWheelTimerTest {
    
    /**
     * The Timer.
     */
    private Timer timer;
    
    /**
     * The Timer task list.
     */
    private TimerTaskList timerTaskList;
    
    /**
     * The Task count.
     */
    private final AtomicInteger taskCount = new AtomicInteger(0);
    
    /**
     * Sets up.
     */
    @BeforeEach
    public void setUp() {
        timer = WheelTimerFactory.newWheelTimer();
        timerTaskList = new TimerTaskList(taskCount);
    }
    
    /**
     * Test timer.
     */
    @Test
    public void testTimer() {
        for (int i = 0; i < 100; i++) {
            timer.add(new TimerTask(1 + i, TimeUnit.SECONDS) {
                @Override
                public void run(final TaskEntity taskEntity) {
                
                }
            });
        }
        assertEquals(timer.size(), 100);
    }
    
    /**
     * Test timer cancel.
     */
    @Test
    public void testTimerCancel() {
        TimerTask timerTask = new TimerTask(100000) {
            @Override
            public void run(final TaskEntity taskEntity) {
            }
        };
        timer.add(timerTask);
        assertEquals(timer.size(), 1);
        timerTask.cancel();
        assertEquals(timer.size(), 0);
    }
    
    /**
     * Test list foreach.
     */
    @Test
    public void testListForeach() {
        TimerTask timerTask = new TimerTask(100000) {
            @Override
            public void run(final TaskEntity taskEntity) {
            
            }
        };
        timerTaskList.add(new TimerTaskList.TimerTaskEntry(timer, timerTask, -1L));
        assertEquals(taskCount.get(), 1);
        timerTaskList.foreach(timerTask1 -> assertSame(timerTask1, timerTask1));
    }
    
    /**
     * Test list iterator.
     */
    @Test
    public void testListIterator() {
        TimerTask timerTask = new TimerTask(100000) {
            @Override
            public void run(final TaskEntity taskEntity) {
            }
        };
        timerTaskList.add(new TimerTaskList.TimerTaskEntry(timer, timerTask, -1L));
        assertEquals(taskCount.get(), 1);
        for (final TimerTask task : timerTaskList) {
            assertSame(timerTask, timerTask);
        }
    }
}
