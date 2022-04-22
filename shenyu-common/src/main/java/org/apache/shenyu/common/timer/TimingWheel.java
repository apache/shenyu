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

import java.util.concurrent.DelayQueue;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * TimingWheel .
 * This is a Hierarchical wheel timer implementation.
 */
class TimingWheel {
    
    private final Long tickMs;
    
    private final Integer wheelSize;
    
    private final AtomicInteger taskCounter;
    
    private final DelayQueue<TimerTaskList> queue;
    
    private final Long interval;
    
    private final TimerTaskList[] buckets;
    
    private Long currentTime;
    
    private TimingWheel overflowWheel;
    
    /**
     * Instantiates a new Timing wheel.
     *
     * @param tickMs      the tick ms
     * @param wheelSize   the wheel size
     * @param startMs     the start ms
     * @param taskCounter the task counter
     * @param queue       the queue
     */
    TimingWheel(final Long tickMs, final Integer wheelSize, final Long startMs, final AtomicInteger taskCounter, final DelayQueue<TimerTaskList> queue) {
        this.tickMs = tickMs;
        this.wheelSize = wheelSize;
        this.taskCounter = taskCounter;
        this.queue = queue;
        this.interval = tickMs * wheelSize;
        this.currentTime = startMs - (startMs % tickMs);
        this.buckets = new TimerTaskList[wheelSize];
    }
    
    private synchronized void addOverflowWheel() {
        if (overflowWheel == null) {
            overflowWheel = new TimingWheel(interval, wheelSize, currentTime, taskCounter, queue);
        }
    }
    
    /**
     * Add boolean.
     *
     * @param taskEntry the task entry
     * @return the boolean
     */
    boolean add(final TimerTaskList.TimerTaskEntry taskEntry) {
        Long expirationMs = taskEntry.getExpirationMs();
        if (taskEntry.cancelled()) {
            return false;
        }
        if (expirationMs < currentTime + tickMs) {
            return false;
        }
        if (expirationMs < currentTime + interval) {
            //Put in its own bucket
            long virtualId = expirationMs / tickMs;
            int index = (int) (virtualId % wheelSize);
            TimerTaskList bucket = this.getBucket(index);
            bucket.add(taskEntry);
            if (bucket.setExpiration(virtualId * tickMs)) {
                queue.offer(bucket);
            }
            return true;
        }
        if (overflowWheel == null) {
            addOverflowWheel();
        }
        return overflowWheel.add(taskEntry);
    }
    
    /**
     * Advance clock.
     *
     * @param timeMs the time ms
     */
    void advanceClock(final long timeMs) {
        if (timeMs >= currentTime + tickMs) {
            currentTime = timeMs - (timeMs % tickMs);
        }
        if (overflowWheel != null) {
            overflowWheel.advanceClock(currentTime);
        }
    }
    
    private TimerTaskList getBucket(final int index) {
        TimerTaskList bucket = buckets[index];
        if (bucket == null) {
            synchronized (this) {
                bucket = buckets[index];
                if (bucket == null) {
                    bucket = new TimerTaskList(taskCounter);
                    buckets[index] = bucket;
                }
            }
        }
        return bucket;
    }
    
}
