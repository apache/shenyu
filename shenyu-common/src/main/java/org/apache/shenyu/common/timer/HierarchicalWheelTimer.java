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

import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;

import java.util.concurrent.DelayQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * HierarchicalWheelTimer
 * The type Hierarchical Wheel timer.
 *
 * @see TimingWheel
 */
public class HierarchicalWheelTimer implements Timer {
    
    private static final AtomicIntegerFieldUpdater<HierarchicalWheelTimer> WORKER_STATE_UPDATER =
            AtomicIntegerFieldUpdater.newUpdater(HierarchicalWheelTimer.class, "workerState");
    
    private final ExecutorService taskExecutor;
    
    private final DelayQueue<TimerTaskList> delayQueue = new DelayQueue<>();
    
    private final AtomicInteger taskCounter = new AtomicInteger(0);
    
    private final TimingWheel timingWheel;
    
    private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    
    private final ReentrantReadWriteLock.ReadLock readLock = readWriteLock.readLock();
    
    private final ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
    
    private volatile int workerState;
    
    private final Thread workerThread;
    
    /**
     * Instantiates a new System timer.
     *
     * @param executorName the executor name
     */
    public HierarchicalWheelTimer(final String executorName) {
        this(executorName, 1L, 20, TimeUnit.NANOSECONDS.toMillis(System.nanoTime()));
    }
    
    /**
     * Instantiates a new System timer.
     *
     * @param executorName the executor name
     * @param tickMs       the tick ms
     * @param wheelSize    the wheel size
     * @param startMs      the start ms
     */
    public HierarchicalWheelTimer(final String executorName,
                                  final Long tickMs,
                                  final Integer wheelSize,
                                  final Long startMs) {
        ThreadFactory threadFactory = ShenyuThreadFactory.create(executorName, false);
        taskExecutor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable>(), threadFactory);
        workerThread = threadFactory.newThread(new Worker(this));
        timingWheel = new TimingWheel(tickMs, wheelSize, startMs, taskCounter, delayQueue);
    }
    
    @Override
    public void add(final TimerTask timerTask) {
        if (timerTask == null) {
            throw new NullPointerException("timer task null");
        }
        this.readLock.lock();
        try {
            start();
            long millis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime());
            this.addTimerTaskEntry(new TimerTaskList.TimerTaskEntry(this, timerTask, timerTask.getDelayMs() + millis));
        } finally {
            this.readLock.unlock();
        }
        
    }
    
    private void addTimerTaskEntry(final TimerTaskList.TimerTaskEntry timerTaskEntry) {
        if (!timingWheel.add(timerTaskEntry)) {
            if (!timerTaskEntry.cancelled()) {
                taskExecutor.submit(() -> timerTaskEntry.getTimerTask().run(timerTaskEntry));
            }
        }
    }
    
    @Override
    public void advanceClock(final long timeoutMs) throws InterruptedException {
        TimerTaskList bucket = delayQueue.poll(timeoutMs, TimeUnit.MILLISECONDS);
        if (bucket != null) {
            writeLock.lock();
            try {
                while (bucket != null) {
                    timingWheel.advanceClock(bucket.getExpiration());
                    bucket.flush(this::addTimerTaskEntry);
                    bucket = delayQueue.poll();
                }
            } finally {
                writeLock.unlock();
            }
        }
    }
    
    private void start() {
        int state = WORKER_STATE_UPDATER.get(this);
        if (state == 0) {
            if (WORKER_STATE_UPDATER.compareAndSet(this, 0, 1)) {
                workerThread.start();
            }
        }
    }
    
    @Override
    public int size() {
        return taskCounter.get();
    }
    
    @Override
    public void shutdown() {
        taskExecutor.shutdown();
    }
    
    private static class Worker implements Runnable {
        
        private final Timer timer;
        
        /**
         * Instantiates a new Worker.
         *
         * @param timer the timer
         */
        Worker(final Timer timer) {
            this.timer = timer;
        }
        
        @Override
        public void run() {
            while (true) {
                try {
                    timer.advanceClock(100L);
                } catch (InterruptedException ignored) {
                }
            }
        }
    }
}
