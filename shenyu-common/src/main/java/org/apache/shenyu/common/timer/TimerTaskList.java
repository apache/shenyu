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

import java.util.Iterator;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;

/**
 * TimerTaskList .
 */
public class TimerTaskList implements Delayed, Iterable<TimerTask> {
    
    private final TimerTaskEntry root;
    
    private final AtomicInteger taskCounter;
    
    private final AtomicLong expiration = new AtomicLong(-1L);
    
    /**
     * Instantiates a new Timer task list.
     *
     * @param taskCounter the task counter
     */
    public TimerTaskList(final AtomicInteger taskCounter) {
        this.taskCounter = taskCounter;
        root = new TimerTaskEntry(null, null, -1L);
        root.next = root;
        root.prev = root;
    }
    
    /**
     * Sets expiration.
     *
     * @param expirationMs the expiration ms
     * @return the expiration
     */
    public boolean setExpiration(final long expirationMs) {
        return expiration.getAndSet(expirationMs) != expirationMs;
    }
    
    /**
     * Get the bucket's expiration time.
     *
     * @return the expiration
     */
    public long getExpiration() {
        return expiration.get();
    }
    
    /**
     * Flush.
     *
     * @param consumer the consumer
     */
    synchronized void flush(final Consumer<TimerTaskEntry> consumer) {
        TimerTaskEntry head = root.next;
        while (head != root) {
            this.remove(head);
            consumer.accept(head);
            head = root.next;
        }
        expiration.set(-1L);
    }
    
    /**
     * Add.
     *
     * @param timerTaskEntry the timer task entry
     */
    public void add(final TimerTaskEntry timerTaskEntry) {
        boolean done = false;
        while (!done) {
            timerTaskEntry.remove();
            synchronized (this) {
                if (timerTaskEntry.list == null) {
                    TimerTaskEntry tail = root.prev;
                    timerTaskEntry.next = root;
                    timerTaskEntry.prev = tail;
                    timerTaskEntry.list = this;
                    tail.next = timerTaskEntry;
                    root.prev = timerTaskEntry;
                    taskCounter.incrementAndGet();
                    done = true;
                }
            }
        }
    }
    
    /**
     * Traversing using this is thread-safe.
     *
     * @param consumer the consumer
     */
    public synchronized void foreach(final Consumer<TimerTask> consumer) {
        TimerTaskEntry entry = root.next;
        while (entry != root) {
            TimerTaskEntry next = entry.next;
            if (!entry.cancelled()) {
                consumer.accept(entry.timerTask);
            }
            entry = next;
        }
    }
    
    @Override
    public long getDelay(final TimeUnit unit) {
        long millis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime());
        return unit.convert(Math.max(getExpiration() - millis, 0), TimeUnit.MILLISECONDS);
    }
    
    @Override
    public int compareTo(final Delayed delayed) {
        boolean other = delayed instanceof TimerTaskList;
        if (other) {
            long expiration = ((TimerTaskList) delayed).getExpiration();
            return Long.compare(getExpiration(), expiration);
        } else {
            return -1;
        }
    }
    
    /**
     * Remove.
     *
     * @param timerTaskEntry the timer task entry
     */
    public void remove(final TimerTaskEntry timerTaskEntry) {
        synchronized (this) {
            if (timerTaskEntry.list == this) {
                timerTaskEntry.next.prev = timerTaskEntry.prev;
                timerTaskEntry.prev.next = timerTaskEntry.next;
                timerTaskEntry.next = null;
                timerTaskEntry.prev = null;
                timerTaskEntry.list = null;
                taskCounter.decrementAndGet();
            }
        }
    }
    
    /**
     * Using Iterator is not thread safe.
     *
     * @return an Iterator.
     */
    @Override
    public Iterator<TimerTask> iterator() {
        return new Itr(root.next);
    }
    
    
    /**
     * The type Timer task entry.
     */
    public static class TimerTaskEntry implements TaskEntity, Comparable<TimerTaskEntry> {
        
        private final Timer timer;
        
        private final TimerTask timerTask;
        
        private final Long expirationMs;
        
        /**
         * The List.
         */
        private TimerTaskList list;
        
        /**
         * The Next.
         */
        private TimerTaskEntry next;
        
        /**
         * The Prev.
         */
        private TimerTaskEntry prev;
        
        /**
         * Instantiates a new Timer task entry.
         *
         * @param timer        the timer
         * @param timerTask    the timer task
         * @param expirationMs the expiration ms
         */
        public TimerTaskEntry(final Timer timer, final TimerTask timerTask, final Long expirationMs) {
            this.timerTask = timerTask;
            this.expirationMs = expirationMs;
            this.timer = timer;
            if (timerTask != null) {
                timerTask.setTimerTaskEntry(this);
            }
        }
        
        /**
         * Has the current task been cancelled.
         *
         * @return the boolean
         */
        @Override
        public boolean cancelled() {
            return this.timerTask.getTimerTaskEntry() != this;
        }
        
        /**
         * Cancel boolean.
         */
        @Override
        public void cancel() {
            this.timerTask.cancel();
        }
        
        /**
         * Gets expiration ms.
         *
         * @return the expiration ms
         */
        public Long getExpirationMs() {
            return expirationMs;
        }
        
        /**
         * Gets timer.
         *
         * @return the timer
         */
        @Override
        public Timer getTimer() {
            return this.timer;
        }
        
        /**
         * Gets timer task.
         *
         * @return the timer task
         */
        @Override
        public TimerTask getTimerTask() {
            return timerTask;
        }
        
        /**
         * Remove.
         */
        void remove() {
            TimerTaskList currentList = list;
            while (currentList != null) {
                currentList.remove(this);
                currentList = list;
            }
        }
        
        @Override
        public int compareTo(final TimerTaskEntry timerTaskEntry) {
            return Long.compare(expirationMs, timerTaskEntry.expirationMs);
        }
    }
    
    /**
     * The type Itr.
     */
    private class Itr implements Iterator<TimerTask> {
        
        private TimerTaskEntry entry;
        
        /**
         * Instantiates a new Itr.
         *
         * @param entry the entry
         */
        Itr(final TimerTaskEntry entry) {
            this.entry = entry;
        }
        
        @Override
        public boolean hasNext() {
            if (entry != root) {
                return !entry.cancelled();
            }
            return false;
        }
        
        @Override
        public TimerTask next() {
            TimerTask timerTask = null;
            if (entry != root) {
                TimerTaskEntry nextEntry = entry.next;
                if (!entry.cancelled()) {
                    timerTask = entry.timerTask;
                }
                entry = nextEntry;
            }
            return timerTask;
        }
        
        @Override
        public void remove() {
            entry.remove();
        }
    }
}
