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

import java.lang.instrument.Instrumentation;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.LongAdder;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * memory limiter.
 *
 * @see org.apache.shenyu.common.concurrent.MemoryLimitCalculator
 */
public class MemoryLimiter {

    private final Instrumentation inst;

    private long memoryLimit;

    private final LongAdder memory = new LongAdder();

    private final ReentrantLock acquireLock = new ReentrantLock();

    private final Condition notLimited = acquireLock.newCondition();

    private final ReentrantLock releaseLock = new ReentrantLock();

    private final Condition notEmpty = releaseLock.newCondition();

    public MemoryLimiter(final Instrumentation inst) {
        this(Integer.MAX_VALUE, inst);
    }

    public MemoryLimiter(final long memoryLimit, final Instrumentation inst) {
        if (memoryLimit <= 0) {
            throw new IllegalArgumentException();
        }
        this.memoryLimit = memoryLimit;
        this.inst = inst;
    }

    /**
     * set the memory limit.
     *
     * @param memoryLimit the memory limit
     */
    public void setMemoryLimit(final long memoryLimit) {
        if (memoryLimit <= 0) {
            throw new IllegalArgumentException();
        }
        this.memoryLimit = memoryLimit;
    }

    /**
     * get the memory limit.
     *
     * @return the memory limit
     */
    public long getMemoryLimit() {
        return memoryLimit;
    }

    /**
     * get the current memory.
     *
     * @return the current memory
     */
    public long getCurrentMemory() {
        return memory.sum();
    }

    /**
     * get the current remain memory.
     *
     * @return the current remain memory
     */
    public long getCurrentRemainMemory() {
        return getMemoryLimit() - getCurrentMemory();
    }

    private void signalNotEmpty() {
        releaseLock.lock();
        try {
            notEmpty.signal();
        } finally {
            releaseLock.unlock();
        }
    }

    private void signalNotLimited() {
        acquireLock.lock();
        try {
            notLimited.signal();
        } finally {
            acquireLock.unlock();
        }
    }

    /**
     * Locks to prevent both acquires and releases.
     */
    private void fullyLock() {
        acquireLock.lock();
        releaseLock.lock();
    }

    /**
     * Unlocks to allow both acquires and releases.
     */
    private void fullyUnlock() {
        releaseLock.unlock();
        acquireLock.unlock();
    }

    /**
     * acquire memory by {@link Object}.
     * this method does not respond to interrupts.
     *
     * @param o memory size to be applied by calculating
     * @return true if acquire success
     */
    public boolean acquire(final Object o) {
        if (Objects.isNull(o)) {
            throw new NullPointerException();
        }
        if (memory.sum() >= memoryLimit) {
            return false;
        }
        acquireLock.lock();
        try {
            final long sum = memory.sum();
            final long objectSize = inst.getObjectSize(o);
            if (sum + objectSize >= memoryLimit) {
                return false;
            }
            memory.add(objectSize);
            if (memory.sum() < memoryLimit) {
                notLimited.signal();
            }
        } finally {
            acquireLock.unlock();
        }
        if (memory.sum() > 0) {
            signalNotEmpty();
        }
        return true;
    }

    /**
     * acquire memory by {@link Object}.
     * this method response to interrupts.
     *
     * @param o       memory size to be applied by calculating
     * @param timeout max time to wait
     * @param unit    time unit
     * @return true if acquire success
     * @throws InterruptedException the InterruptedException
     */
    public boolean acquire(final Object o, final long timeout,
                           final TimeUnit unit) throws InterruptedException {
        if (Objects.isNull(o)) {
            throw new NullPointerException();
        }
        long nanos = unit.toNanos(timeout);
        acquireLock.lockInterruptibly();
        try {
            final long objectSize = inst.getObjectSize(o);
            while (memory.sum() + objectSize >= memoryLimit) {
                if (nanos <= 0) {
                    return false;
                }
                nanos = notLimited.awaitNanos(nanos);
            }
            memory.add(objectSize);
            if (memory.sum() < memoryLimit) {
                notLimited.signal();
            }
        } finally {
            acquireLock.unlock();
        }
        if (memory.sum() > 0) {
            signalNotEmpty();
        }
        return true;
    }

    /**
     * acquire memory by {@link Object}.
     * this method response to interrupts.
     *
     * @param o memory size to be applied by calculating
     * @throws InterruptedException the InterruptedException
     */
    public void acquireInterruptibly(final Object o) throws InterruptedException {
        if (Objects.isNull(o)) {
            throw new NullPointerException();
        }
        acquireLock.lockInterruptibly();
        try {
            final long objectSize = inst.getObjectSize(o);
            while (memory.sum() + objectSize >= memoryLimit) {
                notLimited.await();
            }
            memory.add(objectSize);
            if (memory.sum() < memoryLimit) {
                notLimited.signal();
            }
        } finally {
            acquireLock.unlock();
        }
        if (memory.sum() > 0) {
            signalNotEmpty();
        }
    }

    /**
     * release memory by {@link Object}.
     * this method does not respond to interrupts.
     *
     * @param o memory size to be applied by calculating
     */
    public void release(final Object o) {
        if (Objects.isNull(o)) {
            return;
        }
        if (memory.sum() == 0) {
            return;
        }
        releaseLock.lock();
        try {
            final long objectSize = inst.getObjectSize(o);
            if (memory.sum() > 0) {
                memory.add(-objectSize);
                if (memory.sum() > 0) {
                    notEmpty.signal();
                }
            }
        } finally {
            releaseLock.unlock();
        }
        if (memory.sum() < memoryLimit) {
            signalNotLimited();
        }
    }

    /**
     * release memory by {@link Object}.
     * this method response to interrupts.
     *
     * @param o memory size to be applied by calculating
     * @throws InterruptedException the InterruptedException
     */
    public void releaseInterruptibly(final Object o) throws InterruptedException {
        if (Objects.isNull(o)) {
            return;
        }
        releaseLock.lockInterruptibly();
        try {
            final long objectSize = inst.getObjectSize(o);
            while (memory.sum() == 0) {
                notEmpty.await();
            }
            memory.add(-objectSize);
            if (memory.sum() > 0) {
                notEmpty.signal();
            }
        } finally {
            releaseLock.unlock();
        }
        if (memory.sum() < memoryLimit) {
            signalNotLimited();
        }
    }

    /**
     * release memory by {@link Object}.
     * this method response to interrupts.
     *
     * @param o       memory size to be applied by calculating
     * @param timeout max time to wait
     * @param unit    time unit
     * @throws InterruptedException the InterruptedException
     */
    public void releaseInterruptibly(final Object o, final long timeout,
                                     final TimeUnit unit) throws InterruptedException {
        if (Objects.isNull(o)) {
            return;
        }
        long nanos = unit.toNanos(timeout);
        releaseLock.lockInterruptibly();
        try {
            final long objectSize = inst.getObjectSize(o);
            while (memory.sum() == 0) {
                if (nanos <= 0) {
                    return;
                }
                nanos = notEmpty.awaitNanos(nanos);
            }
            memory.add(-objectSize);
            if (memory.sum() > 0) {
                notEmpty.signal();
            }
        } finally {
            releaseLock.unlock();
        }
        if (memory.sum() < memoryLimit) {
            signalNotLimited();
        }
    }

    /**
     * reset this MemoryLimiter.
     */
    public void reset() {
        fullyLock();
        try {
            if (memory.sumThenReset() < memoryLimit) {
                notLimited.signal();
            }
        } finally {
            fullyUnlock();
        }
    }
}
