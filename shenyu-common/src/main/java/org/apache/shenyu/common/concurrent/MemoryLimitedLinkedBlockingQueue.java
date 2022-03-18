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
import java.util.Collection;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Can completely solve the OOM problem caused by {@link java.util.concurrent.LinkedBlockingQueue}.
 *
 * @see org.apache.shenyu.common.concurrent.MemoryLimiter
 * @see org.apache.shenyu.common.concurrent.MemoryLimitCalculator
 */
public class MemoryLimitedLinkedBlockingQueue<E> extends LinkedBlockingQueue<E> {

    private static final long serialVersionUID = -6106022470621447542L;

    private final MemoryLimiter memoryLimiter;

    public MemoryLimitedLinkedBlockingQueue(final Instrumentation inst) {
        this(Integer.MAX_VALUE, inst);
    }

    public MemoryLimitedLinkedBlockingQueue(final long memoryLimit,
                                            final Instrumentation inst) {
        super(Integer.MAX_VALUE);
        this.memoryLimiter = new MemoryLimiter(memoryLimit, inst);
    }

    public MemoryLimitedLinkedBlockingQueue(final Collection<? extends E> c,
                                            final long memoryLimit,
                                            final Instrumentation inst) {
        super(c);
        this.memoryLimiter = new MemoryLimiter(memoryLimit, inst);
    }

    /**
     * set the memory limit.
     *
     * @param memoryLimit the memory limit
     */
    public void setMemoryLimit(final long memoryLimit) {
        memoryLimiter.setMemoryLimit(memoryLimit);
    }

    /**
     * get the memory limit.
     *
     * @return the memory limit
     */
    public long getMemoryLimit() {
        return memoryLimiter.getMemoryLimit();
    }

    /**
     * get the current memory.
     *
     * @return the current memory
     */
    public long getCurrentMemory() {
        return memoryLimiter.getCurrentMemory();
    }

    /**
     * get the current remain memory.
     *
     * @return the current remain memory
     */
    public long getCurrentRemainMemory() {
        return memoryLimiter.getCurrentRemainMemory();
    }

    @Override
    public void put(final E e) throws InterruptedException {
        memoryLimiter.acquireInterruptibly(e);
        super.put(e);
    }

    @Override
    public boolean offer(final E e, final long timeout, final TimeUnit unit) throws InterruptedException {
        return memoryLimiter.acquire(e, timeout, unit) && super.offer(e, timeout, unit);
    }

    @Override
    public boolean offer(final E e) {
        return memoryLimiter.acquire(e) && super.offer(e);
    }

    @Override
    public E take() throws InterruptedException {
        final E e = super.take();
        memoryLimiter.releaseInterruptibly(e);
        return e;
    }

    @Override
    public E poll(final long timeout, final TimeUnit unit) throws InterruptedException {
        final E e = super.poll(timeout, unit);
        memoryLimiter.releaseInterruptibly(e, timeout, unit);
        return e;
    }

    @Override
    public E poll() {
        final E e = super.poll();
        memoryLimiter.release(e);
        return e;
    }

    @Override
    public boolean remove(final Object o) {
        final boolean success = super.remove(o);
        if (success) {
            memoryLimiter.release(o);
        }
        return success;
    }

    @Override
    public void clear() {
        super.clear();
        memoryLimiter.reset();
    }
}
