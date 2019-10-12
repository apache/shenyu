/*
 *
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.common.cache;

import java.util.Collections;
import java.util.HashSet;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
import org.dromara.soul.common.utils.OsUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Loop time implementation.
 * io.netty.util.HashedWheelTimer
 *
 * @author sixh
 */
public class HashedWheelTimer implements Timer {

    private static final Logger LOGGER = LoggerFactory.getLogger(HashedWheelTimer.class);

    /**
     * 状态更新.
     */
    private static final AtomicIntegerFieldUpdater<HashedWheelTimer> WORKER_STATE_UPDATER;


    /**
     * 任务执行状态.
     */
    private static final int WORKER_STATE_INIT = 0;

    /**
     * 任务已启动.
     */
    private static final int WORKER_STATE_STARTED = 1;

    /**
     * 任务已结束.
     */
    private static final int WORKER_STATE_SHUTDOWN = 2;

    /**
     * 时间分片槽，默认512.
     */
    private final HashedWheelTimer.HashedWheelBucket[] wheel;

    /**
     * 执行时间分片槽的次数； {@link #wheel}长度 -1.
     */
    private final int mask;

    /**
     * 时间分片执行时间.
     */
    private final long tickDuration;

    /**
     * 执行线程.
     */
    private final Thread workerThread;

    /**
     * 任务执行实现.
     */
    private final HashedWheelTimer.Worker worker = new HashedWheelTimer.Worker();

    /**
     * 开始时间.
     */
    private volatile long startTime;

    /**
     * 开始时间初始化.
     */
    private final CountDownLatch startTimeInitialized = new CountDownLatch(1);


    /**
     * 超时器.
     */
    private final Queue<HashedWheelTimeout> timeouts = new ConcurrentLinkedQueue<>();

    /**
     * 取消超时器.
     */
    private final Queue<HashedWheelTimeout> cancelledTimeouts = new ConcurrentLinkedQueue<>();

    static {
        WORKER_STATE_UPDATER = AtomicIntegerFieldUpdater.newUpdater(HashedWheelTimer.class, "workerState");
    }

    /**
     * 0 - init, 1 - started, 2 - shut down.
     */
    @SuppressWarnings({"unused", "FieldMayBeFinal", "RedundantFieldInitialization"})
    private volatile int workerState = WORKER_STATE_INIT;

    /**
     * 初始化一个环形时间执行器.
     *
     * @param threadFactory 线程初始化工厂；
     */
    HashedWheelTimer(ThreadFactory threadFactory) {
        this(threadFactory, 100, TimeUnit.MILLISECONDS, 512);
    }

    /**
     * 初始化一个环形时间执行器.
     *
     * @param threadFactory 线程初始化工厂；
     * @param tickDuration  持续执行时间；
     * @param unit          时间单位；
     * @param ticksPerWheel 时间分片数；
     */
    private HashedWheelTimer(ThreadFactory threadFactory,
                             long tickDuration, TimeUnit unit, int ticksPerWheel) {
        /*
         * 创建时间分片区；
         */
        this.wheel = createWheel(ticksPerWheel);
        //执行时间分片槽的次数；
        mask = wheel.length - 1;
        //把所有时间转为纳秒进方便操作；
        this.tickDuration = unit.toNanos(tickDuration);
        // Prevent overflow.
        if (this.tickDuration >= Long.MAX_VALUE / wheel.length) {
            throw new IllegalArgumentException(String.format(
                    "tickDuration: %d (expected: 0 < tickDuration in nanos < %d",
                    tickDuration, Long.MAX_VALUE / wheel.length));
        }
        workerThread = threadFactory.newThread(worker);
    }

    /**
     * 启动任务.
     */
    private void start() {
        switch (WORKER_STATE_UPDATER.get(this)) {
            //更改线程状态；如果当前的状态是初始，则改成启动中；
            case WORKER_STATE_INIT:
                if (WORKER_STATE_UPDATER.compareAndSet(this, WORKER_STATE_INIT, WORKER_STATE_STARTED)) {
                    //启动线程；
                    workerThread.start();
                }
                break;
            case WORKER_STATE_STARTED:
                break;
            case WORKER_STATE_SHUTDOWN:
                throw new IllegalStateException("cannot be started once stopped");
            default:
                throw new Error("Invalid WorkerState");
        }

        // 等待初始化的Worker线程的startTime实始化；
        while (startTime == 0) {
            try {
                startTimeInitialized.await();
            } catch (InterruptedException ignore) {
                // Ignore - it will be ready very soon.
            }
        }
    }

    private static HashedWheelTimer.HashedWheelBucket[] createWheel(int ticksPerWheel) {
        int newTicksPerWheel = ticksPerWheel;
        if (newTicksPerWheel <= 0) {
            throw new IllegalArgumentException(
                    "ticksPerWheel must be greater than 0: " + newTicksPerWheel);
        }
        if (newTicksPerWheel > 1073741824) {
            throw new IllegalArgumentException(
                    "ticksPerWheel may not be greater than 2^30: " + newTicksPerWheel);
        }

        newTicksPerWheel = normalizeTicksPerWheel(newTicksPerWheel);
        HashedWheelTimer.HashedWheelBucket[] wheel = new HashedWheelTimer.HashedWheelBucket[newTicksPerWheel];
        for (int i = 0; i < wheel.length; i++) {
            wheel[i] = new HashedWheelTimer.HashedWheelBucket();
        }
        return wheel;
    }

    private static int normalizeTicksPerWheel(int ticksPerWheel) {
        int normalizedTicksPerWheel = 1;
        while (normalizedTicksPerWheel < ticksPerWheel) {
            normalizedTicksPerWheel <<= 1;
        }
        return normalizedTicksPerWheel;
    }

    @Override
    public Timeout newTimeout(TimerTask task, long delay, TimeUnit unit) {
        if (task == null) {
            throw new NullPointerException("task");
        }
        if (unit == null) {
            throw new NullPointerException("unit");
        }
        start();
        /*
         * 设置一下超时作务到列队中；计算出下一次的超时时间；
         */
        long deadline = System.nanoTime() + unit.toNanos(delay) - startTime;
        HashedWheelTimer.HashedWheelTimeout timeout = new HashedWheelTimer.HashedWheelTimeout(this, task, deadline);
        timeouts.add(timeout);
        return timeout;
    }

    @Override
    public Set<Timeout> stop() {
        if (Thread.currentThread() == workerThread) {
            throw new IllegalStateException(
                    HashedWheelTimer.class.getSimpleName() +
                    ".stop() cannot be called from " +
                    TimerTask.class.getSimpleName());
        }

        if (!WORKER_STATE_UPDATER.compareAndSet(this, WORKER_STATE_STARTED, WORKER_STATE_SHUTDOWN)) {
            // workerState can be 0 or 2 at this moment - let it always be 2.
            WORKER_STATE_UPDATER.set(this, WORKER_STATE_SHUTDOWN);
            return Collections.emptySet();
        }

        boolean interrupted = false;
        while (workerThread.isAlive()) {
            workerThread.interrupt();
            try {
                workerThread.join(100);
            } catch (InterruptedException ignored) {
                interrupted = true;
            }
        }

        if (interrupted) {
            Thread.currentThread().interrupt();
        }

        return worker.unprocessedTimeouts();
    }

    private final class Worker implements Runnable {
        private final Set<Timeout> unprocessedTimeouts = new HashSet<>();

        /**
         * 到达一个槽就是++.
         */
        private long tick;

        @Override
        public void run() {
            //初始化启动开始时间；
            startTime = System.nanoTime();
            if (startTime == 0) {
                // 因为startTime使用0来判断是否进行初始化，所以要保证他的初始化不为0;
                startTime = 1;
            }
            //结束执行start()方法；
            startTimeInitialized.countDown();

            do {
                //计算下一个到达的时间片；如果没有到达则需要等待；
                final long deadline = waitForNextTick();
                if (deadline > 0) {
                    int idx = (int) (tick & mask);
                    processCancelledTasks();
                    //取出一个时间分片槽
                    HashedWheelTimer.HashedWheelBucket bucket =
                            wheel[idx];
                    //转换时间分片槽。并将数据放入到时间分片槽上；
                    transferTimeoutsToBuckets();
                    bucket.expireTimeouts(deadline);
                    tick++;
                }
            } while (WORKER_STATE_UPDATER.get(HashedWheelTimer.this) == WORKER_STATE_STARTED);

            // 如果调用Stop方法，就会填充未超时的Task将他返回给用户；
            for (HashedWheelTimer.HashedWheelBucket bucket : wheel) {
                bucket.clearTimeouts(unprocessedTimeouts);
            }
            for (; ; ) {
                HashedWheelTimer.HashedWheelTimeout timeout = timeouts.poll();
                if (timeout == null) {
                    break;
                }
                if (!timeout.isCancelled()) {
                    unprocessedTimeouts.add(timeout);
                }
            }
            processCancelledTasks();
        }

        private void transferTimeoutsToBuckets() {
            // transfer only max. 100000 timeouts per tick to prevent a thread to stale the workerThread when it just
            // adds new timeouts in a loop.
            for (int i = 0; i < 100000; i++) {
                //获取一个作务；
                HashedWheelTimer.HashedWheelTimeout timeout = timeouts.poll();
                if (timeout == null) {
                    // all processed
                    break;
                }
                if (timeout.state() == HashedWheelTimer.HashedWheelTimeout.ST_CANCELLED) {
                    // Was cancelled in the meantime.
                    continue;
                }
                //需要轮转多少次，就可以到达时间分片上，（超时时间/任务分片轮转时间）
                long calculated = timeout.deadline / tickDuration;
                //计算还有剩余多少轮没有执行；
                timeout.remainingRounds = (calculated - tick) / wheel.length;
                // Ensure we don't schedule for past.
                final long ticks = Math.max(calculated, tick);
                //算出任务应该插入的时间分片槽位置；
                int stopIndex = (int) (ticks & mask);
                HashedWheelTimer.HashedWheelBucket bucket = wheel[stopIndex];
                bucket.addTimeout(timeout);
            }
        }

        /**
         * 流程任务取消；从取消队列中获取任务。并删除.
         */
        private void processCancelledTasks() {
            for (; ; ) {
                HashedWheelTimer.HashedWheelTimeout timeout = cancelledTimeouts.poll();
                if (timeout == null) {
                    // all processed
                    break;
                }
                try {
                    timeout.remove();
                } catch (Throwable t) {
                    if (LOGGER.isWarnEnabled()) {
                        LOGGER.warn("An exception was thrown while process a cancellation task", t);
                    }
                }
            }
        }

        /**
         * calculate goal nanoTime from startTime and current tick number,
         * then wait until that goal has been reached.
         *
         * @return Long.MIN_VALUE if received a shutdown request,
         * current time otherwise (with Long.MIN_VALUE changed by +1).
         */
        private long waitForNextTick() {
            //计算时间片；例：1000000*(1+1)
            long deadline = tickDuration * (tick + 1);

            for (; ; ) {
                //得到当前已经耗时；
                final long currentTime = System.nanoTime() - startTime;
                //得到休眠的时间毫秒数；
                long sleepTimeMs = (deadline - currentTime + 999999) / 1000000;

                if (sleepTimeMs <= 0) {
                    if (currentTime == Long.MIN_VALUE) {
                        return -Long.MAX_VALUE;
                    } else {
                        return currentTime;
                    }
                }

                // Check if we run on windows, as if thats the case we will need
                // to round the sleepTime as workaround for a bug that only affect
                // the JVM if it runs on windows.
                //
                // See https://github.com/netty/netty/issues/356
                if (OsUtils.isWindows()) {
                    sleepTimeMs = sleepTimeMs / 10 * 10;
                }

                try {
                    TimeUnit.MILLISECONDS.sleep(sleepTimeMs);
                } catch (InterruptedException ignored) {
                    if (WORKER_STATE_UPDATER.get(HashedWheelTimer.this) == WORKER_STATE_SHUTDOWN) {
                        return Long.MIN_VALUE;
                    }
                }
            }
        }

        Set<Timeout> unprocessedTimeouts() {
            return Collections.unmodifiableSet(unprocessedTimeouts);
        }
    }

    /**
     * 超时任务对象.
     */
    private static final class HashedWheelTimeout implements Timeout {

        private static final int ST_INIT = 0;

        private static final int ST_CANCELLED = 1;

        private static final int ST_EXPIRED = 2;

        private static final AtomicIntegerFieldUpdater<HashedWheelTimeout> STATE_UPDATER;

        static {
            AtomicIntegerFieldUpdater<HashedWheelTimeout> updater =
                    AtomicIntegerFieldUpdater.newUpdater(HashedWheelTimer.HashedWheelTimeout.class, "state");
            STATE_UPDATER = updater;
        }

        private final HashedWheelTimer timer;

        private final TimerTask task;

        private final long deadline;

        @SuppressWarnings({"unused", "FieldMayBeFinal", "RedundantFieldInitialization"})
        private volatile int state = ST_INIT;

        /**
         * remainingRounds will be calculated and set by Worker.transferTimeoutsToBuckets() before the
         * HashedWheelTimeout will be added to the correct HashedWheelBucket.
         */
        private long remainingRounds;

        /**
         * This will be used to chain timeouts in HashedWheelTimerBucket via a double-linked-list.
         * As only the workerThread will act on it there is no need for synchronization / volatile.
         */
        private HashedWheelTimer.HashedWheelTimeout next;

        private HashedWheelTimer.HashedWheelTimeout prev;

        /**
         * The bucket to which the timeout was added
         */
        private HashedWheelTimer.HashedWheelBucket bucket;

        HashedWheelTimeout(HashedWheelTimer timer, TimerTask task, long deadline) {
            this.timer = timer;
            this.task = task;
            this.deadline = deadline;
        }

        @Override
        public Timer timer() {
            return timer;
        }

        @Override
        public TimerTask task() {
            return task;
        }

        @Override
        public long deadline() {
            return deadline;
        }

        @Override
        public boolean cancel() {
            // only update the state it will be removed from HashedWheelBucket on next tick.
            if (!compareAndSetState(ST_INIT, ST_CANCELLED)) {
                return false;
            }
            // If a task should be canceled we put this to another queue which will be processed on each tick.
            // So this means that we will have a GC latency of max. 1 tick duration which is good enough. This way
            // we can make again use of our MpscLinkedQueue and so minimize the locking / overhead as much as possible.
            timer.cancelledTimeouts.add(this);
            return true;
        }

        void remove() {
            HashedWheelTimer.HashedWheelBucket bucket = this.bucket;
            if (bucket != null) {
                bucket.remove(this);
            }
        }

        boolean compareAndSetState(int expected, int state) {
            return STATE_UPDATER.compareAndSet(this, expected, state);
        }

        int state() {
            return state;
        }

        @Override
        public boolean isCancelled() {
            return state() == ST_CANCELLED;
        }

        @Override
        public boolean isExpired() {
            return state() == ST_EXPIRED;
        }

        void expire() {
            if (!compareAndSetState(ST_INIT, ST_EXPIRED)) {
                return;
            }

            try {
                task.run(this);
            } catch (Throwable t) {
                if (LOGGER.isWarnEnabled()) {
                    LOGGER.warn("An exception was thrown by " + TimerTask.class.getSimpleName() + '.', t);
                }
            }
        }

        @Override
        public boolean isDefault() {
            return false;
        }
    }

    /**
     * 每个分片槽封装对象，
     * 这里使用的是双向队列.
     */
    private static final class HashedWheelBucket {
        private HashedWheelTimeout head;

        private HashedWheelTimeout tail;

        /**
         * Add {@link HashedWheelTimeout} to this bucket.
         */
        void addTimeout(HashedWheelTimeout timeout) {
            assert timeout.bucket == null;
            timeout.bucket = this;
            if (head == null) {
                head = tail = timeout;
            } else {
                tail.next = timeout;
                timeout.prev = tail;
                tail = timeout;
            }
        }

        /**
         * Expire all {@link HashedWheelTimeout}s for the given {@code deadline}.
         */
        void expireTimeouts(long deadline) {
            HashedWheelTimeout timeout = head;
            // process all timeouts
            while (timeout != null) {
                boolean remove = false;
                if (timeout.remainingRounds <= 0) {
                    if (timeout.deadline <= deadline) {
                        timeout.expire();
                    } else {
                        // The timeout was placed into a wrong slot. This should never happen.
                        throw new IllegalStateException(String.format(
                                "timeout.deadline (%d) > deadline (%d)", timeout.deadline, deadline));
                    }
                    remove = true;
                } else if (timeout.isCancelled()) {
                    remove = true;
                } else {
                    timeout.remainingRounds--;
                }
                // store reference to next as we may null out timeout.next in the remove block.
                HashedWheelTimeout next = timeout.next;
                if (remove) {
                    remove(timeout);
                }
                timeout = next;
            }
        }

        void remove(HashedWheelTimeout timeout) {
            HashedWheelTimeout next = timeout.next;
            // remove timeout that was either processed or cancelled by updating the linked-list
            if (timeout.prev != null) {
                timeout.prev.next = next;
            }
            if (timeout.next != null) {
                timeout.next.prev = timeout.prev;
            }

            if (timeout == head) {
                // if timeout is also the tail we need to adjust the entry too
                if (timeout == tail) {
                    tail = null;
                    head = null;
                } else {
                    head = next;
                }
            } else if (timeout == tail) {
                // if the timeout is the tail modify the tail to be the prev node.
                tail = timeout.prev;
            }
            // null out prev, next and bucket to allow for GC.
            timeout.prev = null;
            timeout.next = null;
            timeout.bucket = null;
        }

        /**
         * Clear this bucket and return all not expired / cancelled {@link Timeout}s.
         */
        void clearTimeouts(Set<Timeout> set) {
            for (; ; ) {
                HashedWheelTimeout timeout = pollTimeout();
                if (timeout == null) {
                    return;
                }
                if (timeout.isExpired() || timeout.isCancelled()) {
                    continue;
                }
                set.add(timeout);
            }
        }

        private HashedWheelTimeout pollTimeout() {
            HashedWheelTimeout head = this.head;
            if (head == null) {
                return null;
            }
            HashedWheelTimeout next = head.next;
            if (next == null) {
                tail = this.head = null;
            } else {
                this.head = next;
                next.prev = null;
            }

            // null out prev and next to allow for GC.
            head.next = null;
            head.prev = null;
            head.bucket = null;
            return head;
        }
    }

}
