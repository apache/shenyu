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

package org.apache.shenyu.disruptor.thread;

import com.google.common.hash.Hashing;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.SortedMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

/**
 * OrderlyExecutor .
 */
public class OrderlyExecutor extends ThreadPoolExecutor {
    
    private final ConcurrentSkipListMap<Long, SingletonExecutor> virtualExecutors = new ConcurrentSkipListMap<>();
    
    private final ThreadSelector threadSelector = new ThreadSelector();
    
    /**
     * Instantiates a new Orderly executor.
     *
     * @param corePoolSize    the core pool size
     * @param maximumPoolSize the maximum pool size
     * @param keepAliveTime   the keep alive time
     * @param unit            the unit
     * @param workQueue       the work queue
     * @param threadFactory   the thread factory
     * @param handler         the handler
     * @param isOrderly the orderly Whether to execute sequentially.
     */
    public OrderlyExecutor(
            final boolean isOrderly,
            final int corePoolSize,
            final int maximumPoolSize,
            final long keepAliveTime,
            final TimeUnit unit,
            final BlockingQueue<Runnable> workQueue,
            final ThreadFactory threadFactory,
            final RejectedExecutionHandler handler) {
        super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler);
        orderlyThreadPool(isOrderly, corePoolSize, threadFactory);
    }
    
    private void orderlyThreadPool(final boolean isOrderly, final int corePoolSize, final ThreadFactory threadFactory) {
        if (isOrderly) {
            IntStream.range(0, corePoolSize).forEach(index -> {
                SingletonExecutor singletonExecutor = new SingletonExecutor(threadFactory);
                String hash = singletonExecutor.hashCode() + ":" + index;
                byte[] bytes = threadSelector.sha(hash);
                for (int i = 0; i < 4; i++) {
                    this.virtualExecutors.put(threadSelector.hash(bytes, i), singletonExecutor);
                }
            });
        }
    }
    
    /**
     * Select.
     *
     * @param hash the hash code
     * @return the singleton executor
     */
    public SingletonExecutor select(final String hash) {
        long select = threadSelector.select(hash);
        if (!virtualExecutors.containsKey(select)) {
            SortedMap<Long, SingletonExecutor> tailMap = virtualExecutors.tailMap(select);
            if (tailMap.isEmpty()) {
                select = virtualExecutors.firstKey();
            } else {
                select = tailMap.firstKey();
            }
        }
        return virtualExecutors.get(select);
    }

    @Override
    public void shutdown() {
        new HashSet<>(virtualExecutors.values()).forEach(SingletonExecutor::shutdown);
        super.shutdown();
    }

    @Override
    public List<Runnable> shutdownNow() {
        List<Runnable> pending = new ArrayList<>();
        new HashSet<>(virtualExecutors.values()).forEach(executor -> pending.addAll(executor.shutdownNow()));
        pending.addAll(super.shutdownNow());
        return pending;
    }

    @Override
    public boolean isTerminated() {
        return super.isTerminated() && virtualExecutors.values().stream().allMatch(SingletonExecutor::isTerminated);
    }

    @Override
    public boolean awaitTermination(final long timeout, final TimeUnit unit) throws InterruptedException {
        long remaining = unit.toNanos(timeout);
        long start = System.nanoTime();
        if (!super.awaitTermination(remaining, TimeUnit.NANOSECONDS)) {
            return false;
        }
        remaining -= System.nanoTime() - start;
        for (SingletonExecutor executor : new HashSet<>(virtualExecutors.values())) {
            start = System.nanoTime();
            if (!executor.awaitTermination(Math.max(0, remaining), TimeUnit.NANOSECONDS)) {
                return false;
            }
            remaining -= System.nanoTime() - start;
        }
        return true;
    }
    
    /**
     * The type Thread selector.
     */
    private static final class ThreadSelector {
        
        /**
         * Select long.
         *
         * @param hash the hash
         * @return the long
         */
        public long select(final String hash) {
            byte[] digest = sha(hash);
            return hash(digest, 0);
        }
        
        /**
         * ketame  hash.
         */
        private long hash(final byte[] digest, final int number) {
            return (((long) (digest[3 + number * 4] & 0xFF) << 24)
                    | ((long) (digest[2 + number * 4] & 0xFF) << 16)
                    | ((long) (digest[1 + number * 4] & 0xFF) << 8)
                    | (digest[number * 4] & 0xFF))
                    & 0xFFFFFFFFL;
        }
        
        private byte[] sha(final String hash) {
            byte[] bytes = hash.getBytes(StandardCharsets.UTF_8);
            return Hashing
                    .sha256()
                    .newHasher()
                    .putBytes(bytes)
                    .hash().asBytes();
        }
    }
}
