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
 *
 */

package org.apache.shenyu.disruptor.thread;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

/**
 * OrderlyExecutor .
 *
 * @author sixh chenbin
 */
public class OrderlyExecutor extends ThreadPoolExecutor {

    private final TreeMap<Long, SingletonExecutor> virtualExecutors = new TreeMap<>();

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
     */
    public OrderlyExecutor(final int corePoolSize,
                           final int maximumPoolSize,
                           final long keepAliveTime,
                           final TimeUnit unit,
                           final BlockingQueue<Runnable> workQueue,
                           final ThreadFactory threadFactory,
                           final RejectedExecutionHandler handler) {
        super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler);
        IntStream.range(0, corePoolSize).forEach((index) -> {
            SingletonExecutor singletonExecutor = new SingletonExecutor(threadFactory);
            String hash = singletonExecutor.hashCode() + ":" + index;
            byte[] bytes = threadSelector.md5(hash);
            for (int i = 0; i < 4; i++) {
                this.virtualExecutors.put(threadSelector.hash(bytes, i), singletonExecutor);
            }
        });
    }

    /**
     * Select.
     *
     * @param hash the hash code
     * @return the singleton executor
     */
    public SingletonExecutor select(String hash) {
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

    /**
     * The type Thread selector.
     */
    final static class ThreadSelector {

        /**
         * Instantiates a new Thread selector.
         */
        public ThreadSelector() {
        }

        /**
         * Select long.
         *
         * @param hash the hash
         * @return the long
         */
        public long select(String hash) {
            byte[] digest = md5(hash);
            return hash(digest, 0);
        }

        /**
         * ketame  hash.
         */
        private long hash(byte[] digest, int number) {
            return (((long) (digest[3 + number * 4] & 0xFF) << 24)
                    | ((long) (digest[2 + number * 4] & 0xFF) << 16)
                    | ((long) (digest[1 + number * 4] & 0xFF) << 8)
                    | (digest[number * 4] & 0xFF))
                    & 0xFFFFFFFFL;
        }

        private byte[] md5(String hash) {
            MessageDigest md5;
            try {
                md5 = MessageDigest.getInstance("MD5");
            } catch (NoSuchAlgorithmException e) {
                throw new IllegalStateException(e.getMessage(), e);
            }
            md5.reset();
            byte[] bytes;
            bytes = hash.getBytes(StandardCharsets.UTF_8);
            md5.update(bytes);
            return md5.digest();
        }
    }
}
