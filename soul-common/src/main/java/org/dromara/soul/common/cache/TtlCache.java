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

import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.timer.HashedWheelTimer;
import org.dromara.soul.common.timer.Timeout;
import org.dromara.soul.common.timer.TimerTask;

import java.util.concurrent.TimeUnit;

/**
 * 实现一个关于时间的过期的缓存；当缓存的对象到期后，
 * 自动推送数据给相应的监听器。进行后续处理.
 * CreateDate: 2018-01-22
 *
 * @author chenbin
 */
public class TtlCache<K, V> {

    /**
     * 过期时间.
     */
    private final long expire;

    /**
     * 环形时间队列.
     */
    private final HashedWheelTimer timer;

    /**
     * 增加监听器.
     */
    private RemovalListener<K, V> removalListener;

    /**
     * 超时的时间单位.
     */
    private final TimeUnit unit;

    /**
     * 生成一个过期缓存对象.
     *
     * @param expire    执行；
     * @param unit      时间单位；
     * @param cacheName 设置一个缓存的名字；
     */
    public TtlCache(long expire, TimeUnit unit, String cacheName) {
        this.expire = expire;
        timer = new HashedWheelTimer(SoulThreadFactory.create(cacheName));
        this.unit = unit;
    }

    /**
     * 增加一个缓存移除的监听器.
     *
     * @param listener 监听器；
     */
    public void addRemovalListener(RemovalListener<K, V> listener) {
        this.removalListener = listener;
    }

    /**
     * 接收缓存变化值.
     *
     * @param k key
     * @param v value
     * @return timeout
     * @see Timeout
     */
    public Timeout put(K k, V v) {
        return put(k, v, this.expire, this.unit);
    }

    /**
     * 接收缓存变化值 ,灵活指定过期时间.
     *
     * @param k      key
     * @param v      value
     * @param expire 过期时间；
     * @param unit   单位；
     * @return Timeout
     */
    public Timeout put(K k, V v, long expire, TimeUnit unit) {
        TtlCacheTimerTask ttlCacheTimerTask = new TtlCacheTimerTask(k, v, expire, unit);
        return timer.newTimeout(ttlCacheTimerTask, expire, unit);
    }

    /**
     * 保存.
     */
    public class TtlCacheTimerTask implements TimerTask {
        /**
         * 缓存的KEY.
         */
        private K key;

        /**
         * 缓存的value.
         */
        private V value;

        /**
         * 开始运行的时间.
         */
        private Long time;

        /**
         * 默认等待的时间.
         */
        private Long expire;

        /**
         * 默认等时间的单位.
         */
        private TimeUnit unit;

        /**
         * 初始化一个对象.
         *
         * @param key    缓存的KEY;
         * @param value  缓存的值 ;
         * @param expire 默认等待时间;
         * @param unit   默认等时间的单位;
         */
        TtlCacheTimerTask(K key, V value, long expire, TimeUnit unit) {
            this.key = key;
            this.value = value;
            time = System.nanoTime();
            this.expire = expire;
            this.unit = unit;
        }

        /**
         * get cache key.
         *
         * @return K.
         */
        public K getKey() {
            return key;
        }

        /**
         * get cache value.
         *
         * @return v.
         */
        public V getValue() {
            return value;
        }

        /**
         * get time.
         *
         * @return long.
         */
        public Long getTime() {
            return time;
        }

        @Override
        public void run(Timeout timeout) {
            if (TtlCache.this.removalListener != null) {
                long elapsed = System.nanoTime() - time;
                long sd = unit.toMillis(expire);
                long ss = TimeUnit.NANOSECONDS.toMillis(elapsed);
                TtlCache.this.removalListener.onRemoval(key, value, sd, ss);
            }
        }
    }

    /**
     * 缓存过期的通知.
     *
     * @author chenbin
     */
    public interface RemovalListener<K, V> {
        /**
         * 通知能数.
         *
         * @param key     key
         * @param value   value
         * @param expire  默认等待时间;
         * @param elapsed 运行了多少时间;
         */
        void onRemoval(K key, V value, Long expire, Long elapsed);
    }
}
