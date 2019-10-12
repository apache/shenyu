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

package org.dromara.soul.remoting.api;


import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import org.dromara.soul.common.cache.Timeout;
import org.dromara.soul.common.cache.TtlCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 用于记录一个通道保存处理.
 *
 * @author chenbin
 */
public final class ChannelCache extends TtlCache<String, Channel> implements TtlCache.RemovalListener<String, Channel> {
    /**
     * The constant LOG.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ChannelCache.class);

    /**
     * 通过保存缓存.
     */
    private final Map<String, Timeout> CACHES;

    private ChannelCacheListener listener;

    /**
     * 生成一个过期缓存对象.
     *
     * @param expire    执行；
     * @param unit      时间单位；
     * @param cacheName 设置一个缓存的名字；
     */
    public ChannelCache(long expire,
                        TimeUnit unit,
                        String cacheName,
                        ChannelCacheListener listener) {
        super(expire, unit, cacheName);
        CACHES = new ConcurrentHashMap<>();
        this.addRemovalListener(this);
        this.listener = listener;
    }

    /**
     * Put timeout.
     *
     * @param s       the s
     * @param channel the channel
     * @return the timeout
     */
    @Override
    public Timeout put(String s, Channel channel) {
        /*
         * 返回一个数据处理器;
         */
        Timeout timeout = super.put(s, channel);
        CACHES.put(s, timeout);
        return timeout;
    }

    /**
     * Put timeout.
     *
     * @param s       the s
     * @param channel the channel
     * @param expire  the expire
     * @param unit    the unit
     * @return the timeout
     */
    @Override
    public Timeout put(String s, Channel channel, long expire, TimeUnit unit) {
        Timeout timeout = super.put(s, channel, expire, unit);
        CACHES.put(s, timeout);
        return timeout;
    }

    /**
     * 获取数据.
     *
     * @param key key;
     * @return 返回时间序列 ;
     */
    @SuppressWarnings("unchecked")
    Channel get(String key) {
        Timeout timeout = CACHES.get(key);
        if (timeout != null && !timeout.isDefault()) {
            Node node = (Node) timeout.task();
            String sKey = node.getKey();
            if (sKey.equals(key)) {
                return node.getValue();
            }
        }
        return null;
    }

    /**
     * 移除数据.
     *
     * @param key cache key;
     */
    public void remove(String key) {
        try {
            Timeout timeout = CACHES.get(key);
            if (timeout != null) {
                CACHES.remove(key);
                timeout.cancel();
            }
        } catch (Exception e) {
            LOG.error("移除关闭一个通道失败!", e);
        }

    }

    /**
     * 调用了这个方法则认为是进行超时处理了.
     *
     * @param key     key
     * @param event   channel超时了;
     * @param expire  默认是需要等待多少毫秒
     * @param elapsed 等待了多少毫秒
     */
    @Override
    public void onRemoval(String key, Channel event, Long expire, Long elapsed) {
        CACHES.remove(key);
        try {
            listener.timeout(event);
        } catch (Exception e) {
            LOG.error("通道超时处理失败!", e);
        }
    }

    @SuppressWarnings("unchecked")
    public Collection<Channel> getAll() {
        return CACHES.values().stream().map(e -> ((Node) e.task()).getValue()).collect(Collectors.toList());
    }
}
