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

package org.apache.shenyu.common.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;

import javax.annotation.concurrent.ThreadSafe;
import java.io.Serializable;
import java.util.AbstractMap;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * this cache is provided by caffeine, the cache has two implements including weak-key cache and strong-key cache.<br>
 * <p>the weak-key cache applies to scenarios where objects can be collected. when memory lock causes full gc, the weakKey will collect by gc.</p>
 * <p>the strong-key cache applies to immutable cache object data, and the user determines the cache size.</p>
 * <p>about the weak-key cache and strong-key cache, please refer to:
 * <a href="https://github.com/ben-manes/caffeine/issues/776">caffeine cache ISSUES #776</a></p>
 */
@ThreadSafe
public class WindowTinyLFUMap<K, V> extends AbstractMap<K, V> implements Serializable {
    
    private static final long serialVersionUID = 2176631265536166614L;
    
    private final Cache<K, V> cache;
    
    /**
     * build caffeine cache.
     *
     * @param maximumSize maximumSize
     */
    public WindowTinyLFUMap(final long maximumSize) {
        this.cache = Caffeine.newBuilder()
                .maximumSize(maximumSize)
                .build();
    }
    
    /**
     * initial caffeine cache include WeakReference cache and StrongReference cache.
     *
     * <p>when the weakKey is true that means using weakKeys cache, gc will collect the weak key, please refer to:
     * com.github.benmanes.caffeine.cache.References.WeakKeyReference</p>
     *
     * <p>when the weakKey is false, use strong reference, jvm maybe throw oom-error.</p>
     *
     * <pre>{@code Map<String, Object> strongMap = new WindowTinyLFUMap<>(100, 100, Boolean.FALSE);
     * strongMap.put(new String("abc"), 1);
     * strongMap.put(new String("abc"), 1);
     * assert strongMap.get("abc") != null;
     *
     * Map<String, Object> strongMap = new WindowTinyLFUMap<>(100, 100, Boolean.TRUE);
     * strongMap.put(new String("abc"), 1);
     * strongMap.put(new String("abc"), 1);
     * assert strongMap.get("abc") == null;
     * }</pre>
     *
     * @param initialCapacity initial capacity
     * @param maximumSize maximum size
     * @param weakKey weak key
     */
    public WindowTinyLFUMap(final int initialCapacity, final long maximumSize, final Boolean weakKey) {
        if (Boolean.TRUE.equals(weakKey)) {
            this.cache = Caffeine.newBuilder()
                    .weakKeys()
                    .initialCapacity(initialCapacity)
                    .maximumSize(maximumSize)
                    .build();
        } else {
            this.cache = Caffeine.newBuilder()
                    .initialCapacity(initialCapacity)
                    .maximumSize(maximumSize)
                    .build();
        }
    }
    
    public WindowTinyLFUMap(final int initialSize, final long expireAfterWrite, final long maximumSize, final Boolean weakKey) {
        if (Boolean.TRUE.equals(weakKey)) {
            this.cache = Caffeine.newBuilder()
                    .weakKeys()
                    .initialCapacity(initialSize)
                    .expireAfterWrite(expireAfterWrite, TimeUnit.MILLISECONDS)
                    .maximumSize(maximumSize)
                    .build();
        } else {
            this.cache = Caffeine.newBuilder()
                    .initialCapacity(initialSize)
                    .expireAfterWrite(expireAfterWrite, TimeUnit.MILLISECONDS)
                    .maximumSize(maximumSize)
                    .build();
        }
        
    }
    
    @Override
    public V put(final K key, final V value) {
        V v = cache.getIfPresent(key);
        cache.put(key, value);
        return v;
    }
    
    @Override
    public V get(final Object key) {
        return cache.getIfPresent(key);
    }
    
    @Override
    public V remove(final Object key) {
        V value = cache.getIfPresent(key);
        cache.invalidate(key);
        cache.cleanUp();
        return value;
    }
    
    @Override
    public void clear() {
        this.cache.invalidateAll();
        this.cache.cleanUp();
    }
    
    @Override
    public int size() {
        return this.cache.asMap().entrySet().size();
    }
    
    @Override
    public Set<Entry<K, V>> entrySet() {
        return cache.asMap().entrySet();
    }
}
