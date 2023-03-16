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

@ThreadSafe
public class WindowTinyLFUMap<K, V> extends AbstractMap<K, V> implements Serializable {
    
    private static final long serialVersionUID = 2176631265536166614L;
    
    private final Cache<K, V> cache;
    
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
        V v = cache.getIfPresent(value);
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
    public Set<Entry<K, V>> entrySet() {
        return cache.asMap().entrySet();
    }
}
