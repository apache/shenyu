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

import org.apache.shenyu.common.concurrent.MemoryLimitCalculator;

import java.util.Map;

/**
 * The only difference between this class and {@link org.apache.shenyu.common.cache.ConcurrentLinkedHashMap}
 * is that it handles memory issues via {@link org.apache.shenyu.common.concurrent.MemoryLimitCalculator}.
 */
public class MemorySafeLRUMap<K, V> extends ConcurrentLinkedHashMap<K, V> {
    
    public MemorySafeLRUMap(final int maxFreeMemory,
                            final Map<? extends K, ? extends V> map) {
        this(maxFreeMemory, map.size());
    }
    
    public MemorySafeLRUMap(final int maxFreeMemory,
                            final int initialSize) {
        this(maxFreeMemory, initialSize, 0.75f);
    }
    
    public MemorySafeLRUMap(final int maxFreeMemory,
                            final int initialSize,
                            final float loadFactor) {
        this(initialSize, loadFactor, (weightedSize, capacity, size) -> {
            // when free memory less than certain value, consider it's full
            return size > 0 && MemoryLimitCalculator.maxAvailable() < maxFreeMemory;
        });
    }
    
    @SuppressWarnings("unchecked")
    public MemorySafeLRUMap(final int initialSize,
                            final float loadFactor,
                            final OverflowChecker checker) {
        super(16, initialSize, MAXIMUM_CAPACITY,
                loadFactor, checker,
                (EvictionListener<K, V>) DiscardingListener.INSTANCE,
                Weighers.entrySingleton());
    }
}
