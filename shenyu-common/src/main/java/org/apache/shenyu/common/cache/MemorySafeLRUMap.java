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

import org.apache.commons.collections4.map.LRUMap;
import org.apache.shenyu.common.concurrent.MemoryLimitCalculator;

import javax.annotation.concurrent.NotThreadSafe;
import java.util.Map;

/**
 * The only difference between this class and {@link org.apache.commons.collections4.map.LRUMap}
 * is that it handles memory issues via {@link org.apache.shenyu.common.concurrent.MemoryLimitCalculator}.
 *
 * <p>Note: this class is not thread safe! see https://github.com/apache/shenyu/issues/3929
 */
@NotThreadSafe
public class MemorySafeLRUMap<K, V> extends LRUMap<K, V> {

    private final int maxFreeMemory;

    public MemorySafeLRUMap(final int maxFreeMemory,
                            final int initialSize) {
        super(MAXIMUM_CAPACITY, initialSize);
        this.maxFreeMemory = maxFreeMemory;
    }

    public MemorySafeLRUMap(final int maxFreeMemory,
                            final int initialSize,
                            final float loadFactor) {
        super(MAXIMUM_CAPACITY, initialSize, loadFactor);
        this.maxFreeMemory = maxFreeMemory;
    }

    public MemorySafeLRUMap(final int maxFreeMemory,
                            final int initialSize,
                            final float loadFactor,
                            final boolean scanUntilRemovable) {
        super(MAXIMUM_CAPACITY, initialSize, loadFactor, scanUntilRemovable);
        this.maxFreeMemory = maxFreeMemory;
    }

    public MemorySafeLRUMap(final int maxFreeMemory,
                            final Map<? extends K, ? extends V> map) {
        super(map);
        this.maxFreeMemory = maxFreeMemory;
    }

    public MemorySafeLRUMap(final int maxFreeMemory,
                            final Map<? extends K, ? extends V> map,
                            final boolean scanUntilRemovable) {
        super(map, scanUntilRemovable);
        this.maxFreeMemory = maxFreeMemory;
    }

    @Override
    public boolean isFull() {
        // when free memory less than certain value, consider it's full
        return size() > 0 && MemoryLimitCalculator.maxAvailable() < maxFreeMemory;
    }
}
