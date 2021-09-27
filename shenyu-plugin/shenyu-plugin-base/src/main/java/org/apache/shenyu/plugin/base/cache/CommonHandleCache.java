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

package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.plugin.api.HandleCache;

import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The selector or rule handle base cache.
 */
public class CommonHandleCache<K, V> implements HandleCache<K, V> {

    /**
     * selectorId.ruleName -> handle.
     */
    private final ConcurrentHashMap<K, V> cached = new ConcurrentHashMap<>();

    @Override
    public V obtainHandle(final K key) {
        return cached.get(key);
    }

    @Override
    public void cachedHandle(final K key, final V value) {
        Optional.ofNullable(key).ifPresent(data -> cached.put(key, value));
    }

    @Override
    public void removeHandle(final K key) {
        Optional.ofNullable(key).ifPresent(cached::remove);
    }
}


