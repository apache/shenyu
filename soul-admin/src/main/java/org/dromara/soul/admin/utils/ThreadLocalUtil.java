package org.dromara.soul.admin.utils;

import java.util.HashMap;
import java.util.Map;

/*
 * Copyright 2007-present the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
public class ThreadLocalUtil {
    private static final ThreadLocal<Map<String, Object>> THREAD_CONTEXT = new ThreadLocal<>();

    /**
     * save thread variable.
     *
     * @param key   put key
     * @param value put value
     */
    public static void put(final String key, final Object value) {
        Map<String, Object> threadBidMap = THREAD_CONTEXT.get();
        if (threadBidMap == null) {
            threadBidMap = new HashMap<>();
            THREAD_CONTEXT.set(threadBidMap);
        }
        threadBidMap.put(key, value);
    }

    /**
     * remove thread variable.
     *
     * @param key remove key
     */
    public static void remove(final String key) {
        Map<String, Object> threadBidMap = THREAD_CONTEXT.get();
        if (threadBidMap != null) {
            threadBidMap.remove(key);
        }
    }

    /**
     * get thread variables.
     *
     * @param key get key
     * @return the Object
     */
    public static Object get(final String key) {
        Map<String, Object> threadBidMap = THREAD_CONTEXT.get();
        return threadBidMap != null ? threadBidMap.get(key) : null;
    }

    /**
     * remove all variables.
     */
    public static void clear() {
        THREAD_CONTEXT.remove();
    }
}
