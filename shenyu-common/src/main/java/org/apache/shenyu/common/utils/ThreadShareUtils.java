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

package org.apache.shenyu.common.utils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * the thread share utils.
 */
public final class ThreadShareUtils {

    /**
     * the default share.
     */
    private static final String DEFAULT_SHARE = "default-thread-share";

    /**
     * the thread share map.
     */
    private static final Map<ThreadShare<Object>, Object> SHARE_MAPPING = new ConcurrentHashMap<>();

    /**
     * get a static instance by name.
     *
     * @param name the name
     * @return static instance
     */
    private static ThreadShare<Object> getShareByName(final String name) {
        return new ThreadShare<>(name);
    }

    /**
     * put data to thread share.
     *
     * @param data the data
     * @param <T> the data type
     */
    public static <T> void put(final T data) {
        put(DEFAULT_SHARE, data);
    }

    /**
     * put the data to thread share.
     *
     * @param name the name
     * @param data the data
     * @param <T> the data type
     */
    public static <T> void put(final String name, final T data) {
        SHARE_MAPPING.put(getShareByName(name), data);
    }

    /**
     * get the thread shared data.
     *
     * @param <T> the data type
     * @return the data
     */
    public static <T> T get() {
        return get(DEFAULT_SHARE);
    }

    /**
     * get share data.
     *
     * @param name the name
     * @param <T> the data type
     * @return the data
     */
    public static <T> T get(final String name) {
        final ThreadShare<Object> share = getShareByName(name);
        if (!SHARE_MAPPING.containsKey(share)) {
            return null;
        }
        final Object data = SHARE_MAPPING.get(share);
        return (T) data;
    }

    /**
     * get and remove the thread shared data.
     *
     * @param <T> the data type
     * @return the data
     */
    public static <T> T getRemove() {
        return getRemove(DEFAULT_SHARE);
    }

    /**
     * get and remove the thread shared data.
     *
     * @param name the name
     * @param <T> the data type
     * @return the data
     */
    public static <T> T getRemove(final String name) {
        final ThreadShare<Object> share = getShareByName(name);
        if (!SHARE_MAPPING.containsKey(share)) {
            return null;
        }
        final Object data = SHARE_MAPPING.remove(share);
        return (T) data;
    }
}
