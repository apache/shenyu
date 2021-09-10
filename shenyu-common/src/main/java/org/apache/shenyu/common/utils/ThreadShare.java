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

import java.util.Objects;

/**
 * the thread share.
 */
public class ThreadShare<T> extends ThreadLocal<T> {

    /**
     * share thread.
     */
    private final Thread thread = Thread.currentThread();

    /**
     * the name.
     */
    private final String name;

    public ThreadShare() {
        this.name = "default-share";
    }

    /**
     * create thread share by name.
     *
     * @param name the name
     */
    public ThreadShare(final String name) {
        this.name = name;
    }

    /**
     * create the share by name.
     *
     * @param name the name
     * @param <T> the type
     * @return thread share object
     */
    public static <T> ThreadShare<T> create(final String name) {
        return new ThreadShare<>(name);
    }

    /**
     * get and remove the shared data.
     *
     * @return the stored data
     */
    public T getRemove() {
        try {
            return this.get();
        } finally {
            this.remove();
        }
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        ThreadShare<?> that = (ThreadShare<?>) o;
        return Objects.equals(thread, that.thread)
                && Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(thread, name);
    }
}
