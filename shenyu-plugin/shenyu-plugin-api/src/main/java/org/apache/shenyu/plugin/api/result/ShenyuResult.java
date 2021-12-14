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

package org.apache.shenyu.plugin.api.result;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The interface shenyu result.
 */
public abstract class ShenyuResult<T> extends ConcurrentHashMap<String, Object> {

    /**
     * Success t.
     *
     * @param code    the code
     * @param message the message
     * @param object  the object
     * @return the t
     */
    public abstract T success(int code, String message, Object object);

    /**
     * Error t.
     *
     * @param code    the code
     * @param message the message
     * @param object  the object
     * @return the t
     */
    public abstract T error(int code, String message, Object object);

    /**
     * put all data and skip the null data.
     *
     * @param m the putting data
     */
    @Override
    public void putAll(final Map<? extends String, ?> m) {
        Optional.ofNullable(m).ifPresent(map -> {
            final Object[] value = {new AtomicReference<>()};
            map.keySet().stream().filter(Objects::nonNull).forEach(key -> {
                if (Objects.nonNull(value[0] = m.get(key))) {
                    put(key, value[0]);
                }
            });
        });
    }
}
