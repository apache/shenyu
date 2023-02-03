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
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public class MapUtils {

    /**
     * Transform to string map.
     *
     * @param map source map
     * @return string map
     */
    public static Map<String, String> transStringMap(final Map<String, Object> map) {
        return Optional.ofNullable(map)
                .map(m -> m.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> Objects.toString(e.getValue(), null))))
                .orElse(null);
    }

    /**
     * This is jdk8 performance bug, see: https://bugs.openjdk.java.net/browse/JDK-8161372.
     *
     * @param map source map
     * @param key key
     * @param mappingFunction mappingFunction
     * @param <K> k
     * @param <V> v
     * @return v
     */
    public static <K, V> V computeIfAbsent(final Map<K, V> map, final K key, final Function<? super K, ? extends V> mappingFunction) {
        V v = map.get(key);
        if (v != null) {
            return v;
        }
        return map.computeIfAbsent(key, mappingFunction);
    }
}
