/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.common.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * AttrMap .
 * 1. About some properties map collection processing.
 * 2. You can use the current method to get the value of the property you wan.
 *
 * @author sixh
 */
public class AttrMap<K, V> extends HashMap<K, V> {

    public AttrMap() {
    }

    public AttrMap(Map<? extends K, ? extends V> m) {
        super(m);
    }

    private boolean checkExist(String key) {
        return this.containsKey(key) && (this.get(key) != null);
    }

    /**
     * Gets the int of the key value.
     *
     * @param parameterKey String KEY
     * @return Value, it is possible to return null.
     */
    public Optional<Integer> getInt(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of((Integer) this.get(parameterKey));
        }
        return Optional.empty();
    }

    /**
     * Gets the key value of type long.
     *
     * @param parameterKey String KEY
     * @return Value, it is possible to return null.
     */
    public Optional<Long> getLong(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of((Long) this.get(parameterKey));
        }
        return Optional.empty();
    }

    /**
     * Gets the Object type of the key value.
     *
     * @param parameterKey String KEY
     * @return Value, it is possible to return null.
     */
    public Optional<Object> getObject(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of(this.get(parameterKey));
        }
        return Optional.empty();
    }

    /**
     * Gets the Object type of the key value.
     *
     * @param parameterKey String KEY.
     * @return Value, it is possible to return null.
     */
    public Optional<Map<String, Object>> getMap(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of((Map<String, Object>) this.get(parameterKey));
        }
        return Optional.empty();
    }

    /**
     * Gets the Bool type of the key value.
     *
     * @param parameterKey String KEY.
     * @return Value, it is possible to return null.
     */
    public Optional<Boolean> getBool(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of((Boolean) this.get(parameterKey));
        }
        return Optional.empty();
    }

    /**
     * Gets the string type of the key value.
     *
     * @param parameterKey String KEY
     * @return Value, it is possible to return null.
     */
    public Optional<String> getString(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of(this.get(parameterKey).toString());
        }
        return Optional.empty();
    }

    /**
     * Gets the dubbo type of this key value.
     *
     * @param parameterKey String KEY
     * @return Value, it is possible to return null.
     */
    public Optional<Double> getDouble(String parameterKey) {
        if (checkExist(parameterKey)) {
            return Optional.of((Double) this.get(parameterKey));
        }
        return Optional.empty();
    }
}
