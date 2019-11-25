/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.remoting.redis.operation;

import org.dromara.soul.remoting.redis.serializer.ObjectSerializer;
import org.dromara.soul.remoting.redis.serializer.Serializer;
import org.dromara.soul.remoting.redis.serializer.ValueSerializer;

/**
 * BaseOperation .
 * <p>
 * 2019/11/23
 *
 * @param <V> the type parameter
 * @author sixh
 */
public class BaseOperation<V> {

    /**
     * value serializer.
     */
    private Serializer<V> valueSerializer;

    public BaseOperation() {
        this.valueSerializer = new ValueSerializer<>(new ObjectSerializer<>());
    }

    /**
     * Raw value byte [ ].
     *
     * @param value the value
     * @return the byte [ ]
     */
    byte[] rawValue(V value) {
        if (valueSerializer == null && value instanceof byte[]) {
            return (byte[]) value;
        }
        return valueSerializer.serialize(value);
    }

    /**
     * Raw key byte [ ].
     *
     * @param key the key
     * @return the byte [ ]
     */
    byte[] rawKey(String key) {
        return key.getBytes();
    }

    /**
     * Deserialize value v.
     *
     * @param bytes the bytes
     * @param clazz the clazz
     * @return the v
     */
    V deserializeValue(byte[] bytes, Class<V> clazz) {
        return valueSerializer.deserialize(bytes, clazz);
    }
}
