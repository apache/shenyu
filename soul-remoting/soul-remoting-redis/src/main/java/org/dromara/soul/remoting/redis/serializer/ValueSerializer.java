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

package org.dromara.soul.remoting.redis.serializer;


import java.nio.charset.StandardCharsets;

/**
 * ValueSerializer
 *
 * @author sixh
 */
public class ValueSerializer<V> implements Serializer<V> {

    private Serializer<V> objectSerializer;

    public ValueSerializer(Serializer<V> objectSerializer) {
        this.objectSerializer = objectSerializer;
    }

    @Override
    public V deserialize(byte[] value, Class<V> clazz) {
        if (clazz.getName().equals(String.class.getName())) {
            return (V) new String(value, StandardCharsets.UTF_8);
        }else {
            return objectSerializer.deserialize(value, clazz);
        }
    }

    @Override
    public byte[] serialize(V value) {
        if (value == null) {
            return new byte[0];
        }
        if (value instanceof String) {
            return ((String) value).getBytes();
        } else {
            return objectSerializer.serialize(value);
        }
    }
}
