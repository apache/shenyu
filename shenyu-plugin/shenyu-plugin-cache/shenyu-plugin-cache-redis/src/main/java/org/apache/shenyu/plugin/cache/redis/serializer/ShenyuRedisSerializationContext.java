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

package org.apache.shenyu.plugin.cache.redis.serializer;

import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

/**
 * ShenyuRedisSerializationContext.
 */
public final class ShenyuRedisSerializationContext {

    private ShenyuRedisSerializationContext() {
    }

    /**
     * string serializer context.
     * @return the string serializer context.
     */
    public static RedisSerializationContext<String, String> stringSerializationContext() {
        RedisSerializer<String> serializer = new StringRedisSerializer();
        return RedisSerializationContext.<String, String>newSerializationContext().key(serializer).value(serializer)
                .hashKey(serializer).hashValue(serializer).build();
    }

    /**
     * bytes serializer context.
     * @return the bytes serializer context.
     */
    public static RedisSerializationContext<String, byte[]> bytesSerializationContext() {
        RedisSerializer<String> serializer = new StringRedisSerializer();
        final ByteArrayRedisSerializer bytesRedisSerializer = new ByteArrayRedisSerializer();
        return RedisSerializationContext.<String, byte[]>newSerializationContext()
                .key(serializer)
                .value(bytesRedisSerializer)
                .hashKey(serializer)
                .hashValue(bytesRedisSerializer)
                .build();
    }
}
