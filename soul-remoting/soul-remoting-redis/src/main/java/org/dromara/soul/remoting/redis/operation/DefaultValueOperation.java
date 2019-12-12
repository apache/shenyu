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

import java.util.concurrent.TimeUnit;
import org.dromara.soul.remoting.redis.RedisConnection;

/**
 * DefaultValueOperation .
 *
 * @author sixh
 */
public class DefaultValueOperation<V> extends BaseOperation<V> implements ValueOperation<String, V> {

    private RedisConnection connection;

    public DefaultValueOperation(RedisConnection connection) {
        this.connection = connection;
    }

    @Override
    public void set(String key, V value) {
        connection.set(rawKey(key), rawValue(value));
    }

    @Override
    public void set(String key, V value, int ttl, TimeUnit unit) {
        connection.setEx(rawKey(key), rawValue(value), ttl, unit);
    }

    @Override
    public void setPx(String key, V value, long ttl, TimeUnit unit) {
        connection.setPx(rawKey(key), rawValue(value), ttl, unit);
    }

    @Override
    public V get(String key, Class<V> clazz) {
        return deserializeValue(connection.get(rawKey(key)), clazz);
    }

    @Override
    public V getAndSet(String key, V value, Class<V> clazz) {
        return deserializeValue(connection.getAndSet(rawKey(key), rawValue(value)), clazz);
    }
}
