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

/**
 * RedisOperation .
 * Redis related operation implementation type.
 *
 * @param <K> the type parameter
 * @param <V> the type parameter
 * @author sixh
 */
public interface RedisOperation<K, V> {

    /**
     * Map operation.
     *
     * @return the map operation.
     */
    MapOperation<K, V> mapOperation();

    /**
     * List operation.
     *
     * @return the list operation.
     */
    ListOperation<K, V> listOperation();

    /**
     * Sets operation.
     *
     * @return the operation
     */
    SetOperation<K, V> setOperation();

    /**
     * Value operation.
     *
     * @return the value operation.
     */
    ValueOperation<K, V> valueOperation();

    /**
     * Zset operation.
     *
     * @return the zset operation.
     */
    ZsetOperation<K, V> zsetOperation();

    /**
     * Script operation.
     *
     * @return the script operation.
     */
    ScriptOperation<K, V> scriptOperation();
}

