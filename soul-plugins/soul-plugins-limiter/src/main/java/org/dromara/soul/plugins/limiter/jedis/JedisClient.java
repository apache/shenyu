/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.plugins.limiter.jedis;

import java.util.List;
import java.util.Set;

/**
 * JedisClient.
 *
 * @author xiaoyu(Myth)
 */
public interface JedisClient {


    /**
     * Evalsha object.
     *
     * @param script the script
     * @param keys   the keys
     * @param args   the args
     * @return the object
     */
    Object evalsha(final String script, final List<String> keys, final List<String> args);

    /**
     * Set string.
     *
     * @param key   the key
     * @param value the value
     * @return the string
     */
    String set(String key, String value);

    /**
     * Set string.
     *
     * @param key   the key
     * @param value the value
     * @return the string
     */
    String set(String key, byte[] value);

    /**
     * Del long.
     *
     * @param keys the keys
     * @return the long
     */
    Long del(String... keys);

    /**
     * Get string.
     *
     * @param key the key
     * @return the string
     */
    String get(String key);

    /**
     * Get byte [ ].
     *
     * @param key the key
     * @return the byte [ ]
     */
    byte[] get(byte[] key);

    /**
     * Keys set.
     *
     * @param pattern the pattern
     * @return the set
     */
    Set<byte[]> keys(byte[] pattern);

    /**
     * Keys set.
     *
     * @param key the key
     * @return the set
     */
    Set<String> keys(String key);

    /**
     * Hset long.
     *
     * @param key   the key
     * @param item  the item
     * @param value the value
     * @return the long
     */
    Long hset(String key, String item, String value);

    /**
     * Hget string.
     *
     * @param key  the key
     * @param item the item
     * @return the string
     */
    String hget(String key, String item);

    /**
     * Hdel long.
     *
     * @param key  the key
     * @param item the item
     * @return the long
     */
    Long hdel(String key, String item);

    /**
     * Incr long.
     *
     * @param key the key
     * @return the long
     */
    Long incr(String key);

    /**
     * Decr long.
     *
     * @param key the key
     * @return the long
     */
    Long decr(String key);

    /**
     * Expire long.
     *
     * @param key    the key
     * @param second the second
     * @return the long
     */
    Long expire(String key, int second);

    /**
     * Zrange set.
     *
     * @param key   the key
     * @param start the start
     * @param end   the end
     * @return the set
     */
    Set<String> zrange(String key, long start, long end);

}
