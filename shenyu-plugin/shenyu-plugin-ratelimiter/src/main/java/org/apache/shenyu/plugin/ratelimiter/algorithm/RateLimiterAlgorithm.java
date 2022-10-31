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

package org.apache.shenyu.plugin.ratelimiter.algorithm;

import org.apache.shenyu.spi.SPI;
import org.springframework.data.redis.core.script.RedisScript;

import java.util.List;

/**
 * The interface Rate limiter algorithm.
 *
 * @param <T> the type parameter
 */
@SPI
public interface RateLimiterAlgorithm<T> {

    /**
     * Gets script name.
     *
     * @return the script name
     */
    String getScriptName();

    /**
     * Gets script.
     *
     * @return the script
     */
    RedisScript<T> getScript();

    /**
     * Gets keys.
     *
     * @param id the id
     * @return the keys
     */
    List<String> getKeys(String id);

    /**
     * Callback string.
     *
     * @param script     the script
     * @param keys       the keys
     * @param scriptArgs the script args
     */
    default void callback(final RedisScript<?> script, final List<String> keys, final List<String> scriptArgs) {
    }
}
