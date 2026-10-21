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

package org.apache.shenyu.plugin.ai.token.limiter.budget;

import org.apache.commons.codec.digest.DigestUtils;

import java.time.Duration;
import java.util.Objects;

/**
 * Immutable fixed budget window, selected with Redis time and retained through completion.
 */
public final class TokenBudgetWindow {

    static final long MAX_EXACT_INTEGER = 9_007_199_254_740_991L;

    private final String key;

    private final long startMillis;

    private final long endMillis;

    private TokenBudgetWindow(final String key, final long startMillis, final long endMillis) {
        this.key = key;
        this.startMillis = startMillis;
        this.endMillis = endMillis;
    }

    static long validateWindowSize(final String scopeKey, final Duration windowSize) {
        if (Objects.isNull(scopeKey) || scopeKey.isBlank() || Objects.isNull(windowSize)) {
            throw new IllegalArgumentException("Budget scope and window size must be provided");
        }
        final long millis;
        try {
            millis = windowSize.toMillis();
        } catch (ArithmeticException exception) {
            throw new IllegalArgumentException("Window size is out of range", exception);
        }
        if (millis <= 0L || millis > MAX_EXACT_INTEGER || !windowSize.equals(Duration.ofMillis(millis))) {
            throw new IllegalArgumentException("Window size must be a positive exact number of milliseconds");
        }
        return millis;
    }

    static TokenBudgetWindow at(final String scopeKey, final long redisTimeMillis, final long windowMillis) {
        if (redisTimeMillis < 0L || redisTimeMillis > MAX_EXACT_INTEGER || windowMillis <= 0L || windowMillis > MAX_EXACT_INTEGER) {
            throw new IllegalArgumentException("Redis time and window size must be exact nonnegative integers with a positive window");
        }
        final long start = redisTimeMillis - redisTimeMillis % windowMillis;
        if (windowMillis > MAX_EXACT_INTEGER - start) {
            throw new IllegalArgumentException("Window end exceeds the exact Lua integer range");
        }
        final String key = scopePrefix(scopeKey) + ":" + windowMillis + ":" + start;
        return new TokenBudgetWindow(key, start, start + windowMillis);
    }

    static String scopePrefix(final String scopeKey) {
        // Hashing prevents user-supplied braces from changing the Redis Cluster hash tag.
        return "SHENYU:AI:TOKENLIMIT:RESERVATION:{" + DigestUtils.sha256Hex(scopeKey) + "}";
    }

    /**
     * Get the scope/window Redis hash key.
     *
     * @return immutable key retained for all reservation completion operations
     */
    public String getKey() {
        return key;
    }

    /**
     * Get the inclusive window start in epoch milliseconds.
     *
     * @return inclusive start
     */
    public long getStartMillis() {
        return startMillis;
    }

    /**
     * Get the exclusive window end in epoch milliseconds.
     *
     * @return exclusive end
     */
    public long getEndMillis() {
        return endMillis;
    }
}
