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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Objects;

/**
 * The sensitive word rule handle, it tells the plugin where the dictionary lives and how long a
 * loaded dictionary may be reused.
 */
public class SensitiveWordHandle {

    /**
     * The default redis key holding the sensitive word set.
     */
    public static final String DEFAULT_REDIS_KEY = "shenyu:sensitive:words";

    /**
     * The redis key of the sensitive word set, every rule may point to its own dictionary.
     */
    private String redisKey = DEFAULT_REDIS_KEY;

    /**
     * How long, in seconds, a loaded dictionary is reused before it is read from redis again.
     * Zero or a negative value reads the dictionary on every request.
     */
    private long refreshIntervalSeconds = 300L;

    /**
     * Whether the request must be rejected when the dictionary is unavailable. It defaults to
     * false, which means such a request is passed through: a broken redis must not take the
     * traffic down. Deployments with a hard compliance requirement can opt into blocking.
     */
    private boolean failClosed;

    /**
     * The words configured on the rule itself, separated by commas or by newlines. They are merged
     * with the dictionary read from redis, so a small list is easier to keep next to the rule and a
     * large one stays outside of shenyu.
     */
    private String words;

    /**
     * The largest request body that is scanned, in bytes. A body above it is not buffered: it is
     * passed through unscanned, or rejected when {@link #failClosed} is set. Zero scans every body.
     */
    private long maxBodySize;

    /**
     * get redis key.
     *
     * @return redis key
     */
    public String getRedisKey() {
        return redisKey;
    }

    /**
     * set redis key.
     *
     * @param redisKey redis key
     */
    public void setRedisKey(final String redisKey) {
        this.redisKey = redisKey;
    }

    /**
     * get refresh interval seconds.
     *
     * @return refresh interval in seconds
     */
    public long getRefreshIntervalSeconds() {
        return refreshIntervalSeconds;
    }

    /**
     * set refresh interval seconds.
     *
     * @param refreshIntervalSeconds refresh interval in seconds
     */
    public void setRefreshIntervalSeconds(final long refreshIntervalSeconds) {
        this.refreshIntervalSeconds = refreshIntervalSeconds;
    }

    /**
     * whether the request must be rejected when the dictionary is unavailable.
     *
     * @return true when the request must be rejected
     */
    public boolean isFailClosed() {
        return failClosed;
    }

    /**
     * set whether the request must be rejected when the dictionary is unavailable.
     *
     * @param failClosed true to reject the request
     */
    public void setFailClosed(final boolean failClosed) {
        this.failClosed = failClosed;
    }

    /**
     * get the words configured on the rule.
     *
     * @return the words, separated by commas or by newlines
     */
    public String getWords() {
        return words;
    }

    /**
     * set the words configured on the rule.
     *
     * @param words the words, separated by commas or by newlines
     */
    public void setWords(final String words) {
        this.words = words;
    }

    /**
     * get the largest request body that is scanned.
     *
     * @return the maximum body size in bytes, zero means unlimited
     */
    public long getMaxBodySize() {
        return maxBodySize;
    }

    /**
     * set the largest request body that is scanned.
     *
     * @param maxBodySize the maximum body size in bytes, zero means unlimited
     */
    public void setMaxBodySize(final long maxBodySize) {
        this.maxBodySize = maxBodySize;
    }

    /**
     * The cache key of the automaton this rule scans with: the redis key plus the words of the rule,
     * so that two rules reading the same redis set but adding different words never share one
     * automaton, and so that a changed word list is compiled again.
     *
     * @return the cache key of the dictionary
     */
    public String dictionaryKey() {
        String key = Objects.isNull(redisKey) || redisKey.trim().isEmpty() ? DEFAULT_REDIS_KEY : redisKey;
        return Objects.isNull(words) || words.trim().isEmpty() ? key : key + '|' + words.trim();
    }

    /**
     * new default instance.
     *
     * @return the default handle
     */
    public static SensitiveWordHandle newDefaultInstance() {
        return new SensitiveWordHandle();
    }
}
