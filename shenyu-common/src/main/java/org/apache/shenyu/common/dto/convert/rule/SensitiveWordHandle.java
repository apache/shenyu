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
     * new default instance.
     *
     * @return the default handle
     */
    public static SensitiveWordHandle newDefaultInstance() {
        return new SensitiveWordHandle();
    }
}
