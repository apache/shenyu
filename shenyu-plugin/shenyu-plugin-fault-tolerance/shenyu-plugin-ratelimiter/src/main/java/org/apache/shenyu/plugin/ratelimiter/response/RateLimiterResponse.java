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

package org.apache.shenyu.plugin.ratelimiter.response;

import java.io.Serializable;
import java.util.List;

/**
 * rateLimiter response.
 */
public class RateLimiterResponse implements Serializable {

    private static final long serialVersionUID = 2896271355629004718L;

    private final boolean allowed;

    private final long tokensRemaining;

    private final List<String> keys;

    private final boolean backendFailure;

    /**
     * Instantiates a new Rate limiter response.
     *
     * @param allowed         the allowed
     * @param tokensRemaining the tokens remaining
     * @param keys the redis keys
     */
    public RateLimiterResponse(final boolean allowed, final long tokensRemaining, final List<String> keys) {
        this(allowed, tokensRemaining, keys, false);
    }

    /**
     * Instantiates a new Rate limiter response.
     *
     * @param allowed         the allowed
     * @param tokensRemaining the tokens remaining
     * @param keys the redis keys
     * @param backendFailure whether the Redis backend failed
     */
    public RateLimiterResponse(final boolean allowed, final long tokensRemaining, final List<String> keys, final boolean backendFailure) {
        this.allowed = allowed;
        this.tokensRemaining = tokensRemaining;
        this.keys = keys;
        this.backendFailure = backendFailure;
    }

    /**
     * Is allowed boolean.
     *
     * @return the boolean
     */
    public boolean isAllowed() {
        return allowed;
    }

    /**
     * Gets tokens remaining.
     *
     * @return the tokens remaining
     */
    public long getTokensRemaining() {
        return tokensRemaining;
    }


    /**
     * get redis keys.
     *
     * @return getKeys
     */
    public List<String> getKeys() {
        return keys;
    }

    /**
     * whether the Redis backend failed while resolving the decision.
     *
     * @return true when the decision came from the backend-failure fallback
     */
    public boolean isBackendFailure() {
        return backendFailure;
    }

    @Override
    public String toString() {
        return "Response{" + "allowed=" + allowed + ", tokensRemaining=" + tokensRemaining + ", backendFailure=" + backendFailure + '}';
    }
}
