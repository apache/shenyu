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

package org.apache.shenyu.common.enums;

/**
 * rate limit.
 */
public enum RateLimitEnum {

    SLIDING_WINDOW("sliding_window_request_rate_limiter", "sliding_window_request_rate_limiter.lua"),

    LEAKY_BUCKET("request_leaky_rate_limiter", "request_leaky_rate_limiter.lua"),

    CONCURRENT("concurrent_request_rate_limiter", "concurrent_request_rate_limiter.lua"),

    TOKEN_BUCKET("request_rate_limiter", "request_rate_limiter.lua");

    private final String keyName;

    private final String scriptName;

    RateLimitEnum(final String keyName, final String scriptName) {
        this.keyName = keyName;
        this.scriptName = scriptName;
    }

    /**
     * getKeyName.
     *
     * @return keyName
     */
    public String getKeyName() {
        return this.keyName;
    }

    /**
     * getScriptName.
     *
     * @return scriptName
     */
    public String getScriptName() {
        return this.scriptName;
    }
}
