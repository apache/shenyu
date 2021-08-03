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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class RateLimitEnumTest {
    @Test
    public void testGetKeyName() {
        assertEquals("concurrent_request_rate_limiter", RateLimitEnum.CONCURRENT.getKeyName());
        assertEquals("request_leaky_rate_limiter", RateLimitEnum.LEAKY_BUCKET.getKeyName());
        assertEquals("sliding_window_request_rate_limiter", RateLimitEnum.SLIDING_WINDOW.getKeyName());
        assertEquals("request_rate_limiter", RateLimitEnum.TOKEN_BUCKET.getKeyName());
    }

    @Test
    public void testGetScriptName() {
        assertEquals("concurrent_request_rate_limiter.lua", RateLimitEnum.CONCURRENT.getScriptName());
        assertEquals("request_leaky_rate_limiter.lua", RateLimitEnum.LEAKY_BUCKET.getScriptName());
        assertEquals("sliding_window_request_rate_limiter.lua", RateLimitEnum.SLIDING_WINDOW.getScriptName());
        assertEquals("request_rate_limiter.lua", RateLimitEnum.TOKEN_BUCKET.getScriptName());
    }
}
