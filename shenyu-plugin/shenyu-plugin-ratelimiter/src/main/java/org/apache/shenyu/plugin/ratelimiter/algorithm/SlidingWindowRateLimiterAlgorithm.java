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

import org.apache.shenyu.common.enums.RateLimitEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.spi.Join;

import java.util.Arrays;
import java.util.List;

/**
 * The type Sliding window rate limiter algorithm.
 */
@Join
public class SlidingWindowRateLimiterAlgorithm extends AbstractRateLimiterAlgorithm {

    public SlidingWindowRateLimiterAlgorithm() {
        super(RateLimitEnum.SLIDING_WINDOW.getScriptName());
    }

    @Override
    protected String getKeyName() {
        return RateLimitEnum.SLIDING_WINDOW.getKeyName();
    }

    @Override
    public List<String> getKeys(final String id) {
        String prefix = getKeyName() + ".{" + id;
        String tokenKey = prefix + "}.tokens";
        String timestampKey = UUIDUtils.getInstance().generateShortUuid();
        return Arrays.asList(tokenKey, timestampKey);
    }
}
