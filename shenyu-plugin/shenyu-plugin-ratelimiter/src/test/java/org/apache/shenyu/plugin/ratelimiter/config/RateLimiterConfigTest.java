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

package org.apache.shenyu.plugin.ratelimiter.config;

import org.apache.shenyu.common.enums.RedisModeEnum;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * RateLimiterConfig default value test.
 */
@RunWith(MockitoJUnitRunner.class)
public final class RateLimiterConfigTest {

    private static final int DEFAULT_MAX_IDLE = 8;

    private static final int DEFAULT_MAX_ACTIVE = 8;

    private static final int DEFAULT_MIN_IDLE = 0;

    private RateLimiterConfig rateLimiterConfig;

    @Before
    public void setUp() {
        this.rateLimiterConfig = new RateLimiterConfig();
    }

    /**
     * mode default value test.
     */
    @Test
    public void modeDefaultValueTest() {
        Assert.assertEquals(RedisModeEnum.STANDALONE.getName(), rateLimiterConfig.getMode());
    }

    /**
     * maxIdle default value test.
     */
    @Test
    public void maxIdleDefaultValueTest() {
        Assert.assertEquals(DEFAULT_MAX_IDLE, rateLimiterConfig.getMaxIdle());
    }

    /**
     * maxActive default value test.
     */
    @Test
    public void maxActiveDefaultValueTest() {
        Assert.assertEquals(DEFAULT_MAX_ACTIVE, rateLimiterConfig.getMaxActive());
    }

    /**
     * minIdle default value test.
     */
    @Test
    public void minIdleDefaultValueTest() {
        Assert.assertEquals(DEFAULT_MIN_IDLE, rateLimiterConfig.getMinIdle());
    }

    /**
     * equals test.
     */
    @Test
    public void equalsTest() {
        RateLimiterConfig defaultConfig = new RateLimiterConfig();
        Assert.assertEquals(defaultConfig, this.rateLimiterConfig);
    }
}
