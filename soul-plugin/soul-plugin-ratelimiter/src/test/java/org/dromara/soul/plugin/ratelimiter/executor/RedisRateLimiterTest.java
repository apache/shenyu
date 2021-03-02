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

package org.dromara.soul.plugin.ratelimiter.executor;

import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.convert.RateLimiterHandle;
import org.dromara.soul.plugin.base.utils.Singleton;
import org.dromara.soul.plugin.ratelimiter.response.RateLimiterResponse;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * RedisRateLimiter test.
 *
 * @author wyc192273
 */
@RunWith(MockitoJUnitRunner.class)
@Slf4j
public final class RedisRateLimiterTest {

    private static final String DEFAULT_TEST_ID = "testId";

    private static final double DEFAULT_TEST_REPLENISH_RATE = 1.0;

    private static final double DEFAULT_TEST_BURST_CAPACITY = 300.0;

    private RedisRateLimiter redisRateLimiter;
    
    private RateLimiterHandle rateLimiterHandle;

    @Before
    public void setUp() {
        this.redisRateLimiter = new RedisRateLimiter();
        rateLimiterHandle = new RateLimiterHandle();
        rateLimiterHandle.setAlgorithmName("tokenBucket");
        rateLimiterHandle.setReplenishRate(DEFAULT_TEST_REPLENISH_RATE);
        rateLimiterHandle.setBurstCapacity(DEFAULT_TEST_BURST_CAPACITY);
    }

    /**
     * redisRateLimiter.isAllowed allowed case.
     */
    @Test
    public void allowedTest() {
        isAllowedPreInit(1L, 1L, false);
        Mono<RateLimiterResponse> responseMono = redisRateLimiter.isAllowed(DEFAULT_TEST_ID, rateLimiterHandle);
        StepVerifier.create(responseMono).assertNext(r -> {
            Assert.assertEquals(1L, r.getTokensRemaining());
            Assert.assertTrue(r.isAllowed());
        }).verifyComplete();
    }

    /**
     * redisRateLimiter.isAllowed not allowed case.
     */
    @Test
    public void notAllowedTest() {
        isAllowedPreInit(0L, 0L, false);
        Mono<RateLimiterResponse> responseMono = redisRateLimiter.isAllowed(DEFAULT_TEST_ID, rateLimiterHandle);
        StepVerifier.create(responseMono).assertNext(r -> {
            Assert.assertEquals(0, r.getTokensRemaining());
            Assert.assertFalse(r.isAllowed());
        }).verifyComplete();
    }

    /**
     * redisRateLimiter.isAllowed exception case.
     */
    @Test
    public void allowedThrowableTest() {
        isAllowedPreInit(0, 0, true);
        Mono<RateLimiterResponse> responseMono = redisRateLimiter.isAllowed(DEFAULT_TEST_ID, rateLimiterHandle);
        StepVerifier.create(responseMono).assertNext(r -> {
            Assert.assertEquals(-1, r.getTokensRemaining());
            Assert.assertTrue(r.isAllowed());
        }).verifyComplete();
    }

    /**
     * redisRateLimiter.isAllowed test pre init.
     *
     * @param allowedNum         mock lua allowedNum result
     * @param newTokens          mock lua newTokens result
     * @param needThrowException mock lua throw exception
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    private void isAllowedPreInit(final long allowedNum, final long newTokens, final boolean needThrowException) {
        ReactiveRedisTemplate reactiveRedisTemplate = mock(ReactiveRedisTemplate.class);
        Singleton.INST.single(ReactiveRedisTemplate.class, reactiveRedisTemplate);
        if (needThrowException) {
            when(reactiveRedisTemplate.execute(any(RedisScript.class), anyList(), anyList())).thenReturn(
                    Flux.error(Throwable::new));
        } else {
            when(reactiveRedisTemplate.execute(any(RedisScript.class), anyList(), anyList())).thenReturn(
                    Flux.just(Lists.newArrayList(allowedNum, newTokens)));
        }
    }
}
