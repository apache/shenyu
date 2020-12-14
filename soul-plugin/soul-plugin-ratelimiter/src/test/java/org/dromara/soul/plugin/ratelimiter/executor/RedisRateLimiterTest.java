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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import org.dromara.soul.plugin.base.utils.Singleton;
import org.dromara.soul.plugin.ratelimiter.response.RateLimiterResponse;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;

import com.google.common.collect.Lists;

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

    @Before
    public void setUp() {
        this.redisRateLimiter = new RedisRateLimiter();
    }

    /**
     *  getKeys result test case.
     */
    @Test
    public void getKeysTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        String testId = "testId";
        String tokenKeyActualResult = "request_rate_limiter.{testId}.tokens";
        String timestampKeyActualResult = "request_rate_limiter.{testId}.timestamp";
        List<String> result = redisRateLimiterGetKeys(testId);
        Assert.assertEquals(tokenKeyActualResult, result.get(0));
        Assert.assertEquals(timestampKeyActualResult, result.get(1));
    }

    /**
     * redisRateLimiter.isAllowed allowed case.
     */
    @Test
    public void allowedTest() {
        isAllowedPreInit(1L, 1L, false);
        Mono<RateLimiterResponse> responseMono = redisRateLimiter.isAllowed(DEFAULT_TEST_ID, DEFAULT_TEST_REPLENISH_RATE,
                DEFAULT_TEST_BURST_CAPACITY);
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
        Mono<RateLimiterResponse> responseMono = redisRateLimiter.isAllowed(DEFAULT_TEST_ID, DEFAULT_TEST_REPLENISH_RATE,
                DEFAULT_TEST_BURST_CAPACITY);
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
        Mono<RateLimiterResponse> responseMono = redisRateLimiter.isAllowed(DEFAULT_TEST_ID, DEFAULT_TEST_REPLENISH_RATE,
                DEFAULT_TEST_BURST_CAPACITY);
        StepVerifier.create(responseMono).assertNext(r -> {
            Assert.assertEquals(-1, r.getTokensRemaining());
            Assert.assertTrue(r.isAllowed());
        }).verifyComplete();
    }

    /**
     * initialized error init test.
     */
    @Test(expected = IllegalStateException.class)
    public void initializedErrorTest() throws NoSuchFieldException, IllegalAccessException {
        Field initializedField = RedisRateLimiter.class.getDeclaredField("initialized");
        initializedField.setAccessible(true);
        initializedField.set(redisRateLimiter, new AtomicBoolean(false));
        redisRateLimiter.isAllowed(DEFAULT_TEST_ID, DEFAULT_TEST_REPLENISH_RATE,
                DEFAULT_TEST_BURST_CAPACITY);

    }

    /**
     * redisRateLimiter.isAllowed test pre init.
     *
     * @param allowedNum         mock lua allowedNum result
     * @param newTokens          mock lua newTokens result
     * @param needThrowException mock lua throw exception
     */
    @SuppressWarnings("unchecked")
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

    /**
     * mock RedisRateLimiter getKeys method invoke.
     * @param id        id
     * @return          [tokenKey, timestampKey]
     */
    @SuppressWarnings("unchecked")
    private List<String> redisRateLimiterGetKeys(final String id)
            throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method getKeysMethod = RedisRateLimiter.class.getDeclaredMethod("getKeys", String.class);
        getKeysMethod.setAccessible(true);
        return (List<String>) getKeysMethod.invoke(redisRateLimiter, id);
    }

}
