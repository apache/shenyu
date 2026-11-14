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

package org.apache.shenyu.plugin.ratelimiter.executor;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.infra.redis.RedisConfigProperties;
import org.apache.shenyu.plugin.ratelimiter.algorithm.RateLimiterAlgorithm;
import org.apache.shenyu.plugin.ratelimiter.algorithm.RateLimiterAlgorithmFactory;
import org.apache.shenyu.plugin.ratelimiter.handler.RateLimiterPluginDataHandler;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;
import redis.embedded.RedisServer;

import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Test of rate limiter Lua scripts.
 */
@ExtendWith(MockitoExtension.class)
public class RedisRateLimiterScriptsTest {

    private static final long FIXED_NOW = 1700000000L;

    private static RedisServer redisServer;

    @BeforeAll
    public static void startup() {
        redisServer = RedisServer.builder()
                .port(63792)
                .setting("maxmemory 64m")
                .build();
        redisServer.start();
        RateLimiterPluginDataHandler handler = new RateLimiterPluginDataHandler();
        RedisConfigProperties config = new RedisConfigProperties();
        config.setUrl("127.0.0.1:63792");
        PluginData pluginData = PluginData.builder()
                .enabled(true)
                .config(GsonUtils.getInstance().toJson(config))
                .build();

        handler.handlerPlugin(pluginData);
    }

    @AfterAll
    public static void end() {
        redisServer.stop();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void leakyBucketLuaTest() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("leakyBucket");
        RedisScript<?> script = rateLimiterAlgorithm.getScript();
        List<String> keys = Stream.of("test-leakyBucket").collect(Collectors.toList());
        List<String> scriptArgs = Arrays.asList(10 + "", 100 + "", String.valueOf(Instant.now().getEpochSecond()), "1");
        Flux<List<Long>> resultFlux = Singleton.INST.get(ReactiveRedisTemplate.class).execute(script, keys, scriptArgs);
        StepVerifier
                .create(resultFlux)
                .expectSubscription()
                .expectNext(Arrays.asList(1L, 1L))
                .expectComplete()
                .verify();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void concurrentLuaTest() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("concurrent");
        RedisScript<?> script = rateLimiterAlgorithm.getScript();
        List<String> keys = Stream.of("test-concurrent", "cd849432").collect(Collectors.toList());
        List<String> scriptArgs = Arrays.asList(10 + "", 100 + "", String.valueOf(Instant.now().getEpochSecond()), "1");
        Flux<List<Long>> resultFlux = Singleton.INST.get(ReactiveRedisTemplate.class).execute(script, keys, scriptArgs);
        StepVerifier
                .create(resultFlux)
                .expectSubscription()
                .expectNext(Arrays.asList(1L, 1L))
                .expectComplete()
                .verify();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void tokenBucketLuaTest() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("tokenBucket");
        RedisScript<?> script = rateLimiterAlgorithm.getScript();
        List<String> keys = Stream.of("test-tokenBucket").collect(Collectors.toList());
        List<String> scriptArgs = Arrays.asList(10 + "", 100 + "", String.valueOf(Instant.now().getEpochSecond()), "1");
        Flux<List<Long>> resultFlux = Singleton.INST.get(ReactiveRedisTemplate.class).execute(script, keys, scriptArgs);
        StepVerifier
                .create(resultFlux)
                .expectSubscription()
                .expectNext(Arrays.asList(1L, 99L))
                .expectComplete()
                .verify();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void tokenBucketLuaWithCapacityLessThanHalfRateTest() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("tokenBucket");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        List<String> keys = rateLimiterAlgorithm.getKeys("test-tokenBucket-small-capacity");
        long now = Instant.now().getEpochSecond();
        List<String> scriptArgs = Arrays.asList("10", "4", String.valueOf(now), "1");
        List<String> refillArgs = Arrays.asList("10", "4", String.valueOf(now + 1), "1");
        Flux<List<Long>> resultFlux = Flux.range(0, 5)
                .concatMap(index -> redisTemplate.execute(script, keys, scriptArgs))
                .concatWith(redisTemplate.execute(script, keys, refillArgs));
        StepVerifier
                .create(resultFlux)
                .expectSubscription()
                .expectNext(Arrays.asList(1L, 3L))
                .expectNext(Arrays.asList(1L, 2L))
                .expectNext(Arrays.asList(1L, 1L))
                .expectNext(Arrays.asList(1L, 0L))
                .expectNext(Arrays.asList(0L, 0L))
                .expectNext(Arrays.asList(1L, 3L))
                .expectComplete()
                .verify();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowLuaTest() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        List<String> keys = rateLimiterAlgorithm.getKeys("test-slidingWindow");
        List<String> scriptArgs = Arrays.asList("10", "100", String.valueOf(FIXED_NOW), "1");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        Flux<List<Long>> resultFlux = redisTemplate.execute(script, keys, scriptArgs);
        StepVerifier
                .create(resultFlux)
                .expectSubscription()
                .expectNext(Arrays.asList(1L, 99L))
                .expectComplete()
                .verify();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowRemovesExpiredRequestsBeforeCounting() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        String tokenKey = rateLimiterAlgorithm.getKeys("test-slidingWindow-expired").get(0);
        long now = Instant.now().getEpochSecond();
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        redisTemplate.opsForZSet().add(tokenKey, "expired", now - 2).block();
        List<String> keys = Arrays.asList(tokenKey, "current");
        List<String> scriptArgs = Arrays.asList("1", "1", String.valueOf(now), "1");

        Flux<List<Long>> resultFlux = redisTemplate.execute(script, keys, scriptArgs);

        StepVerifier.create(resultFlux)
                .expectNext(Arrays.asList(1L, 0L))
                .verifyComplete();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowHonoursRequestedCount() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        List<String> scriptArgs = Arrays.asList("0.5", "10", String.valueOf(FIXED_NOW), "5");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        Flux<List<Long>> resultFlux = Flux.range(0, 4)
                .concatMap(index -> redisTemplate.execute(script,
                        rateLimiterAlgorithm.getKeys("test-slidingWindow-request-count"), scriptArgs));
        StepVerifier.create(resultFlux)
                .expectNext(Arrays.asList(1L, 5L))
                .expectNext(Arrays.asList(1L, 0L))
                .expectNext(Arrays.asList(0L, 0L))
                .expectNext(Arrays.asList(0L, 0L))
                .verifyComplete();
        Assertions.assertEquals(Long.valueOf(10L), redisTemplate.opsForZSet()
                .size(rateLimiterAlgorithm.getKeys("test-slidingWindow-request-count").get(0)).block());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowRecordsRequestedUnits() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        List<String> keys = rateLimiterAlgorithm.getKeys("test-slidingWindow-recorded-units");
        List<String> scriptArgs = Arrays.asList("10", "10", String.valueOf(FIXED_NOW), "3");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        StepVerifier.create(redisTemplate.execute(script, keys, scriptArgs))
                .expectNext(Arrays.asList(1L, 7L))
                .verifyComplete();
        Assertions.assertEquals(Long.valueOf(3L), redisTemplate.opsForZSet().size(keys.get(0)).block());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowUnitsAreUniquePerCall() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        List<String> firstKeys = rateLimiterAlgorithm.getKeys("test-slidingWindow-unique-units");
        List<String> secondKeys = rateLimiterAlgorithm.getKeys("test-slidingWindow-unique-units");
        List<String> scriptArgs = Arrays.asList("0.5", "4", String.valueOf(FIXED_NOW), "2");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        Flux<List<Long>> resultFlux = redisTemplate.execute(script, firstKeys, scriptArgs)
                .concatWith(redisTemplate.execute(script, secondKeys, scriptArgs));
        StepVerifier.create(resultFlux)
                .expectNext(Arrays.asList(1L, 2L))
                .expectNext(Arrays.asList(1L, 0L))
                .verifyComplete();
        Assertions.assertEquals(Long.valueOf(4L), redisTemplate.opsForZSet().size(firstKeys.get(0)).block());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowRemainderNeverNegative() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        List<String> keys = rateLimiterAlgorithm.getKeys("test-slidingWindow-negative-remainder");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        long now = FIXED_NOW;
        for (int index = 0; index < 6; index++) {
            redisTemplate.opsForZSet().add(keys.get(0), "seed-" + index, now).block();
        }
        List<String> scriptArgs = Arrays.asList("5", "5", String.valueOf(now), "1");
        StepVerifier.create(redisTemplate.execute(script, keys, scriptArgs))
                .expectNext(Arrays.asList(0L, 0L))
                .verifyComplete();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowNormalizesNonPositiveRequestCount() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        String zeroId = "test-slidingWindow-zero-count";
        String negativeId = "test-slidingWindow-negative-count";
        List<String> zeroArgs = Arrays.asList("0.5", "2", String.valueOf(FIXED_NOW), "0");
        List<String> negativeArgs = Arrays.asList("0.5", "2", String.valueOf(FIXED_NOW), "-3");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        Flux<List<Long>> zeroFlux = Flux.range(0, 3)
                .concatMap(index -> redisTemplate.execute(script, rateLimiterAlgorithm.getKeys(zeroId), zeroArgs));
        Flux<List<Long>> negativeFlux = Flux.range(0, 2)
                .concatMap(index -> redisTemplate.execute(script, rateLimiterAlgorithm.getKeys(negativeId), negativeArgs));
        StepVerifier.create(Flux.concat(zeroFlux, negativeFlux))
                .expectNext(Arrays.asList(1L, 1L))
                .expectNext(Arrays.asList(1L, 0L))
                .expectNext(Arrays.asList(0L, 0L))
                .expectNext(Arrays.asList(1L, 1L))
                .expectNext(Arrays.asList(1L, 0L))
                .verifyComplete();
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowRoundsFractionalRequestCountUp() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        String id = "test-slidingWindow-fractional-count";
        List<String> scriptArgs = Arrays.asList("0.5", "6", String.valueOf(FIXED_NOW), "2.5");
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        Flux<List<Long>> resultFlux = Flux.range(0, 3)
                .concatMap(index -> redisTemplate.execute(script, rateLimiterAlgorithm.getKeys(id), scriptArgs));
        StepVerifier.create(resultFlux)
                .expectNext(Arrays.asList(1L, 3L))
                .expectNext(Arrays.asList(1L, 0L))
                .expectNext(Arrays.asList(0L, 0L))
                .verifyComplete();
        Assertions.assertEquals(Long.valueOf(6L), redisTemplate.opsForZSet()
                .size(rateLimiterAlgorithm.getKeys(id).get(0)).block());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void slidingWindowExpiresRecordedUnits() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<List<Long>> script = (RedisScript<List<Long>>) rateLimiterAlgorithm.getScript();
        String id = "test-slidingWindow-unit-expiry";
        ReactiveRedisTemplate<String, String> redisTemplate = Singleton.INST.get(ReactiveRedisTemplate.class);
        Flux<List<Long>> resultFlux = Flux.just(0L, 3L, 9L)
                .concatMap(offset -> redisTemplate.execute(script, rateLimiterAlgorithm.getKeys(id),
                        Arrays.asList("0.25", "2", String.valueOf(FIXED_NOW + offset), "2")));
        StepVerifier.create(resultFlux)
                .expectNext(Arrays.asList(1L, 0L))
                .expectNext(Arrays.asList(0L, 0L))
                .expectNext(Arrays.asList(1L, 0L))
                .verifyComplete();
    }
}
