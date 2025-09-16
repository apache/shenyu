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
    public void slidingWindowLuaTest() {
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance("slidingWindow");
        RedisScript<?> script = rateLimiterAlgorithm.getScript();
        List<String> keys = Stream.of("test-slidingWindow").collect(Collectors.toList());
        List<String> scriptArgs = Arrays.asList(10 + "", 100 + "", String.valueOf(Instant.now().getEpochSecond()), "1");
        Flux<List<Long>> resultFlux = Singleton.INST.get(ReactiveRedisTemplate.class).execute(script, keys, scriptArgs);
        StepVerifier
                .create(resultFlux)
                .expectSubscription()
                .expectNext(Arrays.asList(1L, 100L))
                .expectComplete()
                .verify();
    }
}
