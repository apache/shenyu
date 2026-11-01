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

package org.apache.shenyu.plugin.ai.token.limiter;

import org.apache.shenyu.infra.redis.serializer.ShenyuRedisSerializationContext;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import redis.embedded.RedisServer;
import redis.embedded.RedisExecProvider;
import redis.embedded.util.OS;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verify accounting and fixed-window expiry against a disposable Redis server.
 */
class AiTokenAccountingRedisTest {

    @TempDir
    private static Path redisDirectory;

    private static RedisServer server;

    private static LettuceConnectionFactory connectionFactory;

    private static ReactiveRedisTemplate<String, String> redis;

    private final AiTokenLimiterPlugin plugin = new AiTokenLimiterPlugin();

    @BeforeAll
    static void startRedis() throws IOException {
        final int port;
        try (ServerSocket socket = new ServerSocket(0)) {
            port = socket.getLocalPort();
        }
        RedisExecProvider executableProvider = RedisExecProvider.defaultProvider();
        if (System.getProperty("os.name").startsWith("Windows")) {
            // The embedded provider's old Guava extraction requests POSIX permissions on Windows.
            Path executable = redisDirectory.resolve("redis-server.exe");
            try (InputStream binary = RedisServer.class.getResourceAsStream("/redis-server-2.8.19.exe")) {
                Files.copy(Objects.requireNonNull(binary), executable);
            }
            executableProvider.override(OS.WINDOWS, executable.toString());
        }
        server = RedisServer.builder().redisExecProvider(executableProvider).port(port).setting("maxmemory 64m").build();
        server.start();
        connectionFactory = new LettuceConnectionFactory("127.0.0.1", port);
        connectionFactory.afterPropertiesSet();
        connectionFactory.start();
        redis = new ReactiveRedisTemplate<>(connectionFactory, ShenyuRedisSerializationContext.stringSerializationContext());
    }

    @AfterAll
    static void stopRedis() {
        if (Objects.nonNull(connectionFactory)) {
            connectionFactory.destroy();
        }
        if (Objects.nonNull(server)) {
            server.stop();
        }
    }

    @Test
    void testAbsentAndLegacyCountersReceiveAnExpiry() {
        assertEquals(10L, record("absent", 10, 60));
        assertPositiveTtl("absent", 60);
        redis.opsForValue().set("legacy", "20").block();
        assertEquals(30L, record("legacy", 10, 60));
        assertPositiveTtl("legacy", 60);
    }

    @Test
    void testLiveCounterKeepsItsDeadlineUnderConcurrentWriters() {
        redis.opsForValue().set("live", "20", Duration.ofSeconds(10)).block();
        final Duration before = redis.getExpire("live").block();
        Flux.range(0, 100).flatMap(index -> new AiTokenLimiterPlugin().recordTokensUsage(redis, "live", 1L, 60L), 16).blockLast();
        assertEquals("120", redis.opsForValue().get("live").block());
        Duration after = redis.getExpire("live").block();
        assertTrue(!after.isNegative() && !after.isZero());
        assertTrue(after.compareTo(before) <= 0, "increments must not extend the original deadline");
    }

    @Test
    void testExpiredCounterIsRecreatedWithAnExpiry() {
        redis.opsForValue().set("expired", "99", Duration.ofMillis(1)).block();
        Mono.delay(Duration.ofMillis(30)).block();
        assertNull(redis.opsForValue().get("expired").block());
        assertEquals(100L, record("expired", 100, 1));
        assertPositiveTtl("expired", 1);
        Mono.delay(Duration.ofMillis(1100)).block();
        assertNull(redis.opsForValue().get("expired").block(), "a final over-budget increment must recover without another response");
        assertEquals(1L, record("expired", 1, 1));
        assertPositiveTtl("expired", 1);
    }

    @Test
    void testConcurrentCreationCannotLeaveAPersistentCounter() {
        Flux.range(0, 100).flatMap(index -> new AiTokenLimiterPlugin().recordTokensUsage(redis, "concurrent", 1L, 60L), 16).blockLast();
        assertEquals("100", redis.opsForValue().get("concurrent").block());
        assertPositiveTtl("concurrent", 60);
    }

    @ParameterizedTest
    @ValueSource(strings = {"initialization", "ttl", "repair"})
    void testExpiryAtEveryFormerCommandBoundary(final String boundary) {
        String key = "former-boundary-" + boundary;
        redis.opsForValue().set(key, "99").block();
        if (!"repair".equals(boundary)) {
            redis.expire(key, Duration.ofSeconds(10)).block();
        }
        redis.opsForValue().setIfAbsent(key, "0", Duration.ofSeconds(60)).block();
        if (!"initialization".equals(boundary)) {
            redis.getExpire(key).block();
        }
        if ("repair".equals(boundary)) {
            redis.expire(key, Duration.ofSeconds(60)).block();
        }
        // Delete at the barrier to model expiry after each command of the former sequence.
        redis.delete(key).block();
        assertEquals(100L, record(key, 100, 60));
        assertPositiveTtl(key, 60);
    }

    @ParameterizedTest
    @ValueSource(longs = {0, -1})
    void testInvalidWindowCannotCreateAPersistentCounter(final long window) {
        String key = "invalid-window-" + window;
        assertThrows(RuntimeException.class, () -> record(key, 10, window));
        assertNull(redis.opsForValue().get(key).block());
    }

    @Test
    void testNearExpiryCounterIsNeverMadePersistent() {
        for (int index = 0; index < 30; index++) {
            String key = "boundary-" + index;
            redis.opsForValue().set(key, "20", Duration.ofMillis(1)).block();
            record(key, 10, 60);
            Duration ttl = redis.getExpire(key).block();
            // An already expired key completes empty; only a persistent counter violates the invariant.
            assertNotEquals(Duration.ofSeconds(-1), ttl);
        }
    }

    @Test
    void testAccountingErrorsArePropagatedWithoutReplacingTheCounter() {
        redis.opsForValue().set("invalid", "not-a-number", Duration.ofSeconds(10)).block();
        assertThrows(RuntimeException.class, () -> record("invalid", 10, 60));
        assertEquals("not-a-number", redis.opsForValue().get("invalid").block());
        assertPositiveTtl("invalid", 10);
    }

    private Long record(final String key, final long tokens, final long windowSeconds) {
        return plugin.recordTokensUsage(redis, key, tokens, windowSeconds).block(Duration.ofSeconds(5));
    }

    private void assertPositiveTtl(final String key, final long windowSeconds) {
        Duration ttl = redis.getExpire(key).block();
        assertTrue(!ttl.isNegative() && !ttl.isZero());
        assertTrue(ttl.compareTo(Duration.ofSeconds(windowSeconds)) <= 0);
    }
}
