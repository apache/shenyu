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

package org.apache.shenyu.plugin.messagesignature;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveStringRedisTemplate;
import reactor.core.publisher.Flux;
import redis.embedded.RedisExecProvider;
import redis.embedded.RedisServer;
import redis.embedded.util.OS;

import java.io.InputStream;
import java.net.ServerSocket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RedisNonceStoreTest {

    @TempDir
    private Path temp;

    @Test
    void onlyOneSharedClaimSucceedsAndItHasAnExpiry() throws Exception {
        int port;
        try (ServerSocket socket = new ServerSocket(0)) {
            port = socket.getLocalPort();
        }
        RedisExecProvider provider = RedisExecProvider.defaultProvider();
        if (System.getProperty("os.name").startsWith("Windows")) {
            Path binary = temp.resolve("redis-server.exe");
            try (InputStream stream = RedisServer.class.getResourceAsStream("/redis-server-2.8.19.exe")) {
                Files.copy(Objects.requireNonNull(stream), binary);
            }
            provider.override(OS.WINDOWS, binary.toString());
        }
        RedisServer server = RedisServer.builder().redisExecProvider(provider).port(port).setting("bind 127.0.0.1").setting("maxmemory 64m").build();
        server.start();
        LettuceConnectionFactory factory = new LettuceConnectionFactory("127.0.0.1", port);
        try {
            factory.afterPropertiesSet();
            factory.start();
            ReactiveStringRedisTemplate redis = new ReactiveStringRedisTemplate(factory);
            RedisNonceStore first = new RedisNonceStore(redis);
            RedisNonceStore second = new RedisNonceStore(redis);
            Duration ttl = Duration.ofSeconds(60);
            long accepted = Flux.merge(first.claim("key", "nonce", ttl), second.claim("key", "nonce", ttl))
                    .filter(Boolean.TRUE::equals).count().block(Duration.ofSeconds(10));
            assertEquals(1L, accepted);
            Duration remaining = redis.getExpire("shenyu:rfc9421:a2V5:bm9uY2U").block();
            assertTrue(!remaining.isNegative() && !remaining.isZero() && remaining.compareTo(ttl) <= 0);
        } finally {
            factory.destroy();
            server.stop();
        }
    }
}
