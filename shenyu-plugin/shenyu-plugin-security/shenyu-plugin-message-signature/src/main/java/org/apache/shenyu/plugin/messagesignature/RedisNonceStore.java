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

import org.springframework.data.redis.core.ReactiveStringRedisTemplate;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Base64;
import java.util.Objects;

/** One SET NX with expiry per verified request, shared by all gateways in the trust domain. */
public final class RedisNonceStore implements NonceStore {

    private final ReactiveStringRedisTemplate redis;

    public RedisNonceStore(final ReactiveStringRedisTemplate redis) {
        this.redis = Objects.requireNonNull(redis);
    }

    @Override
    public Mono<Boolean> claim(final String keyId, final String nonce, final Duration ttl) {
        String key = "shenyu:rfc9421:" + encode(keyId) + ":" + encode(nonce);
        return redis.opsForValue().setIfAbsent(key, "1", ttl);
    }

    private static String encode(final String value) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(value.getBytes(StandardCharsets.UTF_8));
    }
}
