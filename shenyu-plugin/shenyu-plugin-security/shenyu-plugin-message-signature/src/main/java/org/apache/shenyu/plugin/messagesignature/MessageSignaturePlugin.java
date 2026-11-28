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

import java.security.GeneralSecurityException;
import java.time.DateTimeException;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Objects;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/** Verify a matched request against its original target and bytes, before any rewriting plugin. */
public final class MessageSignaturePlugin extends AbstractShenyuPlugin {

    private static final int ORDER = 19;

    private static final int BODY_LIMIT = 1024 * 1024;

    private final Map<String, MessageSignatureVerifier> rules;

    public MessageSignaturePlugin(final Map<String, MessageSignatureVerifier> rules) {
        this.rules = Map.copyOf(rules);
    }

    @Override
    public String named() {
        return "messageSignature";
    }

    @Override
    public int getOrder() {
        return ORDER;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        MessageSignatureVerifier verifier = Objects.isNull(rule.getId()) ? null : rules.get(rule.getId());
        if (Objects.isNull(verifier)) {
            return reject(exchange, HttpStatus.FORBIDDEN);
        }
        return DataBufferUtils.join(Flux.from(exchange.getRequest().getBody()), BODY_LIMIT)
                .map(MessageSignaturePlugin::copy).defaultIfEmpty(new byte[0])
                .onErrorResume(DataBufferLimitException.class, ex -> reject(exchange, HttpStatus.PAYLOAD_TOO_LARGE).then(Mono.empty()))
                .flatMap(body -> authorize(exchange, chain, verifier, body));
    }

    private Mono<Void> authorize(final ServerWebExchange exchange, final ShenyuPluginChain chain, final MessageSignatureVerifier verifier, final byte[] body) {
        final String nonce;
        try {
            nonce = verifier.verify(exchange.getRequest(), body, Instant.now());
        } catch (GeneralSecurityException | IllegalArgumentException | IllegalStateException | DateTimeException ex) {
            return reject(exchange, HttpStatus.UNAUTHORIZED);
        }
        if (Objects.isNull(verifier.store())) {
            return forward(exchange, chain, body);
        }
        return Mono.defer(() -> verifier.store().claim(verifier.keyId(), nonce, Duration.ofSeconds(MessageSignatureVerifier.MAX_AGE_SECONDS)))
                .switchIfEmpty(Mono.error(new IllegalStateException("Missing nonce store result")))
                .onErrorResume(ex -> reject(exchange, HttpStatus.SERVICE_UNAVAILABLE).then(Mono.empty()))
                .flatMap(first -> first ? forward(exchange, chain, body) : reject(exchange, HttpStatus.UNAUTHORIZED));
    }

    private static byte[] copy(final DataBuffer buffer) {
        try {
            if (buffer.readableByteCount() > BODY_LIMIT) {
                throw new DataBufferLimitException("Request body too large");
            }
            byte[] data = new byte[buffer.readableByteCount()];
            buffer.read(data);
            return data;
        } finally {
            DataBufferUtils.release(buffer);
        }
    }

    private static Mono<Void> forward(final ServerWebExchange exchange, final ShenyuPluginChain chain, final byte[] body) {
        ServerHttpRequestDecorator request = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return Flux.defer(() -> Flux.just(exchange.getResponse().bufferFactory().wrap(body)));
            }
        };
        return chain.execute(exchange.mutate().request(request).build());
    }

    private static Mono<Void> reject(final ServerWebExchange exchange, final HttpStatus status) {
        exchange.getResponse().setStatusCode(status);
        return exchange.getResponse().setComplete();
    }
}
