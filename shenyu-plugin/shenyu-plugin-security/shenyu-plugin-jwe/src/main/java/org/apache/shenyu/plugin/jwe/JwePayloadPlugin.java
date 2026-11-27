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

package org.apache.shenyu.plugin.jwe;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.InvalidMediaTypeException;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/** Transform bounded JSON on explicitly matched HTTP routes; never write upstream plaintext on encryption failure. */
public final class JwePayloadPlugin extends AbstractShenyuPlugin {

    private static final int ORDER = 99;

    private static final MediaType JOSE = new MediaType("application", "jose");

    private final Map<RuleKey, JweRoute> routes;

    private final int ciphertextLimit;

    private final int plaintextLimit;

    public JwePayloadPlugin(final Map<String, JweRoute> routes) {
        this(routes.entrySet().stream().collect(Collectors.toMap(entry -> new RuleKey(Constants.SYS_DEFAULT_NAMESPACE_ID, entry.getKey()), Map.Entry::getValue)),
                Constants.BYTES_PER_MB, Constants.BYTES_PER_MB);
    }

    public JwePayloadPlugin(final Map<RuleKey, JweRoute> routes, final int ciphertextLimit, final int plaintextLimit) {
        if (ciphertextLimit <= 0 || plaintextLimit <= 0) {
            throw new IllegalArgumentException("JWE byte limits must be positive");
        }
        this.routes = Map.copyOf(routes);
        this.ciphertextLimit = ciphertextLimit;
        this.plaintextLimit = plaintextLimit;
    }

    @Override
    public String named() {
        return "jwePayload";
    }

    @Override
    public int getOrder() {
        return ORDER;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        JweRoute route = findRoute(rule);
        ServerHttpResponse response = exchange.getResponse();
        if (Objects.isNull(route)) {
            return reject(response, HttpStatus.INTERNAL_SERVER_ERROR);
        }
        if (exchange.getRequest().getMethod() != HttpMethod.POST) {
            return reject(response, HttpStatus.METHOD_NOT_ALLOWED);
        }
        HttpHeaders headers = exchange.getRequest().getHeaders();
        if (!mediaMatches(headers, route.decrypts() ? JOSE : MediaType.APPLICATION_JSON)) {
            return reject(response, HttpStatus.UNSUPPORTED_MEDIA_TYPE);
        }
        if (route.decrypts() && exchange.getAttributes().containsKey(Constants.CACHED_REQUEST_BODY)) {
            // An HTTP retry cache created before decryption would replay the wrong representation.
            return reject(response, HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return requestBody(exchange, route)
                .onErrorResume(DataBufferLimitException.class, ex -> reject(response, HttpStatus.PAYLOAD_TOO_LARGE).then(Mono.empty()))
                .onErrorResume(Exception.class, ex -> reject(response, HttpStatus.BAD_REQUEST).then(Mono.empty()))
                .flatMap(body -> chain.execute(exchange.mutate().request(request(exchange, body))
                        .response(route.encrypts() ? encryptedResponse(response, route) : response).build()));
    }

    private Mono<byte[]> requestBody(final ServerWebExchange exchange, final JweRoute route) {
        int inputLimit = route.decrypts() ? ciphertextLimit : plaintextLimit;
        return collect(exchange.getRequest().getBody(), inputLimit).flatMap(data -> Mono.fromCallable(() -> {
            if (route.decrypts()) {
                return route.decrypt(data, plaintextLimit);
            }
            JweRoute.requireJson(data);
            return data;
        }).subscribeOn(Schedulers.boundedElastic()));
    }

    private JweRoute findRoute(final RuleData rule) {
        if (Objects.isNull(rule.getNamespaceId()) || rule.getNamespaceId().isBlank() || Objects.isNull(rule.getId()) || rule.getId().isBlank()) {
            return null;
        }
        return routes.get(new RuleKey(rule.getNamespaceId(), rule.getId()));
    }

    private static boolean mediaMatches(final HttpHeaders headers, final MediaType expected) {
        if (headers.containsKey(HttpHeaders.CONTENT_ENCODING) || headers.getOrEmpty(HttpHeaders.CONTENT_TYPE).size() != 1) {
            return false;
        }
        try {
            MediaType actual = headers.getContentType();
            return Objects.nonNull(actual) && expected.getType().equalsIgnoreCase(actual.getType()) && expected.getSubtype().equalsIgnoreCase(actual.getSubtype());
        } catch (InvalidMediaTypeException ex) {
            return false;
        }
    }

    private static ServerHttpRequestDecorator request(final ServerWebExchange exchange, final byte[] body) {
        HttpHeaders headers = new HttpHeaders();
        headers.putAll(exchange.getRequest().getHeaders());
        representation(headers, MediaType.APPLICATION_JSON, body.length);
        return new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public HttpHeaders getHeaders() {
                return HttpHeaders.readOnlyHttpHeaders(headers);
            }

            @Override
            public Flux<DataBuffer> getBody() {
                return Flux.defer(() -> Flux.just(exchange.getResponse().bufferFactory().wrap(body)));
            }
        };
    }

    private ServerHttpResponse encryptedResponse(final ServerHttpResponse delegate, final JweRoute route) {
        return new ServerHttpResponseDecorator(delegate) {
            @Override
            public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
                return collect(body, plaintextLimit).flatMap(data -> Mono.fromCallable(() -> {
                    if (!mediaMatches(getHeaders(), MediaType.APPLICATION_JSON)) {
                        throw new IllegalArgumentException("Expected a JSON response");
                    }
                    byte[] compact = route.encrypt(data);
                    if (compact.length > ciphertextLimit) {
                        throw new DataBufferLimitException("Encrypted response too large");
                    }
                    return compact;
                }).subscribeOn(Schedulers.boundedElastic()))
                        .onErrorResume(Exception.class, ex -> reject(delegate, HttpStatus.BAD_GATEWAY).then(Mono.empty()))
                        .flatMap(compact -> {
                            representation(getHeaders(), JOSE, compact.length);
                            return delegate.writeWith(Mono.just(bufferFactory().wrap(compact)));
                        });
            }

            @Override
            public Mono<Void> writeAndFlushWith(final Publisher<? extends Publisher<? extends DataBuffer>> body) {
                // Cancel upstream without waiting for the first item of a potentially idle stream.
                return Flux.from(body).take(0, false)
                        .then(Mono.defer(() -> reject(delegate, HttpStatus.BAD_GATEWAY)));
            }
        };
    }

    private static Mono<byte[]> collect(final Publisher<? extends DataBuffer> body, final int limit) {
        return DataBufferUtils.join(Flux.from(body), limit).map(buffer -> {
            try {
                if (buffer.readableByteCount() > limit) {
                    throw new DataBufferLimitException("Payload too large");
                }
                byte[] bytes = new byte[buffer.readableByteCount()];
                buffer.read(bytes);
                return bytes;
            } finally {
                DataBufferUtils.release(buffer);
            }
        }).defaultIfEmpty(new byte[0]);
    }

    private static void representation(final HttpHeaders headers, final MediaType type, final int length) {
        headers.remove(HttpHeaders.TRANSFER_ENCODING);
        headers.remove(HttpHeaders.CONTENT_ENCODING);
        headers.remove(HttpHeaders.ETAG);
        headers.remove("Content-Digest");
        headers.remove("Repr-Digest");
        headers.remove("Content-MD5");
        headers.setContentType(type);
        headers.setContentLength(length);
    }

    private static Mono<Void> reject(final ServerHttpResponse response, final HttpStatus status) {
        response.getHeaders().clear();
        response.setStatusCode(status);
        response.getHeaders().setContentLength(0);
        return response.setComplete();
    }

    /** Deployment trust is scoped by both the synchronized namespace and rule identity. */
    public record RuleKey(String namespaceId, String ruleId) {
        public RuleKey {
            if (namespaceId.isBlank() || ruleId.isBlank()) {
                throw new IllegalArgumentException("Namespace and rule IDs must be nonblank");
            }
        }
    }
}
