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

package org.apache.shenyu.plugin.cache;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verifies response status, headers and body semantics across cache misses and hits.
 */
class CacheResponseReplayTest {

    private static final String SELECTOR = "cache-6505-selector";

    private final CachePlugin plugin = new CachePlugin();

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private ICache cache;

    @BeforeEach
    void setUp() {
        selector.setId(SELECTOR);
        selector.setHandle("{\"cacheType\":\"memory\",\"url\":\"memory://metadata\"}");
        rule.setSelectorId(SELECTOR);
        rule.setId("cache-6505-rule");
        CachePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(rule), new CacheRuleHandle());
        cache = ApplicationConfigCache.getInstance().init(SELECTOR, GsonUtils.getInstance().fromJson(selector.getHandle(), CacheUpstream.class));
        ApplicationContext context = mock(ApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void tearDown() {
        ApplicationConfigCache.getInstance().invalidateCache(SELECTOR);
        CachePluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(rule));
        SpringBeanUtils.getInstance().setApplicationContext(null);
    }

    @ParameterizedTest
    @ValueSource(ints = {201, 202, 302, 404, 500})
    void testHitPreservesStatusAndHeaders(final int status) {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            exchange.getResponse().setStatusCode(HttpStatusCode.valueOf(status));
            exchange.getResponse().getHeaders().setContentType(MediaType.parseMediaType("application/problem+json;charset=UTF-8"));
            exchange.getResponse().getHeaders().addAll("X-Upstream", List.of("first", "second"));
            exchange.getResponse().getHeaders().set(HttpHeaders.LOCATION, "/new-location");
            exchange.getResponse().getHeaders().setCacheControl("public, max-age=60");
            exchange.getResponse().getHeaders().setETag("\"v1\"");
            exchange.getResponse().getHeaders().setContentLength(999);
            return write(exchange, "你好");
        };
        MockServerWebExchange miss = request();
        StepVerifier.create(execute(miss, chain)).verifyComplete();
        MockServerWebExchange hit = request();
        StepVerifier.create(execute(hit, chain)).verifyComplete();

        assertEquals(1, calls.get());
        assertEquals(status, Objects.requireNonNullElse(hit.getResponse().getStatusCode(), HttpStatus.OK).value());
        assertEquals(miss.getResponse().getHeaders().getContentType(), hit.getResponse().getHeaders().getContentType());
        assertEquals(List.of("first", "second"), hit.getResponse().getHeaders().get("X-Upstream"));
        assertEquals("/new-location", hit.getResponse().getHeaders().getFirst(HttpHeaders.LOCATION));
        assertEquals("public, max-age=60", hit.getResponse().getHeaders().getCacheControl());
        assertEquals("\"v1\"", hit.getResponse().getHeaders().getETag());
        assertEquals(6, hit.getResponse().getHeaders().getContentLength());
        assertEquals("你好", hit.getResponse().getBodyAsString().block());
    }

    @ParameterizedTest
    @ValueSource(strings = {"cookie-header", "cookie-api", "vary", "private", "no-store", "no-cache", "late-private", "late-cookie", "partial", "not-modified"})
    void testResponsesThatCannotBeSharedAreNotStored(final String policy) {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if ("cookie-header".equals(policy)) {
                exchange.getResponse().getHeaders().add(HttpHeaders.SET_COOKIE, "session=private");
            } else if ("cookie-api".equals(policy)) {
                exchange.getResponse().addCookie(ResponseCookie.from("session", "private").build());
            } else if ("vary".equals(policy)) {
                exchange.getResponse().getHeaders().setVary(List.of("Accept"));
            } else if ("private".equals(policy)) {
                exchange.getResponse().getHeaders().setCacheControl("max-age=60, PRIVATE=\"X-User\"");
            } else if ("no-store".equals(policy)) {
                exchange.getResponse().getHeaders().setCacheControl("public, no-store");
            } else if ("no-cache".equals(policy)) {
                exchange.getResponse().getHeaders().setCacheControl("no-cache");
            } else if ("partial".equals(policy)) {
                exchange.getResponse().setStatusCode(HttpStatus.PARTIAL_CONTENT);
            } else if ("not-modified".equals(policy)) {
                exchange.getResponse().setStatusCode(HttpStatus.NOT_MODIFIED);
            } else {
                exchange.getResponse().beforeCommit(() -> {
                    if ("late-private".equals(policy)) {
                        exchange.getResponse().getHeaders().setCacheControl("private");
                    } else {
                        exchange.getResponse().addCookie(ResponseCookie.from("session", "private").build());
                    }
                    return Mono.empty();
                });
            }
            return write(exchange, "response-" + call);
        };

        StepVerifier.create(execute(request(), chain)).verifyComplete();
        StepVerifier.create(execute(request(), chain)).verifyComplete();

        assertEquals(2, calls.get());
    }

    @Test
    void testHopByHopHeadersAreNotReplayed() {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            HttpHeaders headers = exchange.getResponse().getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set(HttpHeaders.CONNECTION, "keep-alive, X-Hop");
            headers.set("Keep-Alive", "timeout=5");
            headers.set("X-Hop", "connection-specific");
            headers.set(HttpHeaders.TRANSFER_ENCODING, "chunked");
            return write(exchange, "body");
        };
        StepVerifier.create(execute(request(), chain)).verifyComplete();
        MockServerWebExchange hit = request();
        StepVerifier.create(execute(hit, chain)).verifyComplete();

        assertEquals(1, calls.get());
        assertFalse(hit.getResponse().getHeaders().containsKey(HttpHeaders.CONNECTION));
        assertFalse(hit.getResponse().getHeaders().containsKey("Keep-Alive"));
        assertFalse(hit.getResponse().getHeaders().containsKey("X-Hop"));
        assertFalse(hit.getResponse().getHeaders().containsKey(HttpHeaders.TRANSFER_ENCODING));
        assertEquals(4, hit.getResponse().getHeaders().getContentLength());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testEmptyResponseCanBeReplayed(final boolean complete) {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            exchange.getResponse().setStatusCode(HttpStatus.NO_CONTENT);
            exchange.getResponse().getHeaders().set("X-Upstream", "empty");
            return complete ? exchange.getResponse().setComplete() : exchange.getResponse().writeWith(Flux.empty());
        };
        StepVerifier.create(execute(request(), chain)).verifyComplete();
        MockServerWebExchange hit = request();
        StepVerifier.create(execute(hit, chain)).verifyComplete();

        assertEquals(1, calls.get());
        assertEquals(HttpStatus.NO_CONTENT, hit.getResponse().getStatusCode());
        assertEquals("empty", hit.getResponse().getHeaders().getFirst("X-Upstream"));
        assertNull(hit.getResponse().getHeaders().getFirst(HttpHeaders.CONTENT_LENGTH));
    }

    @Test
    void testLegacyBodyOnlyEntriesAreIgnored() {
        MockServerWebExchange exchange = request();
        cache.cacheData(CacheUtils.dataKey(exchange), "legacy".getBytes(StandardCharsets.UTF_8), 60).block();
        cache.cacheData(CacheUtils.contentTypeKey(exchange), MediaType.APPLICATION_JSON_VALUE.getBytes(StandardCharsets.UTF_8), 60).block();

        StepVerifier.create(execute(exchange, downstream -> write(downstream, "fresh"))).verifyComplete();

        assertEquals("fresh", exchange.getResponse().getBodyAsString().block());
    }

    @ParameterizedTest
    @CsvSource({"Authorization,Bearer user", "Cookie,session=user", "Range,bytes=0-1", "If-Match,v1", "If-None-Match,v1", "If-Modified-Since,yesterday",
        "If-Unmodified-Since,yesterday", "If-Range,v1", "Cache-Control,no-cache", "Cache-Control,no-store", "Pragma,no-cache"})
    void testRequestConditionsBypassCache(final String header, final String value) {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return write(exchange, "response-" + call);
        };
        StepVerifier.create(execute(request(), chain)).verifyComplete();
        MockServerWebExchange conditional = MockServerWebExchange.from(MockServerHttpRequest.get("http://gateway.example/cache-metadata").header(header, value).build());

        StepVerifier.create(execute(conditional, chain)).verifyComplete();

        assertEquals(2, calls.get());
        assertEquals("response-2", conditional.getResponse().getBodyAsString().block());
        MockServerWebExchange ordinary = request();
        StepVerifier.create(execute(ordinary, chain)).verifyComplete();
        assertEquals(2, calls.get());
        assertEquals("response-1", ordinary.getResponse().getBodyAsString().block());
    }

    @Test
    void testAbsentContentTypeIsPreserved() {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            return write(exchange, "body");
        };
        StepVerifier.create(execute(request(), chain)).verifyComplete();
        MockServerWebExchange hit = request();

        StepVerifier.create(execute(hit, chain)).verifyComplete();

        assertEquals(1, calls.get());
        assertNull(hit.getResponse().getHeaders().getContentType());
        assertEquals("body", hit.getResponse().getBodyAsString().block());
    }

    @Test
    void testMalformedEnvelopeFallsBackToUpstream() {
        MockServerWebExchange exchange = request();
        cache.cacheData(CacheUtils.responseKey(exchange, selector.getId(), rule.getId()), new byte[] {0, 0, 1, 0}, 60).block();

        StepVerifier.create(execute(exchange, downstream -> write(downstream, "fresh"))).verifyComplete();

        assertEquals("fresh", exchange.getResponse().getBodyAsString().block());
    }

    @Test
    void testFailedResponseWriteIsNotCached() {
        MockServerWebExchange exchange = request();
        MockServerHttpResponse response = new MockServerHttpResponse();
        IllegalStateException failure = new IllegalStateException("response write failed");
        response.setWriteHandler(body -> body.doOnNext(DataBufferUtils::release).then(Mono.error(failure)));
        ServerWebExchange failing = exchange.mutate().response(response).build();

        StepVerifier.create(execute(failing, downstream -> write(downstream, "body"))).expectErrorMatches(error -> error == failure).verify();

        assertNull(cache.getData(CacheUtils.responseKey(exchange, selector.getId(), rule.getId())).block());
    }

    @Test
    void testMatchedRulesDoNotShareEntry() {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return write(exchange, "response-" + calls.incrementAndGet());
        };
        StepVerifier.create(execute(request(), chain)).verifyComplete();
        RuleData other = new RuleData();
        other.setSelectorId(SELECTOR);
        other.setId("another-rule");
        String key = CacheKeyUtils.INST.getKey(other);
        CachePluginDataHandler.CACHED_HANDLE.get().cachedHandle(key, new CacheRuleHandle());
        try {
            MockServerWebExchange exchange = request();
            StepVerifier.create(plugin.doExecute(exchange, chain, selector, other)).verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("response-2", exchange.getResponse().getBodyAsString().block());
        } finally {
            CachePluginDataHandler.CACHED_HANDLE.get().removeHandle(key);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testCompletionAfterBodyWriteIsIdempotent(final boolean deferred) {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            Mono<Void> body = write(exchange, "body");
            return body.then(deferred ? Mono.defer(() -> exchange.getResponse().setComplete()) : exchange.getResponse().setComplete());
        };
        MockServerWebExchange miss = request();
        StepVerifier.create(execute(miss, chain)).verifyComplete();
        MockServerWebExchange hit = request();
        StepVerifier.create(execute(hit, chain)).verifyComplete();

        assertEquals(1, calls.get());
        assertEquals("body", miss.getResponse().getBodyAsString().block());
        assertEquals("body", hit.getResponse().getBodyAsString().block());
    }

    private Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return plugin.doExecute(exchange, chain, selector, rule);
    }

    private static Mono<Void> write(final ServerWebExchange exchange, final String body) {
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8))));
    }

    private static MockServerWebExchange request() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("http://gateway.example/cache-metadata").build());
    }
}
