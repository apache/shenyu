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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import com.github.benmanes.caffeine.cache.Cache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;
import reactor.util.context.Context;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AiExactResponseCacheTest {

    private static final String REQUEST = "{\"model\":\"test-model\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private static final String RESPONSE = "{\"id\":\"chatcmpl-test\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"hello back\"},\"finish_reason\":\"stop\"}],"
            + "\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":3,\"total_tokens\":8}}";

    private static final AiResponseCacheScope SCOPE = new AiResponseCacheScope("tenant", "subject", "access", "policy", "prompt", "selector", "rule");

    private static final Function<AiUpstreamResponse, Mono<Void>> DRAIN = response -> response.getBody().then();

    @Test
    void testHitReplaysOriginalJsonAndSafeHeadersWithoutUpstream() {
        final AiExactResponseCache cache = new AiExactResponseCache(enabled());
        final AtomicInteger calls = new AtomicInteger();
        Map<String, List<String>> headers = new LinkedHashMap<>(jsonHeaders());
        headers.put("Date", List.of("yesterday"));
        headers.put("Content-Length", List.of("999"));
        headers.put("X-Request-Id", List.of("upstream-attempt"));
        headers.put("Connection", List.of("X-Private, Content-Language"));
        headers.put("X-Private", List.of("connection-specific"));
        headers.put("Content-Language", List.of("en"));
        AiUpstreamResponse response = response(200, headers, RESPONSE);
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, response, DRAIN, calls));
        AtomicReference<AiUpstreamResponse> hit = new AtomicReference<>();
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, response, value -> {
            hit.set(value);
            return value.getBody().doOnNext(bytes -> assertArrayEquals(bytes(RESPONSE), bytes)).then();
        }, calls));
        assertEquals(1, calls.get());
        assertEquals(Map.of("content-type", List.of("application/json; charset=utf-8")), hit.get().getHeaders());
        assertEquals(new AiExactResponseCache.Snapshot(1, 1, 0, 1), cache.stats());
    }

    @Test
    void testEveryScopeDimensionIsolatesAndMissingScopeBypasses() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        String[] values = {"tenant", "subject", "access", "policy", "prompt", "selector", "rule"};
        for (int index = 0; index < values.length; index++) {
            String[] changed = values.clone();
            changed[index] += "-changed";
            AiResponseCacheScope scope = new AiResponseCacheScope(changed[0], changed[1], changed[2], changed[3], changed[4], changed[5], changed[6]);
            complete(call(cache, REQUEST, request(REQUEST), scope, success(), DRAIN, calls));
        }
        AiUpstreamRequest forged = request(REQUEST);
        forged.setHeaders(Map.of("X-Tenant", List.of("tenant"), "X-Subject", List.of("subject")));
        complete(call(cache, REQUEST, forged, null, success(), DRAIN, calls));
        complete(call(cache, REQUEST, forged, null, success(), DRAIN, calls));
        AiResponseCacheScope incomplete = new AiResponseCacheScope("tenant", " ", "access", "policy", "prompt", "selector", "rule");
        complete(call(cache, REQUEST, request(REQUEST), incomplete, success(), DRAIN, calls));
        AiResponseCacheScope overlong = new AiResponseCacheScope("tenant", "x".repeat(257), "access", "policy", "prompt", "selector", "rule");
        complete(call(cache, REQUEST, request(REQUEST), overlong, success(), DRAIN, calls));
        AiResponseCacheScope missing = new AiResponseCacheScope("tenant", "subject", null, "policy", "prompt", "selector", "rule");
        complete(call(cache, REQUEST, request(REQUEST), missing, success(), DRAIN, calls));
        complete(call(cache, REQUEST, request(REQUEST), null, success(), DRAIN, calls)
                .contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, "untrusted")));
        assertEquals(14, calls.get());
        assertEquals(0, cache.stats().hits());
        assertEquals(6, cache.stats().bypasses());
    }

    @Test
    void testOriginalEffectiveEndpointAndCredentialsPartitionKeys() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        complete(call(cache, REQUEST.replace("hello", "another prompt"), request(REQUEST), SCOPE, success(), DRAIN, calls));
        complete(call(cache, REQUEST, request(REQUEST.replace("test-model", "another-model")), SCOPE, success(), DRAIN, calls));
        complete(call(cache, REQUEST, request(REQUEST.replace("hello", "prepared prompt")), SCOPE, success(), DRAIN, calls));
        AiUpstreamRequest credentials = request(REQUEST);
        credentials.setHeaders(Map.of("Authorization", List.of("Bearer other-secret")));
        complete(call(cache, REQUEST, credentials, SCOPE, success(), DRAIN, calls));
        AiUpstreamRequest endpoint = request(REQUEST);
        endpoint.setUri(URI.create("https://other.example/v1/chat/completions"));
        complete(call(cache, REQUEST, endpoint, SCOPE, success(), DRAIN, calls));
        AiUpstreamRequest header = request(REQUEST);
        header.setHeaders(Map.of("Authorization", List.of("Bearer secret"), "X-Model-Version", List.of("2")));
        complete(call(cache, REQUEST, header, SCOPE, success(), DRAIN, calls));
        assertEquals(7, calls.get());
        assertEquals(7, cache.stats().writes());
    }

    @Test
    void testRecursiveCanonicalizationPreservesArrayOrder() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        String first = "{\"model\":\"test-model\",\"logit_bias\":{\"1\":1,\"2\":2},\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}],\"stop\":[\"a\",\"b\"]}";
        String reordered = "{\"stop\":[\"a\",\"b\"],\"messages\":[{\"content\":\"hello\",\"role\":\"user\"}],\"logit_bias\":{\"2\":2,\"1\":1},\"model\":\"test-model\"}";
        complete(call(cache, first, request(first), SCOPE, success(), DRAIN, calls));
        complete(call(cache, reordered, request(reordered), SCOPE, success(), DRAIN, calls));
        String arrayReordered = reordered.replace("[\"a\",\"b\"]", "[\"b\",\"a\"]");
        complete(call(cache, arrayReordered, request(arrayReordered), SCOPE, success(), DRAIN, calls));
        assertEquals(2, calls.get());
        assertEquals(1, cache.stats().hits());
    }

    @Test
    void testDisabledStreamingFallbackAndUnknownProfilesBypass() {
        AtomicInteger calls = new AtomicInteger();
        AiExactResponseCache disabled = new AiExactResponseCache(new AiResponseCacheProperties());
        complete(call(disabled, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        complete(call(disabled, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(2, disabled.stats().bypasses());
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        complete(cache.execute(REQUEST, request(REQUEST), "OpenAI", "openai-chat", "openai-chat", true, false, DRAIN,
                upstream(success(), calls)).contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, SCOPE)));
        complete(cache.execute(REQUEST, request(REQUEST), "OpenAI", "openai-chat", "openai-chat", false, true, DRAIN,
                upstream(success(), calls)).contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, SCOPE)));
        complete(cache.execute(REQUEST, request(REQUEST), "custom", "openai-chat", "openai-chat", false, false, DRAIN,
                upstream(success(), calls)).contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, SCOPE)));
        complete(cache.execute(REQUEST, request(REQUEST), "OpenAI", "openai-responses", "openai-chat", false, false, DRAIN,
                upstream(success(), calls)).contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, SCOPE)));
        complete(cache.execute(REQUEST, request(REQUEST), "OpenAI", "openai-chat", "openai-responses", false, false, DRAIN,
                upstream(success(), calls)).contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, SCOPE)));
        assertEquals(5, cache.stats().bypasses());
        assertEquals(0, cache.stats().writes());
    }

    @Test
    void testToolsMultimodalUnknownDuplicateAndDeepFieldsBypassOriginalAndEffective() {
        List<String> unsupported = List.of(
                REQUEST.replace("\"model\"", "\"tools\":[],\"model\""),
                REQUEST.replace("\"model\"", "\"stream\":true,\"model\""),
                REQUEST.replace("\"model\"", "\"unknown\":false,\"model\""),
                REQUEST.replace("\"hello\"", "[{\"type\":\"text\",\"text\":\"hello\"}]"),
                REQUEST.replace("\"user\"", "\"tool\""),
                REQUEST.replace("\"role\"", "\"name\":\"agent\",\"role\""),
                REQUEST.replace("\"model\"", "\"model\":\"duplicate\",\"model\""),
                REQUEST.replace("\"model\"", "\"temperature\":{},\"model\""),
                REQUEST.replace("\"hello\"", "[".repeat(40) + "0" + "]".repeat(40)),
                REQUEST + "{}");
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        for (String body : unsupported) {
            complete(call(cache, body, request(REQUEST), SCOPE, success(), DRAIN, calls));
            complete(call(cache, REQUEST, request(body), SCOPE, success(), DRAIN, calls));
        }
        assertEquals(unsupported.size() * 2, calls.get());
        assertEquals(unsupported.size() * 2, cache.stats().bypasses());
        assertEquals(0, cache.stats().writes());
    }

    @Test
    void testUncacheableResponseHeadersAndStatusStillForward() {
        List<Map<String, List<String>>> variants = List.of(
                Map.of("Cache-Control", List.of("public, No-Store")),
                Map.of("Cache-Control", List.of("no-cache=\"fields\"")),
                Map.of("Cache-Control", List.of("private")),
                Map.of("Set-Cookie", List.of("session=private")),
                Map.of("Vary", List.of("Accept")),
                Map.of("Content-Encoding", List.of("gzip")),
                Map.of("Content-Type", List.of("text/event-stream")),
                Map.of("Pragma", List.of("no-cache")),
                Map.of("Connection", List.of("Content-Type")));
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        for (Map<String, List<String>> variant : variants) {
            Map<String, List<String>> headers = new LinkedHashMap<>(jsonHeaders());
            headers.putAll(variant);
            AiUpstreamResponse response = response(200, headers, RESPONSE);
            complete(call(cache, REQUEST, request(REQUEST), SCOPE, response, DRAIN, calls));
        }
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, response(500, jsonHeaders(), RESPONSE), DRAIN, calls));
        assertEquals(variants.size() + 1, calls.get());
        assertEquals(0, cache.stats().writes());
    }

    @Test
    void testInvalidErrorTruncatedAndToolResponsesAreNotStored() {
        List<String> bodies = List.of("{", "{\"choices\":[]}", RESPONSE + "{}", RESPONSE.replace("\"stop\"", "\"length\""),
                RESPONSE.replace("\"id\"", "\"error\":{},\"id\""), RESPONSE.replace("\"content\"", "\"tool_calls\":[],\"content\""),
                RESPONSE.replace("\"content\"", "\"reasoning_content\":\"hidden\",\"content\""), RESPONSE.replace("\"hello back\"", "null"));
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        for (String body : bodies) {
            complete(call(cache, REQUEST, request(REQUEST), SCOPE, response(200, jsonHeaders(), body), DRAIN, calls));
        }
        assertEquals(bodies.size(), calls.get());
        assertEquals(0, cache.stats().writes());
    }

    @Test
    void testPassThroughDoesNotWaitForBodyCompletionAndCancellationDoesNotStore() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        AtomicInteger delivered = new AtomicInteger();
        AtomicInteger cancelled = new AtomicInteger();
        AiUpstreamResponse pending = new AiUpstreamResponse(200, jsonHeaders(), Flux.concat(Flux.just(bytes(RESPONSE)), Flux.never())
                .doOnCancel(cancelled::incrementAndGet));
        Mono<Void> operation = call(cache, REQUEST, request(REQUEST), SCOPE, pending,
                response -> response.getBody().doOnNext(ignored -> delivered.incrementAndGet()).then(), calls);
        StepVerifier.create(operation).then(() -> assertEquals(1, delivered.get())).thenCancel().verify();
        assertEquals(1, cancelled.get());
        assertEquals(0, cache.stats().writes());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(2, calls.get());
    }

    @Test
    void testBodyErrorAndEarlyConsumptionNeverStore() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        AiUpstreamResponse failed = new AiUpstreamResponse(200, jsonHeaders(), Flux.concat(Flux.just(bytes(RESPONSE)), Flux.error(new IllegalStateException("upstream"))));
        StepVerifier.create(call(cache, REQUEST, request(REQUEST), SCOPE, failed, DRAIN, calls)).expectErrorMessage("upstream").verify();
        AiUpstreamResponse never = new AiUpstreamResponse(200, jsonHeaders(), Flux.concat(Flux.just(bytes(RESPONSE)), Flux.never()));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, never, response -> response.getBody().take(1).then(), calls));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), ignored -> Mono.empty(), calls));
        assertEquals(0, cache.stats().writes());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(4, calls.get());
    }

    @Test
    void testDownstreamAndOuterTransportErrorsPropagateWithoutRetry() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        Function<AiUpstreamResponse, Mono<Void>> brokenWriter = response -> response.getBody().then(Mono.error(new IllegalStateException("writer")));
        StepVerifier.create(call(cache, REQUEST, request(REQUEST), SCOPE, success(), brokenWriter, calls)).expectErrorMessage("writer").verify();
        assertEquals(1, calls.get());
        assertEquals(0, cache.stats().writes());
        Mono<Void> outerError = cache.execute(REQUEST, request(REQUEST), "OpenAI", "openai-chat", "openai-chat", false, false, DRAIN,
                consumer -> consumer.apply(success()).then(Mono.error(new IllegalStateException("transport"))))
                .contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, SCOPE));
        StepVerifier.create(outerError).expectErrorMessage("transport").verify();
        assertEquals(0, cache.stats().writes());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        StepVerifier.create(call(cache, REQUEST, request(REQUEST), SCOPE, success(), brokenWriter, calls)).expectErrorMessage("writer").verify();
        assertEquals(2, calls.get());
        assertEquals(1, cache.stats().hits());
    }

    @Test
    void testConcurrentFillCapacityIsReleasedAfterCancellationAndSynchronousFailure() {
        AiResponseCacheProperties properties = enabled();
        properties.setMaxConcurrentFills(1);
        AiExactResponseCache cache = new AiExactResponseCache(properties);
        AtomicInteger calls = new AtomicInteger();
        AiUpstreamResponse pending = new AiUpstreamResponse(200, jsonHeaders(), Flux.never());
        StepVerifier.create(call(cache, REQUEST, request(REQUEST), SCOPE, pending, DRAIN, calls))
                .then(() -> complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls)))
                .thenCancel().verify();
        assertEquals(1, cache.stats().bypasses());
        assertEquals(0, cache.stats().writes());
        StepVerifier.create(call(cache, REQUEST, request(REQUEST), SCOPE, success(), ignored -> {
            throw new IllegalStateException("sync writer");
        }, calls)).expectErrorMessage("sync writer").verify();
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(4, calls.get());
        assertEquals(1, cache.stats().writes());
        assertEquals(1, cache.stats().hits());
    }

    @Test
    void testTtlAndInvalidationRejectInflightPublication() {
        AtomicLong clock = new AtomicLong();
        AiResponseCacheProperties properties = enabled();
        properties.setTtlSeconds(1);
        AiExactResponseCache cache = new AiExactResponseCache(properties, clock::get);
        AtomicInteger calls = new AtomicInteger();
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        clock.addAndGet(Duration.ofSeconds(1).toNanos());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(2, calls.get());
        cache.invalidateAll();
        Sinks.One<Void> release = Sinks.one();
        StepVerifier.create(call(cache, REQUEST, request(REQUEST), SCOPE, success(), response -> response.getBody().then(release.asMono()), calls))
                .then(cache::invalidateAll).then(() -> release.tryEmitEmpty()).verifyComplete();
        assertEquals(2, cache.stats().writes());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(4, calls.get());
    }

    @Test
    void testCaptureLimitsContinueForwardingAndIncludeHeaderBudget() {
        AiResponseCacheProperties properties = enabled();
        properties.setMaxEntryBytes(1024);
        AiExactResponseCache cache = new AiExactResponseCache(properties);
        AtomicInteger calls = new AtomicInteger();
        String oversized = RESPONSE.replace("hello back", "x".repeat(2000));
        AtomicInteger received = new AtomicInteger();
        AiUpstreamResponse chunked = new AiUpstreamResponse(200, jsonHeaders(), Flux.just(bytes(oversized.substring(0, 700)), bytes(oversized.substring(700))));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, chunked,
                response -> response.getBody().doOnNext(chunk -> received.addAndGet(chunk.length)).then(), calls));
        assertEquals(bytes(oversized).length, received.get());
        Map<String, List<String>> headers = new LinkedHashMap<>(jsonHeaders());
        headers.put("Content-Language", List.of("en".repeat(600)));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, response(200, headers, RESPONSE), DRAIN, calls));
        assertEquals(0, cache.stats().writes());
        properties.setMaxRequestBytes(20);
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(1, cache.stats().bypasses());
    }

    @Test
    void testTotalWeightIsBoundedAndOversizeEntriesCannotBeAdmitted() {
        AiResponseCacheProperties properties = enabled();
        properties.setMaxWeightBytes(1600);
        AiExactResponseCache cache = new AiExactResponseCache(properties);
        AtomicInteger calls = new AtomicInteger();
        for (int index = 0; index < 10; index++) {
            String body = REQUEST.replace("hello", "request-" + index);
            complete(call(cache, body, request(body), SCOPE, success(), DRAIN, calls));
        }
        Cache<?, ?> store = (Cache<?, ?>) ReflectionTestUtils.getField(cache, "cache");
        store.cleanUp();
        assertTrue(store.policy().eviction().orElseThrow().weightedSize().orElseThrow() <= 1600);
        assertTrue(store.estimatedSize() < 10);
        AiResponseCacheProperties tiny = enabled();
        tiny.setMaxWeightBytes(100);
        AiExactResponseCache noRoom = new AiExactResponseCache(tiny);
        complete(call(noRoom, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(0, noRoom.stats().writes());
    }

    @Test
    void testStoredAndReplayedArraysCannotBeMutatedAcrossRequestsOrSubscriptions() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        byte[] original = bytes(RESPONSE);
        AiUpstreamResponse source = new AiUpstreamResponse(200, jsonHeaders(), Flux.just(original));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, source, DRAIN, calls));
        Arrays.fill(original, (byte) 'x');
        List<byte[]> observed = new ArrayList<>();
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, source, response -> response.getBody().doOnNext(chunk -> {
            observed.add(chunk.clone());
            Arrays.fill(chunk, (byte) 'y');
        }).then(response.getBody().doOnNext(observed::add).then()), calls));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, source, response -> response.getBody().doOnNext(observed::add).then(), calls));
        observed.forEach(value -> assertArrayEquals(bytes(RESPONSE), value));
        assertEquals(3, observed.size());
        assertEquals(1, calls.get());
    }

    @Test
    void testStoreFailuresOnlyBypassOrDropPublication() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        Cache<Object, Object> store = mock(Cache.class);
        ReflectionTestUtils.setField(cache, "cache", store);
        AtomicInteger calls = new AtomicInteger();
        when(store.getIfPresent(any())).thenThrow(new IllegalStateException("cache read"));
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(1, calls.get());
        assertEquals(1, cache.stats().bypasses());
        doReturn(null).when(store).getIfPresent(any());
        doThrow(new IllegalStateException("cache write")).when(store).put(any(), any());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(2, calls.get());
        assertEquals(0, cache.stats().writes());
    }

    @Test
    void testOriginalModelCanBeResolvedByProviderButEffectiveModelIsRequired() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        String withoutModel = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";
        complete(call(cache, withoutModel, request(REQUEST), SCOPE, success(), DRAIN, calls));
        complete(call(cache, withoutModel, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(1, calls.get());
        assertEquals(1, cache.stats().hits());
        complete(call(cache, REQUEST, request(withoutModel), SCOPE, success(), DRAIN, calls));
        complete(call(cache, REQUEST.replace("\"test-model\"", "null"), request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(3, calls.get());
        assertEquals(2, cache.stats().bypasses());
    }

    @Test
    void testInvalidationDuringLookupCannotAssignNewGenerationToOldRequest() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        Cache<Object, Object> store = mock(Cache.class);
        ReflectionTestUtils.setField(cache, "cache", store);
        AtomicInteger calls = new AtomicInteger();
        when(store.getIfPresent(any())).thenAnswer(invocation -> {
            cache.invalidateAll();
            return null;
        }).thenReturn(null);
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(0, cache.stats().writes());
        complete(call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls));
        assertEquals(1, cache.stats().writes());
        assertEquals(2, calls.get());
    }

    @Test
    void testReSubscriptionDoesNotReuseFillState() {
        AiExactResponseCache cache = new AiExactResponseCache(enabled());
        AtomicInteger calls = new AtomicInteger();
        Mono<Void> execution = call(cache, REQUEST, request(REQUEST), SCOPE, success(), DRAIN, calls);
        complete(execution);
        complete(execution);
        assertEquals(1, calls.get());
        assertFalse(cache.stats().writes() > 1);
    }

    private AiResponseCacheProperties enabled() {
        AiResponseCacheProperties properties = new AiResponseCacheProperties();
        properties.setEnabled(true);
        return properties;
    }

    private AiUpstreamRequest request(final String body) {
        AiUpstreamRequest request = new AiUpstreamRequest();
        request.setUri(URI.create("https://upstream.example/v1/chat/completions"));
        request.setMethod("POST");
        request.setHeaders(Map.of("Authorization", List.of("Bearer secret")));
        request.setBody(bytes(body));
        return request;
    }

    private AiUpstreamResponse success() {
        return response(200, jsonHeaders(), RESPONSE);
    }

    private AiUpstreamResponse response(final int status, final Map<String, List<String>> headers, final String body) {
        return new AiUpstreamResponse(status, headers, Flux.defer(() -> Flux.just(bytes(body))));
    }

    private Map<String, List<String>> jsonHeaders() {
        return Map.of("Content-Type", List.of("application/json; charset=utf-8"));
    }

    private byte[] bytes(final String text) {
        return text.getBytes(StandardCharsets.UTF_8);
    }

    private Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream(final AiUpstreamResponse response, final AtomicInteger calls) {
        return consumer -> {
            calls.incrementAndGet();
            return consumer.apply(response);
        };
    }

    private Mono<Void> call(final AiExactResponseCache cache, final String original, final AiUpstreamRequest prepared,
                            final AiResponseCacheScope scope, final AiUpstreamResponse response,
                            final Function<AiUpstreamResponse, Mono<Void>> consumer, final AtomicInteger calls) {
        Mono<Void> operation = cache.execute(original, prepared, "OpenAI", "openai-chat", "openai-chat", false, false, consumer, upstream(response, calls));
        return Objects.isNull(scope) ? operation : operation.contextWrite(Context.of(AiResponseCacheScope.REACTOR_KEY, scope));
    }

    private void complete(final Mono<Void> operation) {
        StepVerifier.create(operation).verifyComplete();
    }
}
