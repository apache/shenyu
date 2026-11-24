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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.test.publisher.TestPublisher;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProxyCredentialFailureInspectorTest {

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 16383, 16384, 16385, 65536})
    void forwardsEveryByteAndSubscribesOnceAcrossBudgetBoundary(final int size) {
        byte[] original = new byte[size];
        Arrays.fill(original, (byte) 'x');
        AtomicInteger subscriptions = new AtomicInteger();
        AtomicInteger classifications = new AtomicInteger();
        ByteArrayOutputStream received = new ByteArrayOutputStream();
        Flux<byte[]> source = Flux.defer(() -> {
            assertEquals(1, subscriptions.incrementAndGet());
            return Flux.just(Arrays.copyOfRange(original, 0, size / 2), Arrays.copyOfRange(original, size / 2, size));
        });
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(source), body -> {
            classifications.incrementAndGet();
            assertArrayEquals(original, body);
            return false;
        }, inspection -> {
            assertFalse(inspection.credentialFailure());
            assertEquals(401, inspection.response().getStatusCode());
            assertEquals(List.of("kept"), inspection.response().getHeaders().get("X-Fixture"));
            return inspection.response().getBody().doOnNext(received::writeBytes).then();
        })).verifyComplete();
        assertEquals(size <= AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES ? 1 : 0, classifications.get());
        assertEquals(1, subscriptions.get());
        assertArrayEquals(original, received.toByteArray());
    }

    @Test
    void waitsForCompletionAndPreservesFragmentedUtf8() {
        byte[] original = "{\"error\":{\"code\":\"credential_revoked\",\"message\":\"凭证已撤销\"}}".getBytes(StandardCharsets.UTF_8);
        TestPublisher<byte[]> source = TestPublisher.create();
        AtomicBoolean consumed = new AtomicBoolean();
        ByteArrayOutputStream received = new ByteArrayOutputStream();
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(source.flux()), body -> {
            assertArrayEquals(original, body);
            return true;
        }, inspection -> {
            consumed.set(true);
            assertTrue(inspection.credentialFailure());
            return inspection.response().getBody().doOnNext(received::writeBytes).then();
        })).then(() -> {
            for (byte value : original) {
                source.next(new byte[] {value});
            }
            assertFalse(consumed.get());
            source.complete();
        }).verifyComplete();
        source.assertSubscribers(0).assertWasSubscribed();
        assertArrayEquals(original, received.toByteArray());
    }

    @Test
    void emptyBodyCompletesWithoutInventingAnOnNext() {
        AtomicInteger chunks = new AtomicInteger();
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(Flux.empty()), body -> false,
                inspection -> inspection.response().getBody().doOnNext(bytes -> chunks.incrementAndGet()).then())).verifyComplete();
        assertEquals(0, chunks.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "prefix"})
    void preservesPrefixIncludingEmptyOnNextBeforeOriginalError(final String prefix) {
        IllegalStateException failure = new IllegalStateException("fixture");
        AtomicInteger classifications = new AtomicInteger();
        AtomicInteger chunks = new AtomicInteger();
        ByteArrayOutputStream received = new ByteArrayOutputStream();
        Flux<byte[]> source = Flux.just(prefix.getBytes(StandardCharsets.UTF_8)).concatWith(Flux.error(failure));
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(source), body -> {
            classifications.incrementAndGet();
            return true;
        }, inspection -> {
            assertFalse(inspection.credentialFailure());
            return inspection.response().getBody().doOnNext(bytes -> {
                chunks.incrementAndGet();
                received.writeBytes(bytes);
            }).then();
        })).expectErrorMatches(error -> error == failure).verify();
        assertEquals(0, classifications.get());
        assertEquals(1, chunks.get());
        assertArrayEquals(prefix.getBytes(StandardCharsets.UTF_8), received.toByteArray());
    }

    @Test
    void preservesErrorBeforeFirstChunk() {
        IllegalStateException failure = new IllegalStateException("fixture");
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(Flux.error(failure)), body -> true,
                inspection -> inspection.response().getBody().then()))
                .expectErrorMatches(error -> error == failure).verify();
    }

    @Test
    void cancelsOriginalBodyDuringInspection() {
        TestPublisher<byte[]> source = TestPublisher.create();
        AtomicBoolean consumed = new AtomicBoolean();
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(source.flux()), body -> true, inspection -> {
            consumed.set(true);
            return inspection.response().getBody().then();
        })).then(() -> source.next(new byte[] {1})).thenCancel().verify();
        source.assertCancelled().assertWasSubscribed();
        assertFalse(consumed.get());
    }

    @Test
    void releasesOversizedPrefixWithoutWaitingForTailAndCancelsAtDownstreamBoundary() {
        AtomicInteger readChunks = new AtomicInteger();
        AtomicBoolean cancelled = new AtomicBoolean();
        byte[] large = new byte[AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES + 1];
        Flux<byte[]> source = Flux.concat(Flux.just(new byte[] {1}, large), Flux.range(0, 1000).map(i -> new byte[] {2}))
                .doOnNext(bytes -> readChunks.incrementAndGet()).doOnCancel(() -> cancelled.set(true));
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(source), body -> {
            throw new AssertionError("oversized body must not be classified");
        }, inspection -> inspection.response().getBody().take(1).then())).verifyComplete();
        assertTrue(cancelled.get());
        assertTrue(readChunks.get() <= 3, "tail must not be drained or buffered");
    }

    @Test
    void consumerCompletionCancelsUnconsumedTail() {
        TestPublisher<byte[]> source = TestPublisher.create();
        StepVerifier.create(AiProxyCredentialFailureInspector.inspect(response(source.flux()), body -> false,
                inspection -> Mono.empty()))
                .then(() -> source.next(new byte[AiProxyCredentialFailureInspector.MAX_INSPECTION_BYTES + 1]))
                .verifyComplete();
        source.assertCancelled();
    }

    private AiUpstreamResponse response(final Flux<byte[]> body) {
        return new AiUpstreamResponse(401, Map.of("X-Fixture", List.of("kept")), body);
    }
}
