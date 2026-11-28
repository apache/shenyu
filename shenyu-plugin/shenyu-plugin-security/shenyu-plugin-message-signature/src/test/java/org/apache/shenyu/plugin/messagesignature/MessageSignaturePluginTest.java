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

import com.authlete.hms.SignatureInputField;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rewrite.RewritePlugin;
import org.apache.shenyu.plugin.rewrite.handler.RewritePluginDataHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
import java.time.Instant;
import java.util.Base64;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class MessageSignaturePluginTest {

    private static final String TARGET = "https://example.com/echo?a=%2F";

    private static final String BODY = "{ \"name\" : \"café\" }\n";

    private static KeyPair pair;

    @BeforeAll
    static void keys() throws GeneralSecurityException {
        pair = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
    }

    @Test
    void verifiesPublishedRfc9421AppendixB26CryptographicVector() throws GeneralSecurityException {
        String input = "sig-b26=(\"date\" \"@method\" \"@path\" \"@" + "authority\" \"content-type\" \"content-length\");created=1618884473;keyid=\"test-key-ed25519\"";
        String sig = "wqcAqbmYJ2ji2glfAMaRy4gruYYnx2nEFN2HN6jrnDnQCK1u02Gb04v9EDgwUPiu4A0w6vuQv5lIp5WPpBKRCw==";
        PublicKey key = KeyFactory.getInstance("Ed25519").generatePublic(new X509EncodedKeySpec(
                Base64.getDecoder().decode("MCowBQYDK2VwAyEAJrQLj5P/89iXES9+vFgrIy29clF9CC/oPPsw3c5D0bs=")));
        MockServerHttpRequest request = MockServerHttpRequest.post("https://example.com/foo")
                .header("Date", "Tue, 20 Apr 2021 02:07:55 GMT").header("Content-Type", "application/json").header("Content-Length", "18").build();
        assertTrue(MessageSignatureVerifier.verifySignature(request, SignatureInputField.parse(input).get("sig-b26"), Base64.getDecoder().decode(sig), key));
    }

    @Test
    void forwardsOriginalBytesOnSuccess() throws Exception {
        MockServerWebExchange exchange = request(BODY, "application/json", Instant.now().getEpochSecond(), true);
        StepVerifier.create(run(plugin(null), exchange, forwarded -> DataBufferUtils.join(forwarded.getRequest().getBody()).doOnNext(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            DataBufferUtils.release(buffer);
            assertArrayEquals(BODY.getBytes(StandardCharsets.UTF_8), bytes);
        }).then())).verifyComplete();
    }

    @Test
    void badDigestExpiryAndMissingCoverageDoNotReserveNonce() throws Exception {
        NonceStore unused = (key, nonce, ttl) -> {
            throw new AssertionError("invalid input reached replay store");
        };
        long now = Instant.now().getEpochSecond();
        for (MockServerWebExchange exchange : new MockServerWebExchange[]{
            request("{}", "application/json", now, true), request(BODY, "application/json", now - 600, true),
            request(BODY, "application/json", now, false), request(BODY, "text/plain", now, true)}) {
            StepVerifier.create(run(plugin(unused), exchange, ignored -> fail("unexpected forward"))).verifyComplete();
            assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        }
    }

    @Test
    void twoGatewaysOnlyForwardOneConcurrentDelivery() throws Exception {
        Set<String> seen = ConcurrentHashMap.newKeySet();
        NonceStore shared = (key, nonce, ttl) -> Mono.fromSupplier(() -> seen.add(key + ":" + nonce));
        long now = Instant.now().getEpochSecond();
        AtomicInteger forwards = new AtomicInteger();
        ShenyuPluginChain chain = ignored -> Mono.fromRunnable(forwards::incrementAndGet);
        MockServerWebExchange a = request(BODY, "application/json", now, true);
        MockServerWebExchange b = request(BODY, "application/json", now, true);
        StepVerifier.create(Mono.when(run(plugin(shared), a, chain).subscribeOn(Schedulers.parallel()),
                run(plugin(shared), b, chain).subscribeOn(Schedulers.parallel()))).verifyComplete();
        assertEquals(1, forwards.get());
        assertTrue(a.getResponse().getStatusCode() == HttpStatus.UNAUTHORIZED || b.getResponse().getStatusCode() == HttpStatus.UNAUTHORIZED);
    }

    @Test
    void doesNotTrustAValidSignatureOnAnotherTarget() throws Exception {
        MockServerWebExchange original = request(BODY, "application/json", Instant.now().getEpochSecond(), true);
        MockServerWebExchange altered = MockServerWebExchange.from(MockServerHttpRequest.post("https://example.com/other")
                .headers(original.getRequest().getHeaders()).body(BODY));
        StepVerifier.create(run(plugin(null), altered, ignored -> fail("unexpected forward"))).verifyComplete();
        assertEquals(HttpStatus.UNAUTHORIZED, altered.getResponse().getStatusCode());
    }

    @Test
    void verifiedOriginalTargetIsRewrittenOnlyAfterVerification() throws Exception {
        MockServerWebExchange signed = request(BODY, "application/json", Instant.now().getEpochSecond(), true);
        RuleData rule = RuleData.builder().id("rewrite-signed").selectorId("webhooks").handle("{}").build();
        String cacheKey = CacheKeyUtils.INST.getKey(rule);
        RewriteHandle handle = new RewriteHandle();
        handle.setRegex("^/echo$");
        handle.setReplace("/internal/events");
        RewritePluginDataHandler.CACHED_HANDLE.get().cachedHandle(cacheKey, handle);
        MatchedRewrite rewrite = new MatchedRewrite();
        try {
            MessageSignaturePlugin signature = plugin(null);
            assertTrue(signature.getOrder() < rewrite.getOrder());
            StepVerifier.create(run(signature, signed, exchange -> rewrite.apply(exchange, rule))).verifyComplete();
            assertEquals("/internal/events", signed.getAttribute(Constants.REWRITE_URI));
            MockServerWebExchange altered = MockServerWebExchange.from(MockServerHttpRequest.post("https://example.com/internal/events")
                    .headers(signed.getRequest().getHeaders()).body(BODY));
            StepVerifier.create(run(signature, altered, ignored -> fail("rewrite ran before verification"))).verifyComplete();
            assertEquals(HttpStatus.UNAUTHORIZED, altered.getResponse().getStatusCode());
        } finally {
            RewritePluginDataHandler.CACHED_HANDLE.get().removeHandle(cacheKey);
        }
    }

    @Test
    void missingRequiredStoreDoesNotDowngradeToVerificationOnly() throws Exception {
        MockServerWebExchange exchange = request(BODY, "application/json", Instant.now().getEpochSecond(), true);
        MessageSignaturePlugin plugin = new MessageSignaturePlugin(Map.of("rule", MessageSignatureVerifier.replayProtected("partner", pair.getPublic(), null)));
        StepVerifier.create(run(plugin, exchange, ignored -> fail("unexpected forward"))).verifyComplete();
        assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode());
    }

    @Test
    void oversizedBodyDoesNotReachTheChain() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post(TARGET).body("x".repeat(1024 * 1024 + 1)));
        StepVerifier.create(run(plugin(null), exchange, ignored -> fail("unexpected forward"))).verifyComplete();
        assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, exchange.getResponse().getStatusCode());
    }

    @Test
    void unavailableStoreFailsClosed() throws Exception {
        MockServerWebExchange exchange = request(BODY, "application/json", Instant.now().getEpochSecond(), true);
        StepVerifier.create(run(plugin((key, nonce, ttl) -> Mono.error(new IllegalStateException())), exchange, ignored -> fail("unexpected forward"))).verifyComplete();
        assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode());
    }

    private MessageSignaturePlugin plugin(final NonceStore store) {
        return new MessageSignaturePlugin(Map.of("rule", Objects.isNull(store) ? MessageSignatureVerifier.verificationOnly("partner", pair.getPublic())
                : MessageSignatureVerifier.replayProtected("partner", pair.getPublic(), store)));
    }

    private Mono<Void> run(final MessageSignaturePlugin plugin, final MockServerWebExchange exchange, final ShenyuPluginChain chain) {
        return plugin.doExecute(exchange, chain, null, RuleData.builder().id("rule").build());
    }

    private MockServerWebExchange request(final String body, final String contentType, final long created, final boolean coverDigest) throws GeneralSecurityException {
        String digest = "sha-256=:" + Base64.getEncoder().encodeToString(MessageDigest.getInstance("SHA-256").digest(BODY.getBytes(StandardCharsets.UTF_8))) + ":";
        String params = "(\"@method\" \"@target-uri\" \"content-type\"" + (coverDigest ? " \"content-digest\"" : "")
                + ");created=" + created + ";expires=" + (created + 60) + ";keyid=\"partner\";alg=\"ed25519\";nonce=\"once\"";
        String base = "\"@method\": POST\n\"@target-uri\": " + TARGET + "\n\"content-type\": application/json\n"
                + (coverDigest ? "\"content-digest\": " + digest + "\n" : "") + "\"@signature-params\": " + params;
        Signature signer = Signature.getInstance("Ed25519");
        signer.initSign(pair.getPrivate());
        signer.update(base.getBytes(StandardCharsets.UTF_8));
        return MockServerWebExchange.from(MockServerHttpRequest.method(org.springframework.http.HttpMethod.POST, java.net.URI.create(TARGET))
                .header("Signature-Input", "sig1=" + params).header("Signature", "sig1=:" + Base64.getEncoder().encodeToString(signer.sign()) + ":")
                .header("Content-Digest", digest).header("Content-Type", contentType).body(body));
    }

    private static final class MatchedRewrite extends RewritePlugin {
        private Mono<Void> apply(final ServerWebExchange exchange, final RuleData rule) {
            return doExecute(exchange, ignored -> Mono.empty(), null, rule);
        }
    }
}
