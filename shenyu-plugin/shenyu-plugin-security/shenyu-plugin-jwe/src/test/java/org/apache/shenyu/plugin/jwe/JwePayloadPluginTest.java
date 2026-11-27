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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.jose4j.jwe.JsonWebEncryption;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.test.publisher.TestPublisher;

import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;

class JwePayloadPluginTest {

    private static final byte[] JSON = "{ \"value\": 1 }\n".getBytes(StandardCharsets.UTF_8);

    private static KeyPair gateway;

    private static KeyPair client;

    @BeforeAll
    static void keys() throws GeneralSecurityException {
        KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
        generator.initialize(2048);
        gateway = generator.generateKeyPair();
        client = generator.generateKeyPair();
    }

    @Test
    void decryptsExactBytesAndEncryptsForDifferentRecipientPreservingStatus() throws Exception {
        MockServerWebExchange exchange = incoming(compact(gateway, JSON));
        StepVerifier.create(run(exchange, both(), forwarded -> DataBufferUtils.join(forwarded.getRequest().getBody()).flatMap(buffer -> {
            byte[] clear = new byte[buffer.readableByteCount()];
            buffer.read(clear);
            DataBufferUtils.release(buffer);
            assertArrayEquals(JSON, clear);
            assertEquals(MediaType.APPLICATION_JSON, forwarded.getRequest().getHeaders().getContentType());
            forwarded.getResponse().setStatusCode(HttpStatus.CREATED);
            forwarded.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return forwarded.getResponse().writeWith(Mono.just(forwarded.getResponse().bufferFactory().wrap(clear)));
        }))).verifyComplete();
        assertEquals(HttpStatus.CREATED, exchange.getResponse().getStatusCode());
        assertEquals("application/jose", exchange.getResponse().getHeaders().getContentType().toString());
        JsonWebEncryption jwe = new JsonWebEncryption();
        jwe.setCompactSerialization(exchange.getResponse().getBodyAsString().block());
        jwe.setKey(client.getPrivate());
        assertArrayEquals(JSON, jwe.getPlaintextBytes());
    }

    @Test
    void rejectsWrongKeyMalformedAndNonJsonBeforeForwarding() throws Exception {
        for (String input : new String[]{"bad", compact(client, JSON), compact(gateway, "abc".getBytes(StandardCharsets.UTF_8))}) {
            MockServerWebExchange exchange = incoming(input);
            StepVerifier.create(run(exchange, both(), ignored -> fail("unexpected forward"))).verifyComplete();
            assertEquals(HttpStatus.BAD_REQUEST, exchange.getResponse().getStatusCode());
        }
    }

    @Test
    void rejectsOversizeRequest() {
        MockServerWebExchange exchange = incoming("x".repeat(1024 * 1024 + 1));
        StepVerifier.create(run(exchange, both(), ignored -> fail("unexpected forward"))).verifyComplete();
        assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, exchange.getResponse().getStatusCode());
    }

    @Test
    void responseOnlyAcceptsPlainRequestAndNeverLeaksInvalidUpstreamBody() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/echo").contentType(MediaType.APPLICATION_JSON).body("{}"));
        JweRoute outbound = new JweRoute(null, (RSAPublicKey) client.getPublic());
        StepVerifier.create(run(exchange, outbound, forwarded -> {
            forwarded.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            DataBuffer secret = forwarded.getResponse().bufferFactory().wrap("private invalid JSON".getBytes(StandardCharsets.UTF_8));
            return forwarded.getResponse().writeWith(Mono.just(secret));
        })).verifyComplete();
        assertEquals(HttpStatus.BAD_GATEWAY, exchange.getResponse().getStatusCode());
        assertFalse(exchange.getResponse().getBodyAsString().block().contains("private"));
    }

    @Test
    void streamingAndUnsupportedMethodsFailClosed() throws Exception {
        MockServerWebExchange exchange = incoming(compact(gateway, JSON));
        TestPublisher<Flux<DataBuffer>> idle = TestPublisher.create();
        StepVerifier.create(run(exchange, both(), forwarded -> forwarded.getResponse().writeAndFlushWith(idle.flux())))
                .expectComplete().verify(java.time.Duration.ofSeconds(5));
        idle.assertCancelled();
        assertEquals(HttpStatus.BAD_GATEWAY, exchange.getResponse().getStatusCode());
        MockServerWebExchange get = MockServerWebExchange.from(MockServerHttpRequest.get("/echo"));
        StepVerifier.create(run(get, both(), ignored -> fail("unexpected forward"))).verifyComplete();
        assertEquals(HttpStatus.METHOD_NOT_ALLOWED, get.getResponse().getStatusCode());
    }

    @Test
    void oversizedResponseDoesNotCommitAnyUpstreamBytes() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/echo").contentType(MediaType.APPLICATION_JSON).body("{}"));
        StepVerifier.create(run(exchange, new JweRoute(null, (RSAPublicKey) client.getPublic()), forwarded -> {
            forwarded.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return forwarded.getResponse().writeWith(Mono.just(forwarded.getResponse().bufferFactory().wrap(new byte[1024 * 1024 + 1])));
        })).verifyComplete();
        assertEquals(HttpStatus.BAD_GATEWAY, exchange.getResponse().getStatusCode());
        assertEquals("", exchange.getResponse().getBodyAsString().block());
    }

    @Test
    void requestOnlyLeavesResponseUnwrapped() throws Exception {
        MockServerWebExchange exchange = incoming(compact(gateway, JSON));
        StepVerifier.create(run(exchange, new JweRoute((RSAPrivateKey) gateway.getPrivate(), null), forwarded ->
                forwarded.getResponse().writeWith(Mono.just(forwarded.getResponse().bufferFactory().wrap(JSON))))).verifyComplete();
        assertEquals(new String(JSON, StandardCharsets.UTF_8), exchange.getResponse().getBodyAsString().block());
    }

    private Mono<Void> run(final MockServerWebExchange exchange, final JweRoute route, final ShenyuPluginChain chain) {
        return new JwePayloadPlugin(Map.of("route", route)).doExecute(exchange, chain, null, RuleData.builder().id("route").build());
    }

    private JweRoute both() {
        return new JweRoute((RSAPrivateKey) gateway.getPrivate(), (RSAPublicKey) client.getPublic());
    }

    private MockServerWebExchange incoming(final String token) {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/echo").header(HttpHeaders.CONTENT_TYPE, "application/jose").body(token));
    }

    private String compact(final KeyPair pair, final byte[] clear) throws Exception {
        JsonWebEncryption jwe = new JsonWebEncryption();
        jwe.setAlgorithmHeaderValue("RSA-OAEP-256");
        jwe.setEncryptionMethodHeaderParameter("A256GCM");
        jwe.setContentTypeHeaderValue("application/json");
        jwe.setKey(pair.getPublic());
        jwe.setPlaintext(clear);
        return jwe.getCompactSerialization();
    }
}
