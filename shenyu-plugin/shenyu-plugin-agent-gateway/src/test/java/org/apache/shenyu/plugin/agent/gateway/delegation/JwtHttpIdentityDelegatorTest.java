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

package org.apache.shenyu.plugin.agent.gateway.delegation;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException.Reason;
import org.apache.shenyu.plugin.agent.gateway.delegation.DelegationSigningKeyResolver.SigningKey;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.Signature;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Base64;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class JwtHttpIdentityDelegatorTest {

    private static final ObjectMapper JSON = new ObjectMapper();

    private static final Instant NOW = Instant.parse("2026-10-01T00:00:00.125Z");

    private static final Clock CLOCK = Clock.fixed(NOW, ZoneOffset.UTC);

    @TempDir
    private Path root;

    private Path directory;

    private Path keyFile;

    private KeyPair keyPair;

    private FileDelegationSigningKeyResolver resolver;

    private JwtHttpIdentityDelegator signer;

    @BeforeEach
    void configureKey() throws Exception {
        directory = Files.createDirectory(root.resolve("keys"));
        keyFile = Files.createDirectory(directory.resolve("orders")).resolve("v1.json");
        keyPair = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
        Files.writeString(keyFile, document().toString());
        resolver = new FileDelegationSigningKeyResolver(directory);
        signer = new JwtHttpIdentityDelegator(resolver, CLOCK);
    }

    @Test
    void signsWithBoundIssuerAudienceScopesRouteAndPolicy() throws Exception {
        DelegationRequest request = request(true, NOW.plusSeconds(60), "tenant-a");
        DelegatedCredential credential = signer.delegate(request).block();
        String[] compact = credential.getBearerToken().split("\\.");
        assertEquals(3, compact.length);
        Signature verifier = Signature.getInstance("Ed25519");
        verifier.initVerify(keyPair.getPublic());
        verifier.update((compact[0] + "." + compact[1]).getBytes(StandardCharsets.US_ASCII));
        assertTrue(verifier.verify(Base64.getUrlDecoder().decode(compact[2])));
        JsonNode header = payload(compact[0]);
        assertEquals(JSON.createObjectNode().put("typ", "JWT").put("alg", "EdDSA").put("kid", "orders:v1"), header);
        JsonNode claims = payload(compact[1]);
        assertEquals("orders-issuer", claims.path("iss").textValue());
        assertEquals("private-subject", claims.path("sub").textValue());
        assertEquals("tenant-a", claims.path("tenant").textValue());
        assertEquals("orders-api", claims.path("aud").textValue());
        assertEquals("orders:read data", claims.path("scope").textValue());
        assertEquals("lookup", claims.path("tool").textValue());
        assertEquals("POST /api/orders/lookup", claims.path("route").textValue());
        assertEquals("rule-1:23", claims.path("policy_version").textValue());
        assertEquals(NOW.getEpochSecond(), claims.path("iat").longValue());
        assertEquals(NOW.getEpochSecond(), claims.path("nbf").longValue());
        assertExpiry(NOW.plusSeconds(30), credential, claims);
        String id = claims.path("jti").textValue();
        assertEquals(id, UUID.fromString(id).toString());
        assertNotEquals(id, claims(signer.delegate(request).block()).path("jti").textValue());
        assertEquals("DelegatedCredential[REDACTED]", credential.toString());
        assertEquals("SigningKey[REDACTED]", resolver.resolve(request).toString());
        assertFalse(request.toString().contains("private-subject"));
        assertFalse(request.toString().contains("tenant-a"));
    }

    @Test
    void supportsOptionalTenantAndClipsExpiryToSubSecondDeadline() throws Exception {
        Instant deadline = NOW.plusMillis(250);
        DelegatedCredential issued = signer.delegate(request(false, deadline, null)).block();
        JsonNode claims = claims(issued);
        assertFalse(claims.has("tenant"));
        assertExpiry(deadline, issued, claims);
    }

    @Test
    void capsAssertionAtSigningKeyExpiry() throws Exception {
        Clock clock = Clock.fixed(Instant.now(), ZoneOffset.UTC);
        Instant expiry = Instant.ofEpochSecond(clock.instant().getEpochSecond() + 15);
        Files.writeString(keyFile, document().put("expiresAt", expiry.toString()).toString());
        DelegatedCredential issued = new JwtHttpIdentityDelegator(resolver, clock).delegate(request(true, expiry.plusSeconds(60), "tenant-a")).block();
        assertExpiry(expiry, issued, claims(issued));
        assertTrue(claims(issued).get("exp").isIntegralNumber());
    }

    @Test
    void rejectsMissingTenantAndElapsedDeadlineBeforeReadingKey() {
        AtomicBoolean resolved = new AtomicBoolean();
        JwtHttpIdentityDelegator counting = new JwtHttpIdentityDelegator(request -> {
            resolved.set(true);
            return resolver.resolve(request);
        }, CLOCK);
        assertFailure(Reason.TENANT_MISSING, counting.delegate(request(true, NOW.plusSeconds(1), null)));
        assertFailure(Reason.DEADLINE_EXCEEDED, counting.delegate(request(false, NOW, null)));
        assertFalse(resolved.get());
    }

    @Test
    void checksDeadlineAgainAfterResolutionAndReadsOffEventLoop() {
        AtomicReference<Thread> worker = new AtomicReference<>();
        Clock clock = mock(Clock.class);
        Instant deadline = NOW.plusMillis(500);
        when(clock.instant()).thenReturn(NOW, deadline);
        JwtHttpIdentityDelegator slow = new JwtHttpIdentityDelegator(request -> {
            worker.set(Thread.currentThread());
            return resolver.resolve(request);
        }, clock);
        assertFailure(Reason.DEADLINE_EXCEEDED, slow.delegate(request(true, deadline, "tenant-a")));
        assertNotEquals(Thread.currentThread(), worker.get());
    }

    @Test
    void cancellingSubscriptionInterruptsAnInFlightKeyResolution() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch interrupted = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        JwtHttpIdentityDelegator delayed = new JwtHttpIdentityDelegator(request -> {
            entered.countDown();
            try {
                release.await();
            } catch (InterruptedException cancelled) {
                interrupted.countDown();
                Thread.currentThread().interrupt();
            }
            return new SigningKey("issuer", keyPair.getPrivate());
        }, CLOCK);
        AtomicBoolean delivered = new AtomicBoolean();
        Disposable call = delayed.delegate(request(true, NOW.plusSeconds(60), "tenant-a")).subscribe(credential -> delivered.set(true));
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            call.dispose();
            assertTrue(interrupted.await(5, TimeUnit.SECONDS));
            assertFalse(delivered.get());
        } finally {
            release.countDown();
            call.dispose();
        }
    }

    @Test
    void everySubscriptionSeesRevocationRemovalAndReplacement() throws Exception {
        Mono<DelegatedCredential> pending = signer.delegate(request(true, NOW.plusSeconds(60), "tenant-a"));
        final DelegatedCredential first = pending.block();
        Files.writeString(keyFile, document().put("revoked", true).toString());
        assertFailure(Reason.REVOKED_OR_EXPIRED, pending);
        Files.writeString(keyFile, document().put("expiresAt", Instant.EPOCH.toString()).toString());
        assertFailure(Reason.REVOKED_OR_EXPIRED, pending);
        Files.delete(keyFile);
        assertFailure(Reason.KEY_UNAVAILABLE, pending);
        keyPair = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
        Files.writeString(keyFile, document().put("revoked", false).put("expiresAt", Instant.MAX.toString()).toString());
        String refreshed = pending.block().getBearerToken();
        assertNotEquals(first.getBearerToken(), refreshed);
        String[] compact = refreshed.split("\\.");
        Signature verifier = Signature.getInstance("Ed25519");
        verifier.initVerify(keyPair.getPublic());
        verifier.update((compact[0] + "." + compact[1]).getBytes(StandardCharsets.US_ASCII));
        assertTrue(verifier.verify(Base64.getUrlDecoder().decode(compact[2])));
    }

    @ParameterizedTest
    @ValueSource(strings = {"tool", "method", "path", "audience", "credentialRef", "credentialVersion", "allowedScopes"})
    void rejectsEachIndependentTargetMismatch(final String field) throws Exception {
        ObjectNode content = document();
        if ("allowedScopes".equals(field)) {
            content.putArray(field).add("orders:read");
        } else {
            content.put(field, "other-target");
        }
        Files.writeString(keyFile, content.toString());
        assertFileFailure(Reason.TARGET_MISMATCH);
    }

    @ParameterizedTest
    @ValueSource(strings = {"issuer", "credentialRef", "credentialVersion", "tool", "method", "path", "audience", "allowedScopes", "privateKeyPkcs8Base64"})
    void rejectsEachMissingKeyField(final String field) throws Exception {
        ObjectNode content = document();
        content.remove(field);
        Files.writeString(keyFile, content.toString());
        assertFileFailure(Reason.KEY_UNAVAILABLE);
    }

    @Test
    void rejectsMalformedAmbiguousOrOversizedKeyDocuments() throws Exception {
        String original = document().toString();
        for (String invalid : List.of("", "[]", "{", original + " {}", original.replace("{", "{\"issuer\":\"duplicate\","),
                document().put("privateKeyPkcs8Base64", "sensitive-bad-key").toString(), document().put("issuer", " ").toString(),
                document().put("unknown", "secret").toString(), document().put("revoked", "false").toString(),
                document().put("expiresAt", "yesterday").toString(), document().put("allowedScopes", "orders:read").toString(),
                original.replace("\"orders:read\"", "false"), original.replace("\"orders:read\"", "\"\""),
                original.replace("\"orders:read\"", "\"orders:read\",\"orders:read\""), " ".repeat(8193) + original)) {
            Files.writeString(keyFile, invalid);
            assertFileFailure(Reason.KEY_UNAVAILABLE);
        }
    }

    @Test
    void rejectsFilesOutsideMountedDirectoryAndMissingDirectory() throws Exception {
        Path outside = Files.writeString(root.resolve("sensitive-outside-key"), document().toString());
        Files.delete(keyFile);
        Files.createSymbolicLink(keyFile, outside);
        assertFileFailure(Reason.KEY_UNAVAILABLE);
        AgentIdentityDelegationException error = assertThrows(AgentIdentityDelegationException.class,
                () -> new FileDelegationSigningKeyResolver(directory.resolve("sensitive-missing-directory")));
        assertEquals(Reason.KEY_UNAVAILABLE.name(), error.getMessage());
        assertNull(error.getCause());
    }

    @Test
    void sanitizesCustomResolverAndSigningFailuresAndInvalidBearer() throws Exception {
        JwtHttpIdentityDelegator failed = new JwtHttpIdentityDelegator(request -> {
            throw new IllegalArgumentException("private-subject tenant-a sensitive-key-value");
        }, CLOCK);
        DelegationRequest request = request(true, NOW.plusSeconds(60), "tenant-a");
        assertFailure(Reason.KEY_UNAVAILABLE, failed.delegate(request));
        SigningKey wrongKey = new SigningKey("issuer", KeyPairGenerator.getInstance("EC").generateKeyPair().getPrivate());
        assertFailure(Reason.SIGNING_FAILED, new JwtHttpIdentityDelegator(target -> wrongKey, CLOCK).delegate(request));
        AgentIdentityDelegationException error = assertThrows(AgentIdentityDelegationException.class,
                () -> new DelegatedCredential("private-subject\r\nsecond-header", NOW));
        assertEquals(Reason.SIGNING_FAILED.name(), error.getMessage());
        assertNull(error.getCause());
    }

    private JsonNode claims(final DelegatedCredential credential) throws Exception {
        return payload(credential.getBearerToken().split("\\.")[1]);
    }

    private JsonNode payload(final String segment) throws Exception {
        return JSON.readTree(Base64.getUrlDecoder().decode(segment));
    }

    private void assertExpiry(final Instant expected, final DelegatedCredential credential, final JsonNode claims) {
        assertEquals(expected, credential.getExpiresAt());
        BigDecimal numericDate = BigDecimal.valueOf(expected.getEpochSecond()).add(BigDecimal.valueOf(expected.getNano(), 9));
        assertEquals(0, numericDate.compareTo(claims.get("exp").decimalValue()));
    }

    private void assertFileFailure(final Reason reason) {
        assertFailure(reason, signer.delegate(request(true, NOW.plusSeconds(60), "tenant-a")));
    }

    private void assertFailure(final Reason reason, final Mono<DelegatedCredential> subscription) {
        AgentIdentityDelegationException error = assertThrows(AgentIdentityDelegationException.class, subscription::block);
        assertEquals(reason, error.getReason());
        assertEquals(reason.name(), error.getMessage());
        assertNull(error.getCause());
    }

    private ObjectNode document() {
        ObjectNode value = JSON.createObjectNode().put("issuer", "orders-issuer").put("tool", "lookup").put("method", "POST").put("path", "/api/orders/lookup");
        value.put("credentialRef", "orders").put("credentialVersion", "v1").put("audience", "orders-api");
        value.putArray("allowedScopes").add("data").add("orders:read").add("orders:list");
        return value.put("privateKeyPkcs8Base64", Base64.getEncoder().encodeToString(keyPair.getPrivate().getEncoded()));
    }

    private DelegationRequest request(final boolean requireTenant, final Instant deadline, final String tenant) {
        JsonObject tools = JsonParser.parseString("""
                {"lookup":{"description":"Find an order","inputSchema":{"type":"object","properties":{}},
                "requestConfig":{"requestTemplate":{"method":"POST","url":"/api/orders/lookup"},"argsPosition":{}},
                "identityDelegation":{"credentialRef":"orders","credentialVersion":"v1","audience":"orders-api","scopes":["orders:read","data"]}}}
                """).getAsJsonObject();
        tools.getAsJsonObject("lookup").getAsJsonObject("identityDelegation").addProperty("requireTenant", requireTenant);
        AgentGatewayHttpToolConfig.Tool tool = AgentGatewayHttpToolConfig.parse(tools, Set.of("lookup")).getTools().get("lookup");
        JsonObject untrusted = new JsonObject();
        untrusted.addProperty("tenant", "forged");
        AgentToolInvocation invocation = new AgentToolInvocation("1", "private-subject", untrusted, "rule-1", 23, deadline, untrusted);
        return new DelegationRequest(tool, new AgentToolInvocation(invocation, tenant));
    }
}
