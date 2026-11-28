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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig.IdentityDelegation;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentIdentityDelegationException.Reason;
import org.apache.shenyu.plugin.agent.gateway.delegation.DelegationSigningKeyResolver.SigningKey;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.Signature;
import java.time.Clock;
import java.time.Instant;
import java.util.Base64;
import java.util.Objects;
import java.util.UUID;

/** Signs a fresh RFC 8037 Ed25519 assertion for each authorized HTTP invocation. */
public final class JwtHttpIdentityDelegator implements AgentHttpIdentityDelegator {

    private static final ObjectMapper JSON = new ObjectMapper();

    private static final Base64.Encoder BASE64_URL = Base64.getUrlEncoder().withoutPadding();

    private static final int NANOSECOND_SCALE = 9;

    private final DelegationSigningKeyResolver resolver;

    private final Clock clock;

    public JwtHttpIdentityDelegator(final DelegationSigningKeyResolver resolver) {
        this(resolver, Clock.systemUTC());
    }

    JwtHttpIdentityDelegator(final DelegationSigningKeyResolver resolver, final Clock clock) {
        this.resolver = Objects.requireNonNull(resolver);
        this.clock = clock;
    }

    @Override
    public Mono<DelegatedCredential> delegate(final DelegationRequest request) {
        return Mono.fromCallable(() -> {
            checkDeadline(request);
            if (request.getPolicy().isRequireTenant() && request.getTenant().isEmpty()) {
                throw new AgentIdentityDelegationException(Reason.TENANT_MISSING);
            }
            SigningKey key = resolve(request);
            Instant now = checkDeadline(request);
            Instant ttlEnd = now.plusSeconds(request.getPolicy().getTtlSeconds());
            Instant expiry = ttlEnd.isBefore(request.getDeadline()) ? ttlEnd : request.getDeadline();
            if (!key.expiresAt().isAfter(now)) {
                throw new AgentIdentityDelegationException(Reason.REVOKED_OR_EXPIRED);
            }
            return sign(request, key, now, key.expiresAt().isBefore(expiry) ? key.expiresAt() : expiry);
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private Instant checkDeadline(final DelegationRequest request) {
        Instant now = clock.instant();
        if (!now.isBefore(request.getDeadline())) {
            throw new AgentIdentityDelegationException(Reason.DEADLINE_EXCEEDED);
        }
        return now;
    }

    private SigningKey resolve(final DelegationRequest request) {
        try {
            return Objects.requireNonNull(resolver.resolve(request));
        } catch (AgentIdentityDelegationException error) {
            throw error;
        } catch (RuntimeException error) {
            throw new AgentIdentityDelegationException(Reason.KEY_UNAVAILABLE);
        }
    }

    private DelegatedCredential sign(final DelegationRequest request, final SigningKey key, final Instant now, final Instant expiry) {
        IdentityDelegation policy = request.getPolicy();
        ObjectNode header = JSON.createObjectNode().put("alg", "EdDSA").put("typ", "JWT");
        header.put("kid", policy.getCredentialRef() + ":" + policy.getCredentialVersion());
        ObjectNode claims = JSON.createObjectNode().put("iss", key.issuer()).put("sub", request.getSubject());
        claims.put("aud", policy.getAudience()).put("scope", String.join(" ", policy.getScopes()));
        request.getTenant().ifPresent(tenant -> claims.put("tenant", tenant));
        claims.put("iat", now.getEpochSecond()).put("nbf", now.getEpochSecond()).put("jti", UUID.randomUUID().toString());
        // NumericDate permits fractions, preserving sub-second MCP deadlines without rounding up.
        if (expiry.getNano() == 0) {
            claims.put("exp", expiry.getEpochSecond());
        } else {
            claims.put("exp", BigDecimal.valueOf(expiry.getEpochSecond()).add(BigDecimal.valueOf(expiry.getNano(), NANOSECOND_SCALE)));
        }
        claims.put("tool", request.getTool().getName()).put("route", request.getTool().getMethod() + " " + request.getTool().getPath());
        claims.put("policy_version", request.getPolicyVersion());
        try {
            String input = encode(header) + "." + encode(claims);
            Signature signature = Signature.getInstance("Ed25519");
            signature.initSign(key.privateKey());
            signature.update(input.getBytes(StandardCharsets.US_ASCII));
            return new DelegatedCredential(input + "." + BASE64_URL.encodeToString(signature.sign()), expiry);
        } catch (GeneralSecurityException | JsonProcessingException | RuntimeException error) {
            throw new AgentIdentityDelegationException(Reason.SIGNING_FAILED);
        }
    }

    private static String encode(final ObjectNode value) throws JsonProcessingException {
        return BASE64_URL.encodeToString(JSON.writeValueAsBytes(value));
    }
}
