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

import com.authlete.hms.ComponentIdentifier;
import com.authlete.hms.ComponentValueProvider;
import com.authlete.hms.SignatureBaseBuilder;
import com.authlete.hms.SignatureField;
import com.authlete.hms.SignatureInputField;
import com.authlete.hms.SignatureMetadata;
import com.authlete.hms.SignatureMetadataParameters;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.time.Instant;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/** Strict Ed25519 request profile; trust is supplied by the gateway, never by the message. */
public final class MessageSignatureVerifier {

    static final int MAX_AGE_SECONDS = 300;

    private static final String LABEL = "sig1";

    private static final String DIGEST = "content-digest";

    private static final int MAX_NONCE_LENGTH = 128;

    private final String keyId;

    private final PublicKey key;

    private final NonceStore replayStore;

    private MessageSignatureVerifier(final String keyId, final PublicKey key, final NonceStore replayStore) {
        this.keyId = Objects.requireNonNull(keyId);
        this.key = Objects.requireNonNull(key);
        this.replayStore = replayStore;
    }

    /**
     * Verify signatures without replay protection.
     * @param keyId trusted key identifier
     * @param key verification key
     * @return verification-only policy
     */
    public static MessageSignatureVerifier verificationOnly(final String keyId, final PublicKey key) {
        return new MessageSignatureVerifier(keyId, key, null);
    }

    /**
     * Require an atomic nonce claim; a missing store denies verified requests.
     * @param keyId trusted key identifier
     * @param key verification key
     * @param store deployment's shared nonce store, if available
     * @return replay-protected policy
     */
    public static MessageSignatureVerifier replayProtected(final String keyId, final PublicKey key, final NonceStore store) {
        NonceStore required = Objects.nonNull(store) ? store : (id, nonce, ttl) -> Mono.error(new IllegalStateException("Nonce store unavailable"));
        return new MessageSignatureVerifier(keyId, key, required);
    }

    String keyId() {
        return keyId;
    }

    NonceStore store() {
        return replayStore;
    }

    String verify(final ServerHttpRequest request, final byte[] body, final Instant now) throws GeneralSecurityException {
        HttpHeaders headers = request.getHeaders();
        SignatureInputField inputs = SignatureInputField.parse(single(headers, "Signature-Input"));
        SignatureField signatures = SignatureField.parse(single(headers, "Signature"));
        if (!inputs.keySet().equals(Set.of(LABEL)) || !signatures.keySet().equals(Set.of(LABEL))) {
            throw new SignatureException("Expected one sig1 signature");
        }
        SignatureMetadata metadata = inputs.get(LABEL);
        validateParameters(metadata.getParameters(), now);
        boolean signedBody = body.length > 0 || headers.containsKey(DIGEST) || headers.containsKey(HttpHeaders.CONTENT_TYPE);
        validateCoverage(metadata, signedBody);
        if (signedBody) {
            String digest = "sha-256=:" + Base64.getEncoder().encodeToString(MessageDigest.getInstance("SHA-256").digest(body)) + ":";
            if (!digest.equals(single(headers, DIGEST))) {
                throw new SignatureException("Content-Digest mismatch");
            }
            single(headers, HttpHeaders.CONTENT_TYPE);
        }
        if (!verifySignature(request, metadata, signatures.get(LABEL), key)) {
            throw new SignatureException("Invalid signature");
        }
        return metadata.getParameters().getNonce();
    }

    static boolean verifySignature(final ServerHttpRequest request, final SignatureMetadata metadata, final byte[] signature, final PublicKey key) throws GeneralSecurityException {
        ComponentValueProvider context = new ComponentValueProvider().setMethod(request.getMethod().name())
                .setTargetUri(request.getURI()).setHeaders(request.getHeaders());
        String base = new SignatureBaseBuilder(context).build(metadata).serialize();
        Signature verifier = Signature.getInstance("Ed25519");
        verifier.initVerify(key);
        verifier.update(base.getBytes(StandardCharsets.UTF_8));
        return verifier.verify(signature);
    }

    private void validateParameters(final SignatureMetadataParameters parameters, final Instant now) throws SignatureException {
        if (!keyId.equals(parameters.getKeyid()) || !"ed25519".equals(parameters.getAlg())) {
            throw new SignatureException("Untrusted key or algorithm");
        }
        validateWindow(parameters.getCreated(), parameters.getExpires(), now);
        String nonce = parameters.getNonce();
        if (Objects.nonNull(replayStore) && (Objects.isNull(nonce) || nonce.isBlank() || nonce.length() > MAX_NONCE_LENGTH)) {
            throw new SignatureException("Missing or invalid nonce");
        }
    }

    private static void validateWindow(final Instant created, final Instant expires, final Instant now) throws SignatureException {
        if (Objects.isNull(created) || Objects.isNull(expires) || created.isAfter(now)) {
            throw new SignatureException("Invalid creation time");
        }
        if (!expires.isAfter(now) || expires.isAfter(created.plusSeconds(MAX_AGE_SECONDS))) {
            throw new SignatureException("Invalid expiry");
        }
    }

    private static void validateCoverage(final SignatureMetadata metadata, final boolean signedBody) throws SignatureException {
        Set<String> required = signedBody ? Set.of("@method", "@target-uri", "content-type", DIGEST) : Set.of("@method", "@target-uri");
        Set<String> actual = metadata.stream().filter(id -> id.getParameters().isEmpty())
                .map(ComponentIdentifier::getComponentName).collect(Collectors.toSet());
        if (!actual.equals(required) || metadata.size() != required.size()) {
            throw new SignatureException("Unsupported signature coverage");
        }
    }

    private static String single(final HttpHeaders headers, final String name) throws SignatureException {
        List<String> values = headers.get(name);
        if (Objects.isNull(values) || values.size() != 1) {
            throw new SignatureException("Missing or ambiguous field");
        }
        return values.get(0);
    }
}
