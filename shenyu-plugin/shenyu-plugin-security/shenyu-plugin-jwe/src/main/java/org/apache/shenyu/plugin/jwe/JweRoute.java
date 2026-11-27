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

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import org.jose4j.jwa.AlgorithmConstraints;
import org.jose4j.jwe.JsonWebEncryption;
import org.jose4j.lang.JoseException;
import org.springframework.core.io.buffer.DataBufferLimitException;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Map;
import java.util.Objects;

/** Deployment-owned keys for a single route. A missing direction key disables that transform. */
public final class JweRoute {

    private static final String KEY_ALGORITHM = "RSA-OAEP-256";

    private static final String ENCRYPTION = "A256GCM";

    private static final String JSON = "application/json";

    private static final ObjectReader READER = new ObjectMapper().reader().with(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final RSAPrivateKey decryptionKey;

    private final Map<String, RSAPrivateKey> decryptionKeys;

    private final RSAPublicKey recipientKey;

    private final String recipientKeyId;

    public JweRoute(final RSAPrivateKey decryptionKey, final RSAPublicKey recipientKey) {
        if (Objects.isNull(decryptionKey) && Objects.isNull(recipientKey)) {
            throw new IllegalArgumentException("At least one JWE direction must be enabled");
        }
        this.decryptionKey = decryptionKey;
        this.decryptionKeys = Map.of();
        this.recipientKey = recipientKey;
        this.recipientKeyId = null;
    }

    /**
     * Bind an inbound key ring and one active outbound recipient for a namespace/rule pair.
     * @param decryptionKeys trusted private keys indexed by protected kid
     * @param recipientKeyId active outbound kid, or null for decrypt-only routes
     * @param recipientKey corresponding recipient public key
     */
    public JweRoute(final Map<String, RSAPrivateKey> decryptionKeys, final String recipientKeyId, final RSAPublicKey recipientKey) {
        this.decryptionKeys = Map.copyOf(decryptionKeys);
        if (decryptionKeys.keySet().stream().anyMatch(String::isBlank) || (decryptionKeys.isEmpty() && Objects.isNull(recipientKey))) {
            throw new IllegalArgumentException("Configure at least one JWE direction with nonblank key IDs");
        }
        if (Objects.isNull(recipientKeyId) != Objects.isNull(recipientKey) || (Objects.nonNull(recipientKeyId) && recipientKeyId.isBlank())) {
            throw new IllegalArgumentException("Configure both recipient key ID and public key");
        }
        this.decryptionKey = null;
        this.recipientKeyId = recipientKeyId;
        this.recipientKey = recipientKey;
    }

    boolean decrypts() {
        return Objects.nonNull(decryptionKey) || !decryptionKeys.isEmpty();
    }

    boolean encrypts() {
        return Objects.nonNull(recipientKey);
    }

    byte[] decrypt(final byte[] compact, final int plaintextLimit) throws JoseException, IOException {
        JsonWebEncryption jwe = new JsonWebEncryption();
        jwe.setAlgorithmConstraints(new AlgorithmConstraints(AlgorithmConstraints.ConstraintType.PERMIT, KEY_ALGORITHM));
        jwe.setContentEncryptionAlgorithmConstraints(new AlgorithmConstraints(AlgorithmConstraints.ConstraintType.PERMIT, ENCRYPTION));
        jwe.setCompactSerialization(new String(compact, StandardCharsets.US_ASCII));
        if (!JSON.equals(jwe.getContentTypeHeaderValue()) || Objects.nonNull(jwe.getObjectHeader("zip")) || Objects.nonNull(jwe.getObjectHeader("crit"))) {
            throw new JoseException("Unsupported protected header");
        }
        jwe.setKey(inboundKey(jwe.getKeyIdHeaderValue()));
        byte[] plaintext = jwe.getPlaintextBytes();
        if (plaintext.length > plaintextLimit) {
            throw new DataBufferLimitException("Decrypted payload too large");
        }
        requireJson(plaintext);
        return plaintext;
    }

    private RSAPrivateKey inboundKey(final String kid) throws JoseException {
        RSAPrivateKey key = Objects.isNull(kid) ? decryptionKey : decryptionKeys.get(kid);
        if (Objects.isNull(key)) {
            throw new JoseException("Unknown recipient key");
        }
        return key;
    }

    byte[] encrypt(final byte[] plaintext) throws JoseException, IOException {
        requireJson(plaintext);
        JsonWebEncryption jwe = new JsonWebEncryption();
        jwe.setAlgorithmHeaderValue(KEY_ALGORITHM);
        jwe.setEncryptionMethodHeaderParameter(ENCRYPTION);
        jwe.setContentTypeHeaderValue(JSON);
        if (Objects.nonNull(recipientKeyId)) {
            jwe.setKeyIdHeaderValue(recipientKeyId);
        }
        jwe.setKey(recipientKey);
        jwe.setPlaintext(plaintext);
        return jwe.getCompactSerialization().getBytes(StandardCharsets.US_ASCII);
    }

    static void requireJson(final byte[] plaintext) throws IOException {
        String json = StandardCharsets.UTF_8.newDecoder().decode(ByteBuffer.wrap(plaintext)).toString();
        JsonNode parsed = READER.readTree(json);
        if (Objects.isNull(parsed) || parsed.isMissingNode()) {
            throw new IOException("Expected a UTF-8 JSON document");
        }
    }
}
