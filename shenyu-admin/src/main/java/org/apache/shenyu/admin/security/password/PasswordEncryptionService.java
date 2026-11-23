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

package org.apache.shenyu.admin.security.password;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.StreamReadConstraints;
import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import jakarta.validation.Validator;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.model.dto.LoginDashboardUserDTO;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PSource;
import javax.crypto.spec.SecretKeySpec;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.RSAPublicKeySpec;
import java.util.Base64;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Bounded, authenticated password envelopes without changing existing password storage or authentication services.
 */
@Service
public class PasswordEncryptionService {

    public static final int MAX_BODY_BYTES = 64 * 1024;

    private static final Set<String> ENVELOPE_FIELDS = Set.of("version", "keyId", "wrappedKey", "iv", "ciphertext");

    private static final Set<String> LOGIN_FIELDS = Set.of("userName", "password", "clientId");

    private static final Set<String> CHANGE_FIELDS = Set.of("id", "userName", "password", "oldPassword");

    private static final OAEPParameterSpec OAEP = new OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT);

    private final ObjectMapper mapper = new ObjectMapper(JsonFactory.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION)
            .streamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(16).maxNumberLength(20).maxStringLength(MAX_BODY_BYTES).build())
            .build()).enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final PasswordEncryptionProperties properties;

    private final SecretProperties legacySecret;

    private final Validator validator;

    private final PrivateKey privateKey;

    private final String publicKey;

    private final String keyId;

    private final int wrappedKeyLength;

    /**
     * Load a configured PKCS8 key, or generate a process-local development key.
     * @param properties envelope configuration
     * @param legacySecret existing login AES configuration
     * @param validator original login DTO validation
     */
    public PasswordEncryptionService(final PasswordEncryptionProperties properties, final SecretProperties legacySecret, final Validator validator) {
        this.properties = properties;
        this.legacySecret = legacySecret;
        this.validator = validator;
        try {
            if (properties.getMaxAgeSeconds() < 1 || properties.getMaxAgeSeconds() > 3600) {
                throw new GeneralSecurityException();
            }
            RSAPrivateCrtKey rsaPrivate;
            PublicKey rsaPublic;
            if (StringUtils.isBlank(properties.getPrivateKey())) {
                KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
                generator.initialize(2048);
                KeyPair pair = generator.generateKeyPair();
                rsaPrivate = (RSAPrivateCrtKey) pair.getPrivate();
                rsaPublic = pair.getPublic();
            } else {
                String encoded = properties.getPrivateKey().replace("-----BEGIN PRIVATE KEY-----", "")
                        .replace("-----END PRIVATE KEY-----", "").replaceAll("\\s", "");
                KeyFactory factory = KeyFactory.getInstance("RSA");
                rsaPrivate = (RSAPrivateCrtKey) factory.generatePrivate(new PKCS8EncodedKeySpec(Base64.getDecoder().decode(encoded)));
                rsaPublic = factory.generatePublic(new RSAPublicKeySpec(rsaPrivate.getModulus(), rsaPrivate.getPublicExponent()));
            }
            if (rsaPrivate.getModulus().bitLength() < 2048) {
                throw new GeneralSecurityException();
            }
            this.privateKey = rsaPrivate;
            this.publicKey = Base64.getEncoder().encodeToString(rsaPublic.getEncoded());
            this.keyId = Base64.getUrlEncoder().withoutPadding().encodeToString(MessageDigest.getInstance("SHA-256").digest(rsaPublic.getEncoded()));
            this.wrappedKeyLength = (rsaPrivate.getModulus().bitLength() + 7) / 8;
        } catch (GeneralSecurityException | IllegalArgumentException | ClassCastException exception) {
            throw new IllegalStateException("Unable to initialize password encryption");
        }
    }

    /**
     * Public encryption capability, preserving the sanitized legacy AES fields.
     * @return Base64 encoded public metadata
     */
    public String secretInfo() {
        ObjectNode result = mapper.createObjectNode();
        result.put("key", "");
        result.put("iv", "");
        ObjectNode encryption = result.putObject("encryption");
        encryption.put("version", 1);
        encryption.put("algorithm", "RSA-OAEP-256+A256GCM");
        encryption.put("keyId", keyId);
        encryption.put("publicKey", publicKey);
        encryption.put("serverTime", System.currentTimeMillis());
        encryption.put("maxAgeSeconds", properties.getMaxAgeSeconds());
        return Base64.getEncoder().encodeToString(result.toString().getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Decode an envelope and return only the original DTO JSON for the existing converter and validation.
     * @param body bounded original JSON body
     * @param purpose server-selected operation
     * @param target server-selected decoded path ID, or empty for login
     * @return original DTO JSON, or unchanged legacy JSON when compatibility is enabled
     */
    public byte[] decodeRequest(final byte[] body, final String purpose, final String target) {
        try {
            if (body.length > MAX_BODY_BYTES || !("login".equals(purpose) || "change-password".equals(purpose))
                    || ("login".equals(purpose) ? !target.isEmpty() : target.isEmpty())) {
                throw new InvalidPasswordRequestException();
            }
            JsonNode envelope = mapper.readTree(body);
            if (Objects.isNull(envelope) || !envelope.isObject()) {
                throw new InvalidPasswordRequestException();
            }
            boolean encrypted = ENVELOPE_FIELDS.stream().anyMatch(envelope::has);
            if (!encrypted) {
                if (properties.isRequired()) {
                    throw new InvalidPasswordRequestException();
                }
                return body;
            }
            requireFields(envelope, ENVELOPE_FIELDS);
            if (!envelope.path("version").isIntegralNumber() || !envelope.path("version").canConvertToInt() || envelope.path("version").intValue() != 1
                    || !keyId.equals(envelope.path("keyId").textValue())) {
                throw new InvalidPasswordRequestException();
            }
            byte[] wrappedKey = decodeBase64(envelope.path("wrappedKey"));
            byte[] iv = decodeBase64(envelope.path("iv"));
            byte[] ciphertext = decodeBase64(envelope.path("ciphertext"));
            if (wrappedKey.length != wrappedKeyLength || iv.length != 12 || ciphertext.length < 16) {
                throw new InvalidPasswordRequestException();
            }
            Cipher rsa = Cipher.getInstance("RSA/ECB/OAEPPadding");
            rsa.init(Cipher.DECRYPT_MODE, privateKey, OAEP);
            byte[] key = rsa.doFinal(wrappedKey);
            if (key.length != 32) {
                throw new InvalidPasswordRequestException();
            }
            Cipher aes = Cipher.getInstance("AES/GCM/NoPadding");
            aes.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"), new GCMParameterSpec(128, iv));
            aes.updateAAD(("shenyu-password/v1|" + keyId + "|" + purpose + "|" + target).getBytes(StandardCharsets.UTF_8));
            JsonNode plaintext = mapper.readTree(aes.doFinal(ciphertext));
            requireFields(plaintext, Set.of("issuedAt", "data"));
            JsonNode issuedAt = plaintext.path("issuedAt");
            long now = System.currentTimeMillis();
            if (!issuedAt.isIntegralNumber() || !issuedAt.canConvertToLong()
                    || issuedAt.longValue() < now - properties.getMaxAgeSeconds() * 1000L || issuedAt.longValue() > now + 30_000L) {
                throw new InvalidPasswordRequestException();
            }
            ObjectNode data = requireData(plaintext.path("data"), "login".equals(purpose) ? LOGIN_FIELDS : CHANGE_FIELDS);
            if ("login".equals(purpose)) {
                LoginDashboardUserDTO dto = mapper.treeToValue(data, LoginDashboardUserDTO.class);
                if (!validator.validate(dto).isEmpty()) {
                    throw new InvalidPasswordRequestException();
                }
                data.put("password", bridgeLoginPassword(dto.getPassword()));
            }
            return mapper.writeValueAsBytes(data);
        } catch (IOException | GeneralSecurityException | RuntimeException exception) {
            throw new InvalidPasswordRequestException();
        }
    }

    private byte[] decodeBase64(final JsonNode value) {
        if (!value.isTextual() || value.textValue().isEmpty()) {
            throw new InvalidPasswordRequestException();
        }
        byte[] decoded = Base64.getDecoder().decode(value.textValue());
        if (!Base64.getEncoder().encodeToString(decoded).equals(value.textValue())) {
            throw new InvalidPasswordRequestException();
        }
        return decoded;
    }

    private void requireFields(final JsonNode node, final Set<String> expected) {
        if (Objects.isNull(node) || !node.isObject()) {
            throw new InvalidPasswordRequestException();
        }
        Set<String> actual = new HashSet<>();
        node.fieldNames().forEachRemaining(actual::add);
        if (!actual.equals(expected)) {
            throw new InvalidPasswordRequestException();
        }
    }

    private ObjectNode requireData(final JsonNode node, final Set<String> allowed) {
        if (!node.isObject()) {
            throw new InvalidPasswordRequestException();
        }
        node.fields().forEachRemaining(field -> {
            if (!allowed.contains(field.getKey()) || !(field.getValue().isTextual() || field.getValue().isNull())) {
                throw new InvalidPasswordRequestException();
            }
        });
        return (ObjectNode) node;
    }

    private String bridgeLoginPassword(final String password) throws GeneralSecurityException {
        boolean keyEmpty = StringUtils.isBlank(legacySecret.getKey());
        boolean ivEmpty = StringUtils.isBlank(legacySecret.getIv());
        if (keyEmpty && ivEmpty) {
            return password;
        }
        if (keyEmpty || ivEmpty) {
            throw new InvalidPasswordRequestException();
        }
        byte[] key = legacySecret.getKey().getBytes(StandardCharsets.UTF_8);
        byte[] iv = legacySecret.getIv().getBytes(StandardCharsets.UTF_8);
        if ((key.length != 16 && key.length != 24 && key.length != 32) || iv.length != 16) {
            throw new InvalidPasswordRequestException();
        }
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"), new IvParameterSpec(iv));
        return Base64.getEncoder().encodeToString(cipher.doFinal(password.getBytes(StandardCharsets.UTF_8)));
    }
}
