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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PSource;
import java.nio.charset.StandardCharsets;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;

/**
 * Independent JCA client implementation for protocol regression tests.
 */
final class PasswordEnvelopeFixtures {

    static final ObjectMapper MAPPER = new ObjectMapper();

    private PasswordEnvelopeFixtures() {
    }

    static JsonNode metadata(final PasswordEncryptionService service) throws Exception {
        return MAPPER.readTree(Base64.getDecoder().decode(service.secretInfo())).get("encryption");
    }

    static byte[] encrypt(final PasswordEncryptionService service, final String purpose, final String target, final JsonNode data) throws Exception {
        return encrypt(service, purpose, target, data, metadata(service).get("serverTime").longValue());
    }

    static byte[] encrypt(final PasswordEncryptionService service, final String purpose, final String target, final JsonNode data, final long issuedAt) throws Exception {
        ObjectNode inner = MAPPER.createObjectNode().put("issuedAt", issuedAt);
        inner.set("data", data);
        return encryptInner(service, purpose, target, inner);
    }

    static byte[] encryptInner(final PasswordEncryptionService service, final String purpose, final String target, final JsonNode inner) throws Exception {
        JsonNode info = metadata(service);
        final PublicKey publicKey = KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(Base64.getDecoder().decode(info.get("publicKey").textValue())));
        KeyGenerator generator = KeyGenerator.getInstance("AES");
        generator.init(256);
        SecretKey key = generator.generateKey();
        byte[] iv = new byte[12];
        new SecureRandom().nextBytes(iv);
        Cipher aes = Cipher.getInstance("AES/GCM/NoPadding");
        aes.init(Cipher.ENCRYPT_MODE, key, new GCMParameterSpec(128, iv));
        aes.updateAAD(("shenyu-password/v1|" + info.get("keyId").textValue() + "|" + purpose + "|" + target).getBytes(StandardCharsets.UTF_8));
        byte[] ciphertext = aes.doFinal(MAPPER.writeValueAsBytes(inner));
        Cipher rsa = Cipher.getInstance("RSA/ECB/OAEPPadding");
        rsa.init(Cipher.ENCRYPT_MODE, publicKey, new OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT));
        ObjectNode envelope = MAPPER.createObjectNode().put("version", 1).put("keyId", info.get("keyId").textValue())
                .put("wrappedKey", Base64.getEncoder().encodeToString(rsa.doFinal(key.getEncoded())))
                .put("iv", Base64.getEncoder().encodeToString(iv)).put("ciphertext", Base64.getEncoder().encodeToString(ciphertext));
        return MAPPER.writeValueAsBytes(envelope);
    }
}
