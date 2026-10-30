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

package org.apache.shenyu.plugin.cryptor.strategy;

import org.bouncycastle.jce.provider.BouncyCastleProvider;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.security.Security;
import java.util.Base64;
import java.util.Objects;

/**
 * Common base for symmetric AEAD strategies (AES-GCM / SM4-GCM).
 *
 * <p><b>Key format:</b> the {@code key} carried by {@link CryptorStrategy} is a single
 * base64-encoded raw secret, i.e. {@code base64(secret)}. Unlike the older CBC
 * convention there is deliberately <em>no</em> IV component: in GCM reusing a
 * (key, nonce) pair catastrophically leaks the secret, so the nonce must never be
 * a fixed, operator-configured value. Subclasses only declare the cipher
 * transformation and the secret-key algorithm name; nonce generation, provider
 * registration, base64 and utf-8 wiring live here.
 *
 * <p><b>Wire format:</b> each {@link #encrypt(String, String)} call draws a fresh
 * 96-bit nonce from {@link SecureRandom} and emits
 * {@code base64(nonce || ciphertext || tag)} (128-bit authentication tag). The
 * nonce is therefore unique per message, which closes the IV-reuse leak
 * (CWE-329). The tag makes the ciphertext non-malleable: any in-transit
 * modification causes {@link #decrypt(String, byte[])} to fail authentication
 * (CWE-1204), so bit-flipping attacks are detected instead of silently honoured.
 *
 * <p>Note: the key format here ({@code base64(secret)}) differs from
 * {@link org.apache.shenyu.common.utils.AesUtils} which accepts a raw UTF-8
 * string for the secret. Operators using both should not interchange secrets
 * between the two without re-encoding.
 */
abstract class AbstractAeadCryptorStrategy implements CryptorStrategy {

    /**
     * GCM nonce length in bytes (96 bits, the value mandated/recommended by NIST SP 800-38D).
     */
    private static final int NONCE_LENGTH_BYTES = 12;

    /**
     * GCM authentication tag length in bits (maximum strength).
     */
    private static final int TAG_LENGTH_BITS = 128;

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    static {
        if (Objects.isNull(Security.getProvider(BouncyCastleProvider.PROVIDER_NAME))) {
            Security.addProvider(new BouncyCastleProvider());
        }
    }

    /**
     * Cipher transformation, e.g. {@code AES/GCM/NoPadding}.
     *
     * @return transformation
     */
    protected abstract String getTransformation();

    /**
     * Secret-key algorithm name, e.g. {@code AES} or {@code SM4}.
     *
     * @return algorithm name
     */
    protected abstract String getAlgorithm();

    @Override
    public String decrypt(final String key, final byte[] encryptData) throws Exception {
        if (encryptData.length < NONCE_LENGTH_BYTES) {
            throw new IllegalArgumentException(
                    "invalid ciphertext: shorter than the " + NONCE_LENGTH_BYTES + "-byte nonce prefix");
        }
        byte[] nonce = new byte[NONCE_LENGTH_BYTES];
        byte[] cipherAndTag = new byte[encryptData.length - NONCE_LENGTH_BYTES];
        System.arraycopy(encryptData, 0, nonce, 0, NONCE_LENGTH_BYTES);
        System.arraycopy(encryptData, NONCE_LENGTH_BYTES, cipherAndTag, 0, cipherAndTag.length);
        byte[] secret = parseSecret(key);
        Cipher cipher = buildCipher(Cipher.DECRYPT_MODE, secret, nonce);
        return new String(cipher.doFinal(cipherAndTag), StandardCharsets.UTF_8);
    }

    @Override
    public String encrypt(final String key, final String data) throws Exception {
        byte[] secret = parseSecret(key);
        byte[] nonce = new byte[NONCE_LENGTH_BYTES];
        SECURE_RANDOM.nextBytes(nonce);
        Cipher cipher = buildCipher(Cipher.ENCRYPT_MODE, secret, nonce);
        byte[] cipherAndTag = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        byte[] output = new byte[nonce.length + cipherAndTag.length];
        System.arraycopy(nonce, 0, output, 0, nonce.length);
        System.arraycopy(cipherAndTag, 0, output, nonce.length, cipherAndTag.length);
        return Base64.getEncoder().encodeToString(output);
    }

    /**
     * Decode the base64 secret, rejecting the legacy {@code base64(secret):base64(iv)}
     * compound format so an operator cannot accidentally configure a fixed IV that
     * GCM cannot honour.
     *
     * @param key base64-encoded raw secret
     * @return decoded secret bytes
     */
    private byte[] parseSecret(final String key) {
        if (key.contains(":")) {
            throw new IllegalArgumentException(
                    "invalid symmetric key: GCM expects a single base64(secret), "
                            + "the legacy base64(secret):base64(iv) form is not accepted");
        }
        return Base64.getDecoder().decode(key);
    }

    private Cipher buildCipher(final int mode, final byte[] secret, final byte[] nonce) throws Exception {
        Cipher cipher = Cipher.getInstance(getTransformation(), BouncyCastleProvider.PROVIDER_NAME);
        cipher.init(mode, new SecretKeySpec(secret, getAlgorithm()),
                new GCMParameterSpec(TAG_LENGTH_BITS, nonce));
        return cipher;
    }
}
