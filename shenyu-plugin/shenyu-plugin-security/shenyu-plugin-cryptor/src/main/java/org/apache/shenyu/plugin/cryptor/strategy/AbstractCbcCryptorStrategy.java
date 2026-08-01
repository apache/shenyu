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
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.Security;
import java.util.Base64;
import java.util.Objects;

/**
 * Common base for symmetric CBC strategies (AES/SM4).
 *
 * <p>The {@code key} is a compound string following the convention
 * {@code base64(secret):base64(iv)}, so the single-key contract inherited from
 * {@link CryptorStrategy} can carry both the secret and the iv without changing
 * the spi interface or the rule handler. Subclasses only declare the cipher
 * transformation and the secret-key algorithm name; everything else (key parsing,
 * provider registration, base64 and utf-8 wiring) lives here.
 */
abstract class AbstractCbcCryptorStrategy implements CryptorStrategy {

    private static final String KEY_SEPARATOR = ":";

    static {
        if (Objects.isNull(Security.getProvider(BouncyCastleProvider.PROVIDER_NAME))) {
            Security.addProvider(new BouncyCastleProvider());
        }
    }

    /**
     * Cipher transformation, e.g. {@code AES/CBC/PKCS7Padding}.
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
        String[] parts = parseKey(key);
        Cipher cipher = buildCipher(Cipher.DECRYPT_MODE, parts[0], parts[1]);
        return new String(cipher.doFinal(encryptData), StandardCharsets.UTF_8);
    }

    @Override
    public String encrypt(final String key, final String data) throws Exception {
        String[] parts = parseKey(key);
        Cipher cipher = buildCipher(Cipher.ENCRYPT_MODE, parts[0], parts[1]);
        byte[] cipherBytes = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        return Base64.getEncoder().encodeToString(cipherBytes);
    }

    private String[] parseKey(final String key) {
        String[] parts = key.split(KEY_SEPARATOR, 2);
        if (parts.length != 2 || parts[0].isEmpty() || parts[1].isEmpty()) {
            throw new IllegalArgumentException(
                    "invalid symmetric key, expected format: base64(secret):base64(iv)");
        }
        return parts;
    }

    private Cipher buildCipher(final int mode, final String secretBase64, final String ivBase64) throws Exception {
        byte[] secret = Base64.getMimeDecoder().decode(secretBase64);
        byte[] iv = Base64.getMimeDecoder().decode(ivBase64);
        Cipher cipher = Cipher.getInstance(getTransformation(), BouncyCastleProvider.PROVIDER_NAME);
        cipher.init(mode, new SecretKeySpec(secret, getAlgorithm()), new IvParameterSpec(iv));
        return cipher;
    }
}
