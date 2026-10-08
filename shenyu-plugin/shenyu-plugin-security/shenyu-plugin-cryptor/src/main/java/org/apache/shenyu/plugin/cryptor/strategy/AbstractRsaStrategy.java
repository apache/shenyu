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

import javax.crypto.Cipher;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;
import java.util.Objects;

/**
 * Common base for RSA strategies. The {@code key} is a single base64-encoded
 * PKCS#8 private key (decrypt) or X.509 public key (encrypt). Subclasses only
 * declare the cipher transformation and, optionally, algorithm parameters —
 * this lets the {@code rsa} (OAEP, default) and {@code rsa-pkcs1} (PKCS#1 v1.5,
 * legacy compatibility) SPI entries share all the key/provider/base64 wiring.
 */
abstract class AbstractRsaStrategy implements CryptorStrategy {

    private static final String RSA = "RSA";

    /**
     * Cipher transformation, e.g. {@code RSA/ECB/OAEPWithSHA-256AndMGF1Padding}.
     *
     * @return transformation
     */
    protected abstract String transformation();

    /**
     * Algorithm parameters for {@link Cipher#init(int, Key, AlgorithmParameterSpec)},
     * or {@code null} to use the parameterless init (e.g. PKCS#1 v1.5).
     *
     * @return params or null
     */
    protected abstract AlgorithmParameterSpec params();

    @Override
    public String decrypt(final String key, final byte[] encryptData) throws Exception {
        byte[] decoded = Base64.getDecoder().decode(key);
        PrivateKey priKey = KeyFactory.getInstance(RSA).generatePrivate(new PKCS8EncodedKeySpec(decoded));
        Cipher cipher = Cipher.getInstance(transformation());
        init(cipher, Cipher.DECRYPT_MODE, priKey);
        return new String(cipher.doFinal(encryptData), StandardCharsets.UTF_8);
    }

    @Override
    public String encrypt(final String key, final String data) throws Exception {
        byte[] decoded = Base64.getDecoder().decode(key);
        RSAPublicKey pubKey = (RSAPublicKey) KeyFactory.getInstance(RSA).generatePublic(new X509EncodedKeySpec(decoded));
        Cipher cipher = Cipher.getInstance(transformation());
        init(cipher, Cipher.ENCRYPT_MODE, pubKey);
        return Base64.getEncoder().encodeToString(cipher.doFinal(data.getBytes(StandardCharsets.UTF_8)));
    }

    private void init(final Cipher cipher, final int mode, final Key key) throws Exception {
        AlgorithmParameterSpec algorithmParameterSpec = params();
        if (Objects.isNull(algorithmParameterSpec)) {
            cipher.init(mode, key);
        } else {
            cipher.init(mode, key, algorithmParameterSpec);
        }
    }
}
