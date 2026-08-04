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

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Verifies that AES and SM4 strategies are correctly wired via the SPI
 * {@link CryptorStrategyFactory#newInstance(String)} / ExtensionLoader, not just
 * instantiable directly. If the META-INF SPI file or @Join annotation is dropped,
 * these tests will fail — catching a regression that the direct-instantiation
 * tests in AesStrategyTest / Sm4StrategyTest would miss.
 */
class CryptorStrategyFactorySpiTest {

    private static final String SECRET = "0123456789abcdef";
    private static final String IV = "abcdef9876543210";

    private static String key() {
        return base64(SECRET) + ":" + base64(IV);
    }

    private static String base64(final String raw) {
        return Base64.getEncoder().encodeToString(raw.getBytes(StandardCharsets.UTF_8));
    }

    @Test
    void aesStrategyShouldBeLoadableViaSpi() throws Exception {
        CryptorStrategy strategy = CryptorStrategyFactory.newInstance("aes");
        assertThat("SPI must resolve 'aes' to a non-null strategy", strategy, notNullValue());

        String plaintext = "spi-round-trip";
        String ciphertext = strategy.encrypt(key(), plaintext);
        byte[] cipherBytes = Base64.getMimeDecoder().decode(ciphertext);
        assertThat(strategy.decrypt(key(), cipherBytes), is(plaintext));
    }

    @Test
    void sm4StrategyShouldBeLoadableViaSpi() throws Exception {
        CryptorStrategy strategy = CryptorStrategyFactory.newInstance("sm4");
        assertThat("SPI must resolve 'sm4' to a non-null strategy", strategy, notNullValue());

        String plaintext = "spi-round-trip";
        String ciphertext = strategy.encrypt(key(), plaintext);
        byte[] cipherBytes = Base64.getMimeDecoder().decode(ciphertext);
        assertThat(strategy.decrypt(key(), cipherBytes), is(plaintext));
    }

    // --- Negative tests: invalid key content (not just format) ---

    @Test
    void shouldThrowWhenAesSecretHasWrongByteLength() {
        // AES requires 16/24/32-byte keys; 15 bytes is invalid
        String tooShort = base64("123456789012345");  // 15 bytes
        String validIv = base64(IV);
        CryptorStrategy strategy = CryptorStrategyFactory.newInstance("aes");
        assertThrows(Exception.class, () -> strategy.encrypt(tooShort + ":" + validIv, "data"));
    }

    @Test
    void shouldThrowWhenSm4KeyHasWrongByteLength() {
        // SM4 requires 16-byte keys; 17 bytes is invalid
        String tooLong = base64("123456789012345678");  // 18 bytes
        String validIv = base64(IV);
        CryptorStrategy strategy = CryptorStrategyFactory.newInstance("sm4");
        assertThrows(Exception.class, () -> strategy.encrypt(tooLong + ":" + validIv, "data"));
    }

    @Test
    void shouldThrowWhenKeyContainsNonBase64Content() {
        // Valid format (has separator) but non-base64 content
        String invalidKey = "not!!!base64:not!!!base64";
        CryptorStrategy strategy = CryptorStrategyFactory.newInstance("aes");
        assertThrows(Exception.class, () -> strategy.encrypt(invalidKey, "data"));
    }
}
