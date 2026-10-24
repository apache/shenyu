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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class AesStrategyTest {

    private static final String SECRET = "0123456789abcdef";

    private final CryptorStrategy strategy = new AesStrategy();

    private final String key = base64(SECRET);

    private static String base64(final String raw) {
        return Base64.getEncoder().encodeToString(raw.getBytes(StandardCharsets.UTF_8));
    }

    @ParameterizedTest
    @ValueSource(strings = {"shenyu", "hello world!", "{\"name\":\"tom\",\"age\":18}", "中文加密报文"})
    void shouldRecoverPlaintextWhenRoundTrip(final String plaintext) throws Exception {
        String ciphertext = strategy.encrypt(key, plaintext);
        byte[] cipherBytes = Base64.getDecoder().decode(ciphertext);
        assertThat(strategy.decrypt(key, cipherBytes), is(plaintext));
    }

    @Test
    void shouldEmitDifferentCiphertextForRepeatedEncrypts() throws Exception {
        // GCM draws a fresh nonce per message, so identical plaintexts must not
        // produce identical ciphertexts — the core property that defeats IV reuse.
        String first = strategy.encrypt(key, "same-message");
        String second = strategy.encrypt(key, "same-message");
        assertThat(first, is(not(equalTo(second))));
    }

    @Test
    void shouldFailAuthenticationWhenCiphertextIsTampered() throws Exception {
        // GCM's tag makes the ciphertext non-malleable: flipping a bit must be
        // detected on decrypt rather than silently producing corrupted plaintext.
        byte[] cipherBytes = Base64.getDecoder().decode(strategy.encrypt(key, "secret"));
        cipherBytes[cipherBytes.length - 1] ^= 0x01;
        assertThrows(Exception.class, () -> strategy.decrypt(key, cipherBytes));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "base64(secret):base64(iv)"})
    void shouldThrowWhenKeyFormatIsInvalid(final String invalidKey) {
        assertThrows(Exception.class, () -> strategy.encrypt(invalidKey, "data"));
    }
}
