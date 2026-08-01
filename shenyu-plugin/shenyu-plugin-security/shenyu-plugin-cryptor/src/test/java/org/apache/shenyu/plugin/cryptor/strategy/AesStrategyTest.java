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

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class AesStrategyTest {

    private static final String SECRET = "0123456789abcdef";

    private static final String IV = "abcdef9876543210";

    private final CryptorStrategy strategy = new AesStrategy();

    private final String key = base64(SECRET) + ":" + base64(IV);

    private static String base64(final String raw) {
        return Base64.getEncoder().encodeToString(raw.getBytes(StandardCharsets.UTF_8));
    }

    @ParameterizedTest
    @ValueSource(strings = {"shenyu", "hello world!", "{\"name\":\"tom\",\"age\":18}", "中文加密报文"})
    void shouldRecoverPlaintextWhenRoundTrip(final String plaintext) throws Exception {
        String ciphertext = strategy.encrypt(key, plaintext);
        byte[] cipherBytes = Base64.getMimeDecoder().decode(ciphertext);
        assertThat(strategy.decrypt(key, cipherBytes), is(plaintext));
    }

    @ParameterizedTest
    @ValueSource(strings = {"noSeparator", "onlySecret:", ":onlyIv", ""})
    void shouldThrowWhenKeyFormatIsInvalid(final String invalidKey) {
        assertThrows(Exception.class, () -> strategy.encrypt(invalidKey, "data"));
    }
}
