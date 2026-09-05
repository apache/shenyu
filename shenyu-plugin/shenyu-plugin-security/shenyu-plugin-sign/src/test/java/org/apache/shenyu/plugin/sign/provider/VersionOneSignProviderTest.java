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

package org.apache.shenyu.plugin.sign.provider;

import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_1;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link VersionOneSignProvider}.
 */
final class VersionOneSignProviderTest {

    private static final String SIGN_KEY = "sign-key";

    private static final String TIMESTAMP = "1700000000000";

    private final SignProvider signProvider = new VersionOneSignProvider();

    @Test
    void testGenerateSignWithoutRequestBody() {
        SignParameters signParameters = createSignParameters(URI.create("https://example.com/api/orders"));
        String data = "path/api/orderstimestamp" + TIMESTAMP + "version" + VERSION_1;

        String actual = signProvider.generateSign(SIGN_KEY, signParameters);

        assertEquals(sign(data), actual);
    }

    @Test
    void testGenerateSignWithRequestBodyAndQuery() {
        SignParameters signParameters = createSignParameters(URI.create("https://example.com/api/orders?channel=web"));
        String requestBody = "{\"name\":\"ShenYu\",\"count\":2}";
        String data = "channelwebcount2nameShenYupath/api/orderstimestamp" + TIMESTAMP + "version" + VERSION_1;

        String actual = signProvider.generateSign(SIGN_KEY, signParameters, requestBody);

        assertEquals(sign(data), actual);
    }

    @Test
    void testGenerateSignWithEmptyRequestBody() {
        SignParameters signParameters = createSignParameters(URI.create("https://example.com/api/orders?channel=web"));
        String data = "channelwebpath/api/orderstimestamp" + TIMESTAMP + "version" + VERSION_1;

        String actual = signProvider.generateSign(SIGN_KEY, signParameters, "");

        assertEquals(sign(data), actual);
    }

    @Test
    void testGenerateSignIgnoresSignatureParameter() {
        SignParameters signParameters = createSignParameters(URI.create("https://example.com/api/orders"));
        String requestBody = "{\"name\":\"ShenYu\",\"sign\":\"untrusted-signature\"}";
        String data = "nameShenYupath/api/orderstimestamp" + TIMESTAMP + "version" + VERSION_1;

        String actual = signProvider.generateSign(SIGN_KEY, signParameters, requestBody);

        assertEquals(sign(data), actual);
    }

    private SignParameters createSignParameters(final URI uri) {
        return new SignParameters(VERSION_1, "app-key", TIMESTAMP, "signature", uri, SignUtils.SIGN_MD5);
    }

    private String sign(final String data) {
        return SignUtils.sign(SignUtils.SIGN_MD5, SIGN_KEY, data).toUpperCase();
    }
}
