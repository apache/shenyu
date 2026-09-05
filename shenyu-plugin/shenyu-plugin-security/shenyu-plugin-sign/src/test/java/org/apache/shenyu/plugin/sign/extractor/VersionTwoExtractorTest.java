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

package org.apache.shenyu.plugin.sign.extractor;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for {@link VersionTwoExtractor}.
 */
final class VersionTwoExtractorTest {

    private final SignParameterExtractor extractor = new VersionTwoExtractor();

    @Test
    void testExtractFromShenyuAuthorizationHeader() {
        String parameters = parameters("preferred-app-key", "1700000000000", "SHA-256");
        String fallbackParameters = parameters("fallback-app-key", "1600000000000", "MD5");
        HttpRequest request = MockServerHttpRequest.get("https://example.com/api/orders")
                .header(Constants.SHENYU_AUTHORIZATION, parameters + ".preferred-signature")
                .header(HttpHeaders.AUTHORIZATION, fallbackParameters + ".fallback-signature")
                .build();

        SignParameters actual = extractor.extract(request);

        assertSignParameters(actual, request, parameters, "preferred-app-key", "1700000000000",
                "preferred-signature", "SHA-256");
    }

    @Test
    void testExtractFromAuthorizationHeader() {
        String parameters = parameters("app-key", "1700000000000", "MD5");
        HttpRequest request = MockServerHttpRequest.get("https://example.com/api/orders")
                .header(HttpHeaders.AUTHORIZATION, parameters + ".signature")
                .build();

        SignParameters actual = extractor.extract(request);

        assertSignParameters(actual, request, parameters, "app-key", "1700000000000", "signature", "MD5");
    }

    @Test
    void testExtractWithMissingAuthorizationHeader() {
        SignParameters actual = extractor.extract(MockServerHttpRequest.get("https://example.com/api/orders").build());

        assertEmpty(actual);
    }

    @Test
    void testExtractWithTokenWithoutSignatureSeparator() {
        HttpRequest request = MockServerHttpRequest.get("https://example.com/api/orders")
                .header(Constants.SHENYU_AUTHORIZATION, "parameters-only")
                .build();

        SignParameters actual = extractor.extract(request);

        assertEmpty(actual);
    }

    private String parameters(final String appKey, final String timestamp, final String algorithm) {
        Map<String, String> values = new HashMap<>();
        values.put(Constants.APP_KEY, appKey);
        values.put(Constants.TIMESTAMP, timestamp);
        values.put("alg", algorithm);
        return Base64.getEncoder().encodeToString(JsonUtils.toJson(values).getBytes(StandardCharsets.UTF_8));
    }

    private void assertSignParameters(final SignParameters actual, final HttpRequest request, final String parameters,
                                      final String appKey, final String timestamp, final String signature,
                                      final String algorithm) {
        assertEquals(VERSION_2, actual.getVersion());
        assertEquals(appKey, actual.getAppKey());
        assertEquals(timestamp, actual.getTimestamp());
        assertEquals(signature, actual.getSignature());
        assertEquals(request.getURI(), actual.getUri());
        assertEquals(algorithm, actual.getSignAlg());
        assertEquals(parameters, actual.getParameters());
    }

    private void assertEmpty(final SignParameters actual) {
        assertNull(actual.getVersion());
        assertNull(actual.getAppKey());
        assertNull(actual.getTimestamp());
        assertNull(actual.getSignature());
        assertNull(actual.getUri());
        assertNull(actual.getParameters());
    }
}
