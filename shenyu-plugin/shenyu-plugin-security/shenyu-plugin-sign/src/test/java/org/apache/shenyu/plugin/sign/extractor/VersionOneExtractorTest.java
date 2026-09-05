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
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_1;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for {@link VersionOneExtractor}.
 */
final class VersionOneExtractorTest {

    private final SignParameterExtractor extractor = new VersionOneExtractor();

    @Test
    void testExtractSignParameters() {
        HttpRequest request = MockServerHttpRequest.get("https://example.com/api/orders?id=1")
                .header(Constants.APP_KEY, "app-key")
                .header(Constants.TIMESTAMP, "1700000000000")
                .header(Constants.SIGN, "signature")
                .build();

        SignParameters actual = extractor.extract(request);

        assertEquals(VERSION_1, actual.getVersion());
        assertEquals("app-key", actual.getAppKey());
        assertEquals("1700000000000", actual.getTimestamp());
        assertEquals("signature", actual.getSignature());
        assertEquals(request.getURI(), actual.getUri());
        assertEquals(SignUtils.SIGN_MD5, actual.getSignAlg());
    }

    @Test
    void testExtractWithMissingHeaders() {
        HttpRequest request = MockServerHttpRequest.get("https://example.com/api/orders").build();

        SignParameters actual = extractor.extract(request);

        assertEquals(VERSION_1, actual.getVersion());
        assertNull(actual.getAppKey());
        assertNull(actual.getTimestamp());
        assertNull(actual.getSignature());
        assertEquals(request.getURI(), actual.getUri());
    }
}
