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

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link VersionTwoSignProvider}.
 */
final class VersionTwoSignProviderTest {

    private static final String SIGN_KEY = "sign-key";

    private static final String PARAMETERS = "encoded-parameters";

    private final SignProvider signProvider = new VersionTwoSignProvider();

    @Test
    void testGenerateSignWithoutRequestBody() {
        SignParameters signParameters = createSignParameters(URI.create("https://example.com/api/orders?channel=web"));

        String actual = signProvider.generateSign(SIGN_KEY, signParameters);

        assertEquals(sign(PARAMETERS + "/api/orders?channel=web"), actual);
    }

    @Test
    void testGenerateSignWithRequestBody() {
        SignParameters signParameters = createSignParameters(URI.create("https://example.com/api/orders"));
        String requestBody = "{\"name\":\"ShenYu\"}";

        String actual = signProvider.generateSign(SIGN_KEY, signParameters, requestBody);

        assertEquals(sign(PARAMETERS + "/api/orders" + requestBody), actual);
    }

    private SignParameters createSignParameters(final URI uri) {
        SignParameters signParameters = new SignParameters(VERSION_2, "app-key", "1700000000000",
                "signature", uri, SignUtils.SIGN_MD5);
        signParameters.setParameters(PARAMETERS);
        return signParameters;
    }

    private String sign(final String data) {
        return SignUtils.sign(SignUtils.SIGN_MD5, SIGN_KEY, data).toUpperCase();
    }
}
