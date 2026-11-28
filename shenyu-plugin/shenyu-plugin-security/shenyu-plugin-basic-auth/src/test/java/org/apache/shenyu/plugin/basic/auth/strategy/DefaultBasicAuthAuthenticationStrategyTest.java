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

package org.apache.shenyu.plugin.basic.auth.strategy;

import org.apache.shenyu.plugin.basic.auth.rule.DefaultBasicAuthRuleHandle;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DefaultBasicAuthAuthenticationStrategyTest {

    private DefaultBasicAuthAuthenticationStrategy defaultBasicAuthAuthenticationStrategy;

    @BeforeEach
    public void setUp() {
        defaultBasicAuthAuthenticationStrategy = new DefaultBasicAuthAuthenticationStrategy();
    }

    @Test
    public void testParseHandleJson() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        MatcherAssert.assertThat(defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson), notNullValue(DefaultBasicAuthRuleHandle.class));
        MatcherAssert.assertThat(defaultBasicAuthAuthenticationStrategy.parseHandleJson(null), nullValue());
    }

    @Test
    public void testConvert() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        assertTrue(defaultBasicAuthAuthenticationStrategy
            .authenticate(defaultBasicAuthRuleHandle, "test:test123"));
    }

    @Test
    public void testExecuteWithWrongHandleJson() {
        String wrongHandleJson = "{\"wrongAuthorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(wrongHandleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy
            .authenticate(defaultBasicAuthRuleHandle, "test:test123"));
    }

    @Test
    public void testExecuteWithWrongAuthorization() {
        String wrongHandleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(wrongHandleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy
            .authenticate(defaultBasicAuthRuleHandle, "test:test456"));
    }

    @Test
    public void testAuthenticateWithNullAuthentication() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy.authenticate(defaultBasicAuthRuleHandle, null));
    }

    @Test
    public void testAuthenticateWithRfc7617BasicCredential() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic dGVzdDp0ZXN0MTIz"));
    }

    @Test
    public void testAuthenticateWithLowercaseBasicScheme() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "basic dGVzdDp0ZXN0MTIz"));
    }

    @Test
    public void testAuthenticateWithUppercaseBasicScheme() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "BASIC dGVzdDp0ZXN0MTIz"));
    }

    @Test
    public void testAuthenticateWithMultipleSpacesAfterBasicScheme() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic    dGVzdDp0ZXN0MTIz"));
    }

    @Test
    public void testAuthenticateWithSurroundingWhitespace() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "  Basic dGVzdDp0ZXN0MTIz  "));
    }

    @Test
    public void testAuthenticateWithWrongBasicPassword() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic dGVzdDp0ZXN0NDU2"));
    }

    @Test
    public void testAuthenticateWithInvalidBase64Token() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic !!!not-base64!!!"));
    }

    @Test
    public void testAuthenticateWithEmptyBasicToken() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy.authenticate(defaultBasicAuthRuleHandle, "Basic "));
    }

    @Test
    public void testAuthenticateWithInvalidUtf8Token() {
        String handleJson = "{\"authorization\":\"test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy.authenticate(defaultBasicAuthRuleHandle, "Basic /w=="));
    }

    @Test
    public void testAuthenticateWithFullBasicHeaderConfigured() {
        String handleJson = "{\"authorization\":\"Basic dGVzdDp0ZXN0MTIz\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic dGVzdDp0ZXN0MTIz"));
    }

    @Test
    public void testAuthenticateWithNullConfiguredAuthorization() {
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = new DefaultBasicAuthRuleHandle();

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic dGVzdDp0ZXN0MTIz"));
    }

    @Test
    public void testAuthenticateWithNonBase64ConfiguredHeader() {
        String handleJson = "{\"authorization\":\"Basic test:test123\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic test:test123"));
    }

    @Test
    public void testAuthenticateWithBareBasicConfiguredValue() {
        String handleJson = "{\"authorization\":\"Basic\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy.authenticate(defaultBasicAuthRuleHandle, "Basic"));
    }

    @Test
    public void testAuthenticateWithColonInPassword() {
        String credential = "user:pa:ss";
        String handleJson = "{\"authorization\":\"" + credential + "\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic " + base64(credential)));
    }

    @Test
    public void testAuthenticateWithNonAsciiUtf8Password() {
        String credential = "用户:密码";
        String handleJson = "{\"authorization\":\"" + credential + "\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);

        Assertions.assertTrue(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, "Basic " + base64(credential)));
    }

    @Test
    public void testAuthenticateWithIso88591PasswordIsRejected() {
        String credential = "user:pässword";
        String handleJson = "{\"authorization\":\"" + credential + "\"}";
        DefaultBasicAuthRuleHandle defaultBasicAuthRuleHandle = defaultBasicAuthAuthenticationStrategy.parseHandleJson(handleJson);
        String iso88591Header = "Basic " + Base64.getEncoder()
                .encodeToString(credential.getBytes(StandardCharsets.ISO_8859_1));

        Assertions.assertFalse(defaultBasicAuthAuthenticationStrategy
                .authenticate(defaultBasicAuthRuleHandle, iso88591Header));
    }

    private String base64(final String value) {
        return Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8));
    }

}
