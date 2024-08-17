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

}
