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

package org.apache.shenyu.plugin.ai.requestid.handle;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiRequestIdRuleParserTest {

    private final AiRequestIdRuleParser parser = new AiRequestIdRuleParser();

    @Test
    void shouldRejectBlankHandle() {
        assertInvalid(parser.parse(null));
        assertInvalid(parser.parse(""));
        assertInvalid(parser.parse("   "));
    }

    @Test
    void shouldRejectMalformedJsonAndNonObjects() {
        assertInvalid(parser.parse("not-json"));
        assertInvalid(parser.parse("[]"));
        assertInvalid(parser.parse("\"value\""));
        assertInvalid(parser.parse("42"));
    }

    @Test
    void shouldDefaultResponseHeaderToEnabled() {
        AiRequestIdRule rule = parser.parse("{}");

        assertTrue(rule.isValid());
        assertTrue(rule.isResponseHeaderEnabled());
    }

    @Test
    void shouldParseDisabledResponseHeader() {
        AiRequestIdRule rule = parser.parse("{\"responseHeaderEnabled\":false}");

        assertTrue(rule.isValid());
        assertFalse(rule.isResponseHeaderEnabled());
    }

    @Test
    void shouldRejectNonBooleanResponseHeader() {
        assertInvalid(parser.parse("{\"responseHeaderEnabled\":\"false\"}"));
        assertInvalid(parser.parse("{\"responseHeaderEnabled\":null}"));
        assertInvalid(parser.parse("{\"responseHeaderEnabled\":1}"));
    }

    @Test
    void shouldIgnoreUnknownFields() {
        AiRequestIdRule rule = parser.parse("{\"responseHeaderEnabled\":true,\"future\":\"value\"}");

        assertTrue(rule.isValid());
        assertTrue(rule.isResponseHeaderEnabled());
    }

    @Test
    void shouldCarryRawHandleForCacheComparison() {
        String handle = "{\"responseHeaderEnabled\":true}";

        assertEquals(handle, parser.parse(handle).getRawHandle());
    }

    private static void assertInvalid(final AiRequestIdRule rule) {
        assertFalse(rule.isValid());
        assertNotNull(rule.getErrorMessage());
    }
}
