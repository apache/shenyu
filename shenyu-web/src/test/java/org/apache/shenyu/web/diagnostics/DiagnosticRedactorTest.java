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

package org.apache.shenyu.web.diagnostics;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link DiagnosticRedactor}.
 */
public final class DiagnosticRedactorTest {

    private final DiagnosticRedactor redactor = new DiagnosticRedactor();

    @Test
    public void secretKeyDetection() {
        assertTrue(redactor.isSecretKey("password"));
        assertTrue(redactor.isSecretKey("api-key"));
        assertTrue(redactor.isSecretKey("API_KEY"));
        assertTrue(redactor.isSecretKey("authorization"));
        assertTrue(redactor.isSecretKey("clientSecret"));
        assertFalse(redactor.isSecretKey("author"));
        assertFalse(redactor.isSecretKey("name"));
        assertFalse(redactor.isSecretKey("sortKey"));
        assertFalse(redactor.isSecretKey(null));
    }

    @Test
    public void redactJsonHidesSecretValuesAtAnyDepth() {
        String json = "{\"username\":\"admin\",\"password\":\"hunter2\","
                + "\"nested\":{\"apiKey\":\"sk-123\",\"safe\":\"v\"},"
                + "\"list\":[{\"token\":\"t1\"},{\"ok\":\"yes\"}]}";
        String result = redactor.redactJson(json);
        assertTrue(result.contains("admin"));
        assertTrue(result.contains("\"safe\":\"v\""));
        assertTrue(result.contains("\"ok\":\"yes\""));
        assertFalse(result.contains("hunter2"));
        assertFalse(result.contains("sk-123"));
        assertFalse(result.contains("t1"));
        assertTrue(result.contains(DiagnosticRedactor.REDACTED));
    }

    @Test
    public void malformedJsonFallsBackToTextScrubbing() {
        String result = redactor.redactJson("{not-json password=abc123");
        assertFalse(result.contains("abc123"));
    }

    @Test
    public void redactTextScrubsBearerJwtAndAssignments() {
        String text = "Authorization: Bearer abc.def.ghi password=secret123 eyJhbGciOi.eyJzdWI.SflKxw";
        String result = redactor.redactText(text);
        assertFalse(result.contains("secret123"));
        assertFalse(result.contains("abc.def.ghi"));
        assertFalse(result.contains("eyJhbGciOi"));
    }

    @Test
    public void stripUrlRemovesCredentialsQueryAndFragment() {
        String result = redactor.stripUrl("https://user:pass@host:8080/path?token=abc&x=1#frag");
        assertFalse(result.contains("pass"));
        assertFalse(result.contains("token=abc"));
        assertFalse(result.contains("frag"));
        assertTrue(result.contains("host:8080/path"));
        assertTrue(result.contains(DiagnosticRedactor.REDACTED));
    }

    @Test
    public void digestIsStableAndEmptyForBlankInput() {
        assertEquals(64, redactor.digest("abc").length());
        assertEquals(redactor.digest("abc"), redactor.digest("abc"));
        assertFalse(redactor.digest("abc").equals(redactor.digest("abd")));
        assertEquals("", redactor.digest(null));
        assertEquals("", redactor.digest(""));
    }
}
