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

package org.apache.shenyu.plugin.shadow.compare;

import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test case for {@link ShadowComparator}.
 */
public final class ShadowComparatorTest {

    @Test
    public void testMatchesEqualJson() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{\"a\":1}"), observation(200, "application/json", "{\"a\":1}"));
        assertEquals(ShadowOutcome.MATCH, comparison.getOutcome());
    }

    @Test
    public void testMismatchesStatus() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{}"), observation(500, "application/json", "{}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertTrue(comparison.getDetail().contains("status"));
    }

    @Test
    public void testMismatchesComparedHeader() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{}"), observation(200, "text/plain", "{}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertTrue(comparison.getDetail().contains("Content-Type"));
    }

    @Test
    public void testNormalizesIgnoredJsonPath() {
        ShadowHandle handle = handle();
        handle.setIgnoreJsonPaths(List.of("$.timestamp"));
        ShadowComparison comparison = ShadowComparator.compare(handle,
                observation(200, "application/json", "{\"a\":1,\"timestamp\":1}"),
                observation(200, "application/json", "{\"a\":1,\"timestamp\":2}"));
        assertEquals(ShadowOutcome.NORMALIZED, comparison.getOutcome());
    }

    @Test
    public void testMismatchesJsonPath() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{\"a\":1,\"b\":{\"c\":2}}"),
                observation(200, "application/json", "{\"a\":1,\"b\":{\"c\":3}}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertTrue(comparison.getDetail().contains("$.b.c"));
    }

    @Test
    public void testMismatchesArrayLength() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{\"a\":[1]}"),
                observation(200, "application/json", "{\"a\":[1,2]}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertTrue(comparison.getDetail().contains("$.a.length"));
    }

    @Test
    public void testTruncatedWins() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{}", true), observation(200, "application/json", "{}"));
        assertEquals(ShadowOutcome.TRUNCATED, comparison.getOutcome());
    }

    @Test
    public void testComparesBytesForNonJson() {
        ShadowHandle handle = handle();
        handle.setCompareHeaders(Collections.emptyList());
        assertEquals(ShadowOutcome.MATCH, ShadowComparator.compare(handle,
                observation(200, "text/plain", "same"), observation(200, "text/plain", "same")).getOutcome());
        assertEquals(ShadowOutcome.MISMATCH, ShadowComparator.compare(handle,
                observation(200, "text/plain", "one"), observation(200, "text/plain", "two")).getOutcome());
    }

    @Test
    public void testMismatchesContentTypeBody() {
        ShadowHandle handle = handle();
        handle.setCompareHeaders(Collections.emptyList());
        ShadowComparison comparison = ShadowComparator.compare(handle,
                observation(200, "application/json", "{}"), observation(200, "text/plain", "{}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertTrue(comparison.getDetail().contains("content type"));
    }

    @Test
    public void testMismatchesInvalidJson() {
        ShadowHandle handle = handle();
        handle.setCompareHeaders(Collections.emptyList());
        ShadowComparison comparison = ShadowComparator.compare(handle,
                observation(200, "application/json", "{"), observation(200, "application/json", "{}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertTrue(comparison.getDetail().contains("not valid JSON"));
    }

    @Test
    public void testDetailNeverCarriesValues() {
        ShadowComparison comparison = ShadowComparator.compare(handle(),
                observation(200, "application/json", "{\"token\":\"primary-secret\"}"),
                observation(200, "application/json", "{\"token\":\"shadow-secret\"}"));
        assertEquals(ShadowOutcome.MISMATCH, comparison.getOutcome());
        assertFalse(comparison.getDetail().contains("secret"));
        assertTrue(comparison.getDetail().contains("$.token"));
    }

    private ShadowHandle handle() {
        ShadowHandle handle = new ShadowHandle();
        handle.setEnabled(true);
        handle.setTarget("http://shadow:8080");
        return handle;
    }

    private ShadowObservation observation(final int status, final String contentType, final String body) {
        return observation(status, contentType, body, false);
    }

    private ShadowObservation observation(final int status, final String contentType, final String body,
                                          final boolean truncated) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(HttpHeaders.CONTENT_TYPE, contentType);
        return new ShadowObservation(status, headers, body.getBytes(StandardCharsets.UTF_8), truncated, 0L);
    }
}
