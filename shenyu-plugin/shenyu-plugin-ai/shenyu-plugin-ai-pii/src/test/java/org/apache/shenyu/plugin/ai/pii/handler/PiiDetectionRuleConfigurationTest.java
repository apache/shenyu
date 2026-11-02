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

package org.apache.shenyu.plugin.ai.pii.handler;

import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.plugin.ai.pii.detector.BuiltinPiiDetectors;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Configuration bounds for {@link PiiDetectionRule}.
 */
public final class PiiDetectionRuleConfigurationTest {

    @Test
    public void testNegativeBodySizeFallsBackToDefault() {
        for (long size : new long[]{-1L, Long.MIN_VALUE}) {
            PiiDetectionHandle handle = new PiiDetectionHandle();
            handle.setMaxBodySize(size);
            PiiDetectionRule rule = PiiDetectionRule.compile("negative", handle);
            assertEquals(PiiDetectionHandle.DEFAULT_MAX_BODY_SIZE, rule.getMaxBodySize());
            assertFalse(rule.isOversized(PiiDetectionHandle.DEFAULT_MAX_BODY_SIZE));
            assertTrue(rule.isOversized(PiiDetectionHandle.DEFAULT_MAX_BODY_SIZE + 1L));
        }
    }

    @Test
    public void testZeroBodySizeIsUnlimited() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(0L);
        PiiDetectionRule rule = PiiDetectionRule.compile("unlimited", handle);
        assertEquals(0L, rule.getMaxBodySize());
        assertFalse(rule.isOversized(Long.MAX_VALUE));
    }

    @Test
    public void testPositiveBodySizeIsFinite() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setMaxBodySize(10L);
        PiiDetectionRule rule = PiiDetectionRule.compile("finite", handle);
        assertEquals(10L, rule.getMaxBodySize());
        assertFalse(rule.isOversized(-1L));
        assertFalse(rule.isOversized(10L));
        assertTrue(rule.isOversized(11L));
    }

    @Test
    public void testCustomPatternCountBoundary() {
        StringBuilder patterns = new StringBuilder();
        for (int index = 0; index < PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT; index++) {
            patterns.append("CUSTOM_").append(index).append("=x\n");
        }
        PiiDetectionRule rule = compile(patterns.toString());
        assertEquals(BuiltinPiiDetectors.all().size() + PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT, rule.getDetectors().size());
        assertRejected(patterns.append("SECRET_TYPE=secret-regex").toString(),
                "Custom pattern configuration exceeds the entry count limit");
    }

    @Test
    public void testDuplicateEntriesCountBeforeDeduplication() {
        String entry = "SECRET_TYPE=secret-regex\n";
        PiiDetectionRule rule = compile(entry.repeat(PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT));
        assertEquals(BuiltinPiiDetectors.all().size() + 1, rule.getDetectors().size());
        assertRejected(entry.repeat(PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT + 1),
                "Custom pattern configuration exceeds the entry count limit");
    }

    @Test
    public void testInvalidNonblankLinesCountButBlankLinesDoNot() {
        String invalid = "missing-delimiter\r\n";
        String blanks = " \t\r\n\n".repeat(PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT + 1);
        PiiDetectionRule rule = compile(blanks + invalid.repeat(PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT - 1) + "CUSTOM=x");
        assertEquals(BuiltinPiiDetectors.all().size() + 1, rule.getDetectors().size());
        assertRejected(invalid.repeat(PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT) + "CUSTOM=x",
                "Custom pattern configuration exceeds the entry count limit");
    }

    @Test
    public void testRegexLengthBoundaryExcludesTypeAndLineEndingCarriageReturn() {
        String prefix = "CUSTOM_" + "T".repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH) + "=";
        String regex = "=".repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH);
        PiiDetectionRule rule = compile(prefix + regex + "\r\n");
        assertEquals(BuiltinPiiDetectors.all().size() + 1, rule.getDetectors().size());
        assertRejected(prefix + regex + "x\r\n", "Custom pattern configuration exceeds the regex length limit");
        assertRejected("CUSTOM=" + regex + "x", "Custom pattern configuration exceeds the regex length limit");
    }

    @Test
    public void testRegexLengthCountsUtf16Characters() {
        String regex = new String(Character.toChars(0x1F600)).repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH / 2);
        assertNotNull(compile("CUSTOM=" + regex));
        assertRejected("CUSTOM=" + regex + "x", "Custom pattern configuration exceeds the regex length limit");
    }

    @Test
    public void testRegexLengthCannotBeBypassedByTrimming() {
        assertRejected("CUSTOM=x" + " ".repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH),
                "Custom pattern configuration exceeds the regex length limit");
        assertRejected("=" + "x".repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH + 1),
                "Custom pattern configuration exceeds the regex length limit");
    }

    @Test
    public void testOversizedRegexCannotBeHiddenByDuplicateType() {
        assertRejected("SECRET_TYPE=" + "x".repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH + 1) + "\nSECRET_TYPE=x",
                "Custom pattern configuration exceeds the regex length limit");
    }

    @Test
    public void testRawConfigurationLengthBoundaryIncludesBlankLines() {
        String entry = "CUSTOM=x\n";
        String patterns = entry + " ".repeat(PiiDetectionRule.MAX_CUSTOM_PATTERNS_LENGTH - entry.length());
        PiiDetectionRule rule = compile(patterns);
        assertEquals(BuiltinPiiDetectors.all().size() + 1, rule.getDetectors().size());
        assertRejected(patterns + " ", "Custom pattern configuration exceeds the total length limit");
    }

    @Test
    public void testRawLengthIsCheckedBeforeBlankConfigurationIsSkipped() {
        assertNotNull(compile(null));
        assertNotNull(compile(""));
        assertNotNull(compile(" ".repeat(PiiDetectionRule.MAX_CUSTOM_PATTERNS_LENGTH)));
        assertRejected(" ".repeat(PiiDetectionRule.MAX_CUSTOM_PATTERNS_LENGTH + 1),
                "Custom pattern configuration exceeds the total length limit");
    }

    private PiiDetectionRule compile(final String patterns) {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setCustomPatterns(patterns);
        return PiiDetectionRule.compile("configuration", handle);
    }

    private void assertRejected(final String patterns, final String message) {
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> compile(patterns));
        assertEquals(message, exception.getMessage());
        assertNull(exception.getCause());
    }
}
