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

package org.apache.shenyu.plugin.ai.pii;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.apache.shenyu.plugin.ai.pii.profile.OpenAiResponsesProfile;
import org.apache.shenyu.plugin.ai.pii.profile.PiiTextSlot;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link PiiScanner}, focused on how overlapping detections are
 * redacted: the ranges are merged on the coordinates of the original text, so
 * a narrower detection inside a wider one never leaves a partially redacted
 * island behind.
 */
public final class PiiScannerTest {

    private static PiiDetectionRule ruleWithCustomDomain() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDetectors("EMAIL");
        handle.setCustomPatterns("DOMAIN=example\\.com");
        return PiiDetectionRule.compile("rule-1", handle);
    }

    private static PiiDetectionRule ruleWithEmail() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDetectors("EMAIL");
        return PiiDetectionRule.compile("rule-email", handle);
    }

    private static List<PiiTextSlot> slotOf(final StringBuilder value) {
        List<PiiTextSlot> slots = new ArrayList<>();
        slots.add(new PiiTextSlot() {

            @Override
            public String text() {
                return value.toString();
            }

            @Override
            public void replace(final String newText) {
                value.setLength(0);
                value.append(newText);
            }
        });
        return slots;
    }

    @Test
    public void testContainedOverlapMergesIntoTheWiderPlaceholder() {
        StringBuilder slot = new StringBuilder("contact alice@example.com now");
        // the email detector covers [8, 25) and the custom domain detector [14, 25),
        // the contained domain must not carve its own placeholder out of the address
        PiiScanner.PiiOutcome outcome = new PiiScanner(ruleWithCustomDomain(), "request")
                .scan(slotOf(slot));
        assertTrue(outcome.isChanged());
        assertEquals("contact <EMAIL> now", slot.toString());
        assertFalse(slot.toString().contains("alice"));
        assertFalse(slot.toString().contains("@"));
        // both detections are audited even though only one placeholder is written
        assertEquals(2, outcome.getAudits().size());
    }

    @Test
    public void testCrossingOverlapMergesBothRanges() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDetectors("");
        handle.setCustomPatterns("P1=ABCDE\nP2=CDEFG");
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        StringBuilder slot = new StringBuilder("ABCDEFGHIJ");
        new PiiScanner(rule, "request").scan(slotOf(slot));
        // P1 covers [0, 5) and P2 covers [2, 7), the merged range takes the
        // placeholder of the match that starts first
        assertEquals("<P1>HIJ", slot.toString());
    }

    @Test
    public void testAdjacentDetectionsKeepSeparatePlaceholders() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDetectors("PHONE_CN");
        handle.setCustomPatterns("ORD=ORD-\\d+");
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        StringBuilder slot = new StringBuilder("order ORD-12345678 then call 13800138000");
        new PiiScanner(rule, "request").scan(slotOf(slot));
        assertEquals("order <ORD> then call <PHONE_CN>", slot.toString());
    }

    @Test
    public void testBlockWinsOverRedactOnOverlap() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDetectors("EMAIL");
        handle.setCustomPatterns("DOMAIN=example\\.com");
        handle.setActions("DOMAIN=BLOCK");
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        StringBuilder slot = new StringBuilder("contact alice@example.com");
        PiiScanner.PiiOutcome outcome = new PiiScanner(rule, "request").scan(slotOf(slot));
        assertTrue(outcome.isBlocked());
        assertFalse(outcome.isChanged());
        assertEquals("contact alice@example.com", slot.toString());
    }

    @Test
    public void testAuditOnlyDetectionNeverChangesTheSlot() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setDetectors("PHONE_CN");
        handle.setDefaultAction("AUDIT");
        PiiDetectionRule rule = PiiDetectionRule.compile("rule-1", handle);
        StringBuilder slot = new StringBuilder("call 13800138000");
        PiiScanner.PiiOutcome outcome = new PiiScanner(rule, "request").scan(slotOf(slot));
        assertFalse(outcome.isChanged());
        assertEquals("call 13800138000", slot.toString());
        assertEquals(1, outcome.getAudits().size());
        assertEquals("PHONE_CN", outcome.getAudits().get(0).getPiiType());
    }

    @Test
    public void testEscapedEmailInToolArgumentsIsRedacted() {
        // a function_call arguments member is a serialized JSON document: its value encodes the
        // at-sign as a JSON unicode escape, so the raw member text has no literal at-sign yet the
        // tool decodes it to alice@example.com; the decoded value must be scanned and redacted
        String escapedAt = "alice" + '\\' + "u0040example.com";
        JsonObject functionCall = new JsonObject();
        functionCall.addProperty("type", "function_call");
        functionCall.addProperty("name", "send");
        functionCall.addProperty("arguments", "{\"email\":\"" + escapedAt + "\"}");
        JsonArray output = new JsonArray();
        output.add(functionCall);
        JsonObject root = new JsonObject();
        root.add("output", output);

        List<PiiTextSlot> slots = new OpenAiResponsesProfile().responseSlots(root);
        PiiScanner.PiiOutcome outcome = new PiiScanner(ruleWithEmail(), "response").scan(slots);
        assertTrue(outcome.isChanged());
        assertEquals("{\"email\":\"<EMAIL>\"}", functionCall.get("arguments").getAsString());
    }
}
