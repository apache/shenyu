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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.pii.PiiAction;
import org.apache.shenyu.plugin.ai.pii.detector.BuiltinPiiDetectors;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link PiiDetectionPluginDataHandler} and {@link PiiDetectionRule}.
 */
public final class PiiDetectionPluginDataHandlerTest {

    private PiiDetectionPluginDataHandler handler;

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        handler = new PiiDetectionPluginDataHandler();
        ruleData = new RuleData();
        ruleData.setId("rule-1");
        ruleData.setSelectorId("selector-1");
        ruleData.setPluginName(PluginEnum.PII_DETECTION.getName());
    }

    @AfterEach
    public void tearDown() {
        handler.removeRule(ruleData);
    }

    @Test
    public void testPluginNamed() {
        assertEquals(PluginEnum.PII_DETECTION.getName(), handler.pluginNamed());
    }

    @Test
    public void testHandlerRuleCompilesHandle() {
        ruleData.setHandle("{\"detectors\":\"EMAIL\",\"actions\":\"NATIONAL_ID_CN=BLOCK\","
                + "\"scope\":\"request,response\",\"maxBodySize\":10,\"failClosed\":true}");
        handler.handlerRule(ruleData);
        PiiDetectionRule rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertNotNull(rule);
        assertEquals(1, rule.getDetectors().size());
        assertEquals(BuiltinPiiDetectors.EMAIL, rule.getDetectors().get(0).type());
        assertEquals(PiiAction.BLOCK, rule.actionFor(BuiltinPiiDetectors.NATIONAL_ID_CN));
        assertEquals(PiiAction.REDACT, rule.actionFor(BuiltinPiiDetectors.EMAIL));
        assertTrue(rule.scansRequest());
        assertTrue(rule.scansResponse());
        assertTrue(rule.isOversized(11));
        assertTrue(rule.isFailClosed());
    }

    @Test
    public void testDefaultHandleEnablesAllDetectors() {
        ruleData.setHandle("{}");
        handler.handlerRule(ruleData);
        PiiDetectionRule rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertEquals(BuiltinPiiDetectors.all().size(), rule.getDetectors().size());
        assertTrue(rule.scansRequest());
        assertTrue(!rule.scansResponse());
        assertEquals(PiiAction.REDACT, rule.actionFor(BuiltinPiiDetectors.EMAIL));
    }

    @Test
    public void testInvalidCustomPatternAndActionAreSkipped() {
        ruleData.setHandle("{\"customPatterns\":\"BAD=[unclosed\\nORD=ORD-\\\\d{4}\","
                + "\"actions\":\"EMAIL=NOT_AN_ACTION\"}");
        handler.handlerRule(ruleData);
        PiiDetectionRule rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertEquals(1, rule.getDetectors().size() - BuiltinPiiDetectors.all().size());
        // the unknown action override is dropped, the default applies
        assertEquals(PiiAction.REDACT, rule.actionFor(BuiltinPiiDetectors.EMAIL));
    }

    @Test
    public void testCustomPatternCannotShadowBuiltin() {
        ruleData.setHandle("{\"customPatterns\":\"EMAIL=.*\"}");
        handler.handlerRule(ruleData);
        PiiDetectionRule rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertEquals(BuiltinPiiDetectors.all().size(), rule.getDetectors().size());
    }

    @Test
    public void testBodySizeConfigurationSemantics() {
        ruleData.setHandle("{\"maxBodySize\":-1}");
        handler.handlerRule(ruleData);
        PiiDetectionRule rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertEquals(PiiDetectionHandle.DEFAULT_MAX_BODY_SIZE, rule.getMaxBodySize());
        assertTrue(rule.isOversized(PiiDetectionHandle.DEFAULT_MAX_BODY_SIZE + 1L));
        ruleData.setHandle("{\"maxBodySize\":0}");
        handler.handlerRule(ruleData);
        rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertEquals(0L, rule.getMaxBodySize());
        assertFalse(rule.isOversized(Long.MAX_VALUE));
        ruleData.setHandle("{\"maxBodySize\":10}");
        handler.handlerRule(ruleData);
        rule = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        assertEquals(10L, rule.getMaxBodySize());
        assertFalse(rule.isOversized(10L));
        assertTrue(rule.isOversized(11L));
    }

    @Test
    public void testOversizedConfigurationRetainsPreviousRuleAndPropagatesSanitizedException() {
        ruleData.setHandle("{\"detectors\":\"EMAIL\",\"maxBodySize\":10}");
        handler.handlerRule(ruleData);
        PiiDetectionRule previous = PiiDetectionPluginDataHandler.obtainRule(ruleData);
        PiiDetectionHandle replacement = new PiiDetectionHandle();
        replacement.setCustomPatterns("SECRET_TYPE=secret-regex\n".repeat(PiiDetectionRule.MAX_CUSTOM_PATTERN_COUNT + 1));
        replacement.setMaxBodySize(0L);
        ruleData.setHandle(GsonUtils.getInstance().toJson(replacement));
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> handler.handlerRule(ruleData));
        assertEquals("Custom pattern configuration exceeds the entry count limit", exception.getMessage());
        assertNull(exception.getCause());
        assertSame(previous, PiiDetectionPluginDataHandler.obtainRule(ruleData));
        assertEquals(10L, previous.getMaxBodySize());
        assertEquals(1, previous.getDetectors().size());
    }

    @Test
    public void testOversizedNewRuleDoesNotCachePartialConfiguration() {
        PiiDetectionHandle handle = new PiiDetectionHandle();
        handle.setCustomPatterns("CUSTOM=x\nSECRET_TYPE=" + "x".repeat(PiiDetectionRule.MAX_CUSTOM_REGEX_LENGTH + 1));
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> handler.handlerRule(ruleData));
        assertEquals("Custom pattern configuration exceeds the regex length limit", exception.getMessage());
        assertNull(PiiDetectionPluginDataHandler.obtainRule(ruleData));
    }

    @Test
    public void testNullHandleGivesNoRule() {
        ruleData.setHandle(null);
        handler.handlerRule(ruleData);
        assertNull(PiiDetectionPluginDataHandler.obtainRule(ruleData));
    }
}
