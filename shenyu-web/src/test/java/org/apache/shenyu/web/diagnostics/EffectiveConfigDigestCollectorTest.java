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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link EffectiveConfigDigestCollector}, focused on the seeded-secret leak fixture.
 */
public final class EffectiveConfigDigestCollectorTest {

    private static final String SELECTOR_HANDLE = "[{\"password\":\"hunter2\",\"upstream\":\"10.0.0.1:8080\"}]";

    private static final String RULE_HANDLE = "{\"apiKey\":\"sk-secret\",\"path\":\"/order\"}";

    private static final String PLUGIN_CONFIG = "{\"clientSecret\":\"cfg-secret\"}";

    private final DiagnosticRedactor redactor = new DiagnosticRedactor();

    private final EffectiveConfigDigestCollector collector = new EffectiveConfigDigestCollector(redactor);

    @BeforeEach
    public void setUp() {
        cleanCaches();
        BaseDataCache base = BaseDataCache.getInstance();
        base.cachePluginData(PluginData.builder().id("p1").name("divide").config(PLUGIN_CONFIG).role("1").enabled(true).build());
        base.cacheSelectData(SelectorData.builder().id("s1").name("sel").pluginName("divide").handle(SELECTOR_HANDLE).enabled(true).build());
        base.cacheRuleData(RuleData.builder().id("r1").name("rule").pluginName("divide").selectorId("s1").handle(RULE_HANDLE).build());
    }

    @AfterEach
    public void tearDown() {
        cleanCaches();
    }

    @Test
    public void seededSecretsNeverAppearInTheSection() {
        DiagnosticSection section = collector.collect(new DiagnosticBudget(2000L, 524288));
        String content = new String(section.getContent(), StandardCharsets.UTF_8);
        assertEquals(SectionStatus.OK, section.getStatus());
        assertFalse(content.contains("hunter2"));
        assertFalse(content.contains("sk-secret"));
        assertFalse(content.contains("cfg-secret"));
        assertFalse(content.contains("password"));
        assertFalse(content.contains("apiKey"));
    }

    @Test
    public void structuralIdentityAndDigestsArePreserved() {
        DiagnosticSection section = collector.collect(new DiagnosticBudget(2000L, 524288));
        String content = new String(section.getContent(), StandardCharsets.UTF_8);
        assertTrue(content.contains("divide"));
        assertTrue(content.contains(redactor.digest(SELECTOR_HANDLE)));
        assertTrue(content.contains(redactor.digest(RULE_HANDLE)));
        assertTrue(content.contains(redactor.digest(PLUGIN_CONFIG)));
    }

    private void cleanCaches() {
        BaseDataCache base = BaseDataCache.getInstance();
        base.cleanPluginData();
        base.cleanSelectorData();
        base.cleanRuleData();
    }
}
