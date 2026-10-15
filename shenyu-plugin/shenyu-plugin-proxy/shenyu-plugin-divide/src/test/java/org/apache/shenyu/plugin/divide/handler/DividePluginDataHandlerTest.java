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

package org.apache.shenyu.plugin.divide.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The type divide plugin data handler test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DividePluginDataHandlerTest {

    private SelectorData selectorData;

    @Mock
    private RuleData ruleData;

    private DividePluginDataHandler dividePluginDataHandler;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @BeforeEach
    public void setUp() {
        this.dividePluginDataHandler = new DividePluginDataHandler();
        List<DivideUpstream> divideUpstreamList = Stream.of(3)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("mock-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("handler");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @AfterEach
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Remove selector test.
     */
    @Test
    public void removeSelectorTest() {
        dividePluginDataHandler.handlerSelector(selectorData);
        dividePluginDataHandler.removeSelector(selectorData);
        List<Upstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        assertNull(result);
    }

    /**
     * Plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals(dividePluginDataHandler.pluginNamed(), PluginEnum.DIVIDE.getName());
    }

    /**
     * Plugin named test.
     */
    @Test
    public void removeRuleTest() {
        dividePluginDataHandler.removeRule(ruleData);
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"percentage\":101", "\"percentage\":20.5", "\"enabled\":true,\"percentage\":20",
        "\"fallbackPolicy\":\"TYPO\"", "\"stickyKey\":{\"paramType\":\"missing-extension\"}"})
    public void testInvalidConfigurationDoesNotReplaceCachedRule(final String fields) {
        RuleData rule = new RuleData();
        rule.setId("invalid-config-rule");
        rule.setSelectorId("invalid-config-selector");
        rule.setHandle("{\"timeout\":5000}");
        dividePluginDataHandler.handlerRule(rule);
        DivideRuleHandle previous = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        rule.setHandle("{\"canary\":{\"stableLabels\":{\"release\":\"stable\"},\"canaryLabels\":{\"release\":\"canary\"}," + fields + "}}");
        try {
            assertThrows(IllegalArgumentException.class, () -> dividePluginDataHandler.handlerRule(rule));
            assertSame(previous, DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)));
        } finally {
            dividePluginDataHandler.removeRule(rule);
        }
    }

    @Test
    public void testInstalledCustomParameterSourceCanBeCached() {
        RuleData rule = new RuleData();
        rule.setId("custom-source-rule");
        rule.setSelectorId("custom-source-selector");
        rule.setHandle("{\"canary\":{\"enabled\":true,\"percentage\":20,\"stickyKey\":{\"paramType\":\"test_attribute\"},"
                + "\"stableLabels\":{\"release\":\"stable\"},\"canaryLabels\":{\"release\":\"canary\"}}}");
        try {
            dividePluginDataHandler.handlerRule(rule);
            DivideRuleHandle cached = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
            assertEquals("test_attribute", cached.getCanary().getStickyKey().getParamType());
        } finally {
            dividePluginDataHandler.removeRule(rule);
        }
    }

    @Test
    public void testRejectOverlappingLabelsBeforeReplacingCachedRule() {
        RuleData rule = new RuleData();
        rule.setId("label-validation-rule");
        rule.setSelectorId("label-validation-selector");
        CanaryConfig config = new CanaryConfig();
        config.setEnabled(false);
        config.setCanaryLabels(Map.of("release", "canary"));
        config.setStableLabels(Map.of("release", "stable"));
        DivideRuleHandle handle = new DivideRuleHandle();
        handle.setCanary(config);
        rule.setHandle(GsonUtils.getGson().toJson(handle));
        dividePluginDataHandler.handlerRule(rule);
        final DivideRuleHandle previous = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        config.setStableLabels(Map.of("region", "east"));
        rule.setHandle(GsonUtils.getGson().toJson(handle));
        assertThrows(IllegalArgumentException.class, () -> dividePluginDataHandler.handlerRule(rule));
        assertSame(previous, DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule)));
        dividePluginDataHandler.removeRule(rule);
    }

}
