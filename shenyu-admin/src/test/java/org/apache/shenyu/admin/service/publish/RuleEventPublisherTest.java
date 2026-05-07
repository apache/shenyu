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

package org.apache.shenyu.admin.service.publish;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.event.rule.RuleChangedEvent;
import org.apache.shenyu.admin.model.event.rule.RuleCreatedEvent;
import org.apache.shenyu.admin.model.event.rule.RuleUpdatedEvent;
import org.apache.shenyu.admin.model.event.selector.BatchSelectorDeletedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link RuleEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class RuleEventPublisherTest {

    private static final String TEST_OPERATOR = "test-operator";

    private static final String TEST_PLUGIN_NAME = "test-plugin";

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    @Mock
    private RuleConditionMapper ruleConditionMapper;

    @Mock
    private RuleMapper ruleMapper;

    private RuleEventPublisher ruleEventPublisher;

    private MockedStatic<SessionUtil> sessionUtilMockedStatic;

    @BeforeEach
    void setUp() {
        ruleEventPublisher = new RuleEventPublisher(applicationEventPublisher, ruleConditionMapper, ruleMapper);
        sessionUtilMockedStatic = mockStatic(SessionUtil.class);
        sessionUtilMockedStatic.when(SessionUtil::visitorName).thenReturn(TEST_OPERATOR);
    }

    @AfterEach
    void tearDown() {
        sessionUtilMockedStatic.close();
    }

    @Test
    void testOnCreatedSimple() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");

        ruleEventPublisher.onCreated(rule);

        ArgumentCaptor<RuleCreatedEvent> captor = ArgumentCaptor.forClass(RuleCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        RuleCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(rule, event.getRule());
    }

    @Test
    void testOnCreatedWithCondition() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");
        List<RuleConditionDTO> conditions = Arrays.asList(
                buildRuleConditionDTO("1", "1", "header", "=", "test", "value1")
        );

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onCreated(rule, conditions);

        ArgumentCaptor<RuleCreatedEvent> createdCaptor = ArgumentCaptor.forClass(RuleCreatedEvent.class);
        ArgumentCaptor<DataChangedEvent> dataChangedCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher).publishEvent(createdCaptor.capture());
        verify(applicationEventPublisher).publishEvent(dataChangedCaptor.capture());

        RuleCreatedEvent createdEvent = createdCaptor.getValue();
        assertNotNull(createdEvent);
        assertEquals(rule, createdEvent.getRule());

        DataChangedEvent dataChangedEvent = dataChangedCaptor.getValue();
        assertNotNull(dataChangedEvent);
        assertEquals(ConfigGroupEnum.RULE, dataChangedEvent.getGroupKey());
    }

    @Test
    void testOnUpdatedSimple() {
        RuleDO before = buildRuleDO("1", "selector1", "old-rule");
        RuleDO after = buildRuleDO("1", "selector1", "new-rule");

        ruleEventPublisher.onUpdated(after, before);

        ArgumentCaptor<RuleUpdatedEvent> captor = ArgumentCaptor.forClass(RuleUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        RuleUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnUpdatedWithCondition() {
        RuleDO before = buildRuleDO("1", "selector1", "old-rule");
        RuleDO after = buildRuleDO("1", "selector1", "new-rule");
        List<RuleConditionDTO> conditions = Arrays.asList(
                buildRuleConditionDTO("1", "1", "header", "=", "test", "value1")
        );

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onUpdated(after, before, conditions);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnUpdatedWithFullConditions() {
        RuleDO before = buildRuleDO("1", "selector1", "old-rule");
        RuleDO after = buildRuleDO("1", "selector1", "new-rule");
        List<RuleConditionDTO> conditions = Arrays.asList(
                buildRuleConditionDTO("1", "1", "header", "=", "test", "value1")
        );
        List<RuleConditionDO> beforeConditions = Arrays.asList(
                buildRuleConditionDO("1", "1", "header", "=", "test", "oldValue")
        );

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onUpdated(after, before, conditions, beforeConditions);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedSingle() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");

        ruleEventPublisher.onDeleted(rule);

        ArgumentCaptor<RuleChangedEvent> captor = ArgumentCaptor.forClass(RuleChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        RuleChangedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testOnDeletedCollection() {
        RuleDO rule1 = buildRuleDO("1", "selector1", "rule1");
        RuleDO rule2 = buildRuleDO("2", "selector1", "rule2");
        List<RuleDO> rules = Arrays.asList(rule1, rule2);

        List<RuleConditionDO> conditions = Arrays.asList(
                buildRuleConditionDO("1", "1", "header", "=", "test", "value1"),
                buildRuleConditionDO("2", "2", "header", "=", "test", "value2")
        );

        when(ruleConditionMapper.selectByRuleIdSet(anySet())).thenReturn(conditions);
        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onDeleted(rules);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedWithBatchSelectorDeletedEvent() {
        RuleDO rule1 = buildRuleDO("1", "selector1", "rule1");
        RuleDO rule2 = buildRuleDO("2", "selector2", "rule2");
        List<RuleDO> rules = Arrays.asList(rule1, rule2);

        PluginDO plugin = buildPluginDO("plugin1", TEST_PLUGIN_NAME);
        BatchSelectorDeletedEvent selectorEvent = new BatchSelectorDeletedEvent(
                Collections.emptyList(), TEST_OPERATOR, Collections.singletonList(plugin)
        );

        List<RuleConditionDO> conditions = Arrays.asList(
                buildRuleConditionDO("1", "1", "header", "=", "test", "value1")
        );

        when(ruleConditionMapper.selectByRuleIdSet(anySet())).thenReturn(conditions);

        ruleEventPublisher.onDeleted(rules, selectorEvent);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<RuleDO> emptyRules = Collections.emptyList();

        when(ruleConditionMapper.selectByRuleIdSet(anySet())).thenReturn(Collections.emptyList());

        ruleEventPublisher.onDeleted(emptyRules);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnRegister() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");
        List<RuleConditionDTO> conditions = Arrays.asList(
                buildRuleConditionDTO("1", "1", "header", "=", "test", "value1")
        );

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onRegister(rule, conditions);

        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        DataChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(ConfigGroupEnum.RULE, event.getGroupKey());
    }

    @Test
    void testPublish() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");
        RuleCreatedEvent event = new RuleCreatedEvent(rule, TEST_OPERATOR);

        ruleEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    @Test
    void testOnCreatedWithEmptyCondition() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");
        List<RuleConditionDTO> emptyConditions = Collections.emptyList();

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onCreated(rule, emptyConditions);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnUpdatedWithEmptyConditions() {
        RuleDO before = buildRuleDO("1", "selector1", "old-rule");
        RuleDO after = buildRuleDO("1", "selector1", "new-rule");
        List<RuleConditionDTO> emptyConditions = Collections.emptyList();
        List<RuleConditionDO> emptyBeforeConditions = Collections.emptyList();

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onUpdated(after, before, emptyConditions, emptyBeforeConditions);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedWithBatchSelectorDeletedEventNullPlugin() {
        RuleDO rule1 = buildRuleDO("1", "selector1", "rule1");
        RuleDO rule2 = buildRuleDO("2", "selector2", "rule2");
        List<RuleDO> rules = Arrays.asList(rule1, rule2);

        BatchSelectorDeletedEvent selectorEvent = new BatchSelectorDeletedEvent(
                Collections.emptyList(), TEST_OPERATOR, Collections.emptyList()
        );

        List<RuleConditionDO> conditions = Arrays.asList(
                buildRuleConditionDO("1", "1", "header", "=", "test", "value1")
        );

        when(ruleConditionMapper.selectByRuleIdSet(anySet())).thenReturn(conditions);

        ruleEventPublisher.onDeleted(rules, selectorEvent);

        ArgumentCaptor<DataChangedEvent> dataChangedCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher).publishEvent(dataChangedCaptor.capture());

        DataChangedEvent event = dataChangedCaptor.getValue();
        assertNotNull(event);
        assertEquals(ConfigGroupEnum.RULE, event.getGroupKey());
    }

    @Test
    void testOnDeletedCollectionWithNoConditions() {
        RuleDO rule1 = buildRuleDO("1", "selector1", "rule1");
        List<RuleDO> rules = Collections.singletonList(rule1);

        when(ruleConditionMapper.selectByRuleIdSet(anySet())).thenReturn(Collections.emptyList());
        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onDeleted(rules);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnRegisterWithEmptyConditions() {
        RuleDO rule = buildRuleDO("1", "selector1", "test-rule");
        List<RuleConditionDTO> emptyConditions = Collections.emptyList();

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onRegister(rule, emptyConditions);

        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        DataChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(ConfigGroupEnum.RULE, event.getGroupKey());
    }

    @Test
    void testOnDeletedCollectionWithMultipleConditionsPerRule() {
        RuleDO rule1 = buildRuleDO("1", "selector1", "rule1");
        List<RuleDO> rules = Collections.singletonList(rule1);

        List<RuleConditionDO> conditions = Arrays.asList(
                buildRuleConditionDO("1", "1", "header", "=", "test", "value1"),
                buildRuleConditionDO("2", "1", "query", "match", "param", "value2"),
                buildRuleConditionDO("3", "1", "uri", "=", "path", "/api")
        );

        when(ruleConditionMapper.selectByRuleIdSet(anySet())).thenReturn(conditions);
        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onDeleted(rules);

        ArgumentCaptor<DataChangedEvent> dataChangedCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher).publishEvent(dataChangedCaptor.capture());

        DataChangedEvent event = dataChangedCaptor.getValue();
        assertNotNull(event);
        assertEquals(ConfigGroupEnum.RULE, event.getGroupKey());
    }

    @Test
    void testOnUpdatedWithConditionAndEmptyBeforeCondition() {
        RuleDO before = buildRuleDO("1", "selector1", "old-rule");
        RuleDO after = buildRuleDO("1", "selector1", "new-rule");
        List<RuleConditionDTO> conditions = Arrays.asList(
                buildRuleConditionDTO("1", "1", "header", "=", "test", "value1")
        );

        when(ruleMapper.getPluginNameBySelectorId(anyString())).thenReturn(TEST_PLUGIN_NAME);

        ruleEventPublisher.onUpdated(after, before, conditions, Collections.emptyList());

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    private static RuleDO buildRuleDO(final String id, final String selectorId, final String ruleName) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        RuleDO ruleDO = RuleDO.builder()
                .id(id)
                .selectorId(selectorId)
                .ruleName(ruleName)
                .matchMode(0)
                .enabled(true)
                .loged(false)
                .sortCode(1)
                .handle("{}")
                .matchRestful(false)
                .build();
        ruleDO.setDateCreated(currentTime);
        ruleDO.setDateUpdated(currentTime);
        return ruleDO;
    }

    private static RuleConditionDTO buildRuleConditionDTO(final String id, final String ruleId,
                                                          final String paramType, final String operator,
                                                          final String paramName, final String paramValue) {
        return new RuleConditionDTO(id, ruleId, paramType, operator, paramName, paramValue);
    }

    private static RuleConditionDO buildRuleConditionDO(final String id, final String ruleId,
                                                        final String paramType, final String operator,
                                                        final String paramName, final String paramValue) {
        RuleConditionDO conditionDO = new RuleConditionDO(ruleId, paramType, operator, paramName, paramValue);
        conditionDO.setId(id);
        return conditionDO;
    }

    private static PluginDO buildPluginDO(final String id, final String name) {
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId(id);
        pluginDO.setName(name);
        return pluginDO;
    }
}
