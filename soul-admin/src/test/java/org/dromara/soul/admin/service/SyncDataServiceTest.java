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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.sync.SyncDataServiceImpl;
import org.dromara.soul.admin.vo.PluginVO;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.OperatorEnum;
import org.dromara.soul.common.enums.ParamTypeEnum;
import org.dromara.soul.common.utils.DateUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationEventPublisher;

import java.time.LocalDateTime;
import java.util.Collections;

import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertThat;
import static org.mockito.BDDMockito.given;

/**
 * test for SyncDataService.
 *
 * @author Redick
 */
@RunWith(MockitoJUnitRunner.class)
public final class SyncDataServiceTest {

    private static final String ID = "1";

    @InjectMocks
    private SyncDataServiceImpl syncDataService;

    @Mock
    private AppAuthService appAuthService;

    /**
     * The Plugin service.
     */
    @Mock
    private PluginService pluginService;

    /**
     * The Selector service.
     */
    @Mock
    private SelectorService selectorService;

    /**
     * The Rule service.
     */
    @Mock
    private RuleService ruleService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private MetaDataService metaDataService;

    @Test
    public void syncAllTest() {
        PluginData pluginData = buildPluginData();
        SelectorData selectorData = buildSelectorData();
        RuleData ruleData = buildRuleData();
        given(this.appAuthService.syncData()).willReturn(SoulAdminResult.success());
        given(this.pluginService.listAll()).willReturn(Collections.singletonList(pluginData));
        given(this.selectorService.listAll()).willReturn(Collections.singletonList(selectorData));
        given(this.ruleService.listAll()).willReturn(Collections.singletonList(ruleData));
        assertThat(syncDataService.syncAll(DataEventTypeEnum.CREATE), greaterThan(false));
    }

    @Test
    public void syncPluginDataTest() {
        PluginVO pluginVO = buildPluginVO();
        given(this.pluginService.findById(pluginVO.getId())).willReturn(pluginVO);
        SelectorData selectorData = buildSelectorData();
        given(this.selectorService.findByPluginId(pluginVO.getId())).willReturn(Collections.singletonList(selectorData));
        RuleData ruleData = buildRuleData();
        given(this.ruleService.findBySelectorId(pluginVO.getId())).willReturn(Collections.singletonList(ruleData));

        assertThat(syncDataService.syncPluginData(pluginVO.getId()), greaterThan(false));
    }


    /**
     * build mock PluginData.
     *
     * @return PluginData
     */
    private PluginData buildPluginData() {
        PluginData pluginData = new PluginData();
        pluginData.setId(ID);
        pluginData.setName("plugin_test");
        pluginData.setConfig("config_test");
        pluginData.setEnabled(true);
        pluginData.setRole(1);
        return pluginData;
    }

    /**
     * build mock SelectorData.
     *
     * @return SelectorData
     */
    private SelectorData buildSelectorData() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(ID);
        selectorData.setContinued(true);
        selectorData.setEnabled(true);
        selectorData.setHandle("divide");
        selectorData.setLoged(true);
        selectorData.setMatchMode(1);
        selectorData.setPluginId("5");
        selectorData.setName("divide");
        selectorData.setPluginName("divide");
        selectorData.setSort(1);
        selectorData.setType(1);
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.POST.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamName("post");
        conditionData.setParamValue("POST");
        selectorData.setConditionList(Collections.singletonList(conditionData));
        return selectorData;
    }

    /**
     * build mock RuleData.
     *
     * @return RuleData
     */
    private RuleData buildRuleData() {
        RuleData ruleData = new RuleData();
        ruleData.setId(ID);
        ruleData.setEnabled(true);
        ruleData.setHandle("divide");
        ruleData.setLoged(true);
        ruleData.setMatchMode(1);
        ruleData.setName("divide");
        ruleData.setPluginName("divide");
        ruleData.setSelectorId("1");
        ruleData.setSort(1);
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.POST.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamName("post");
        conditionData.setParamValue("POST");
        ruleData.setConditionDataList(Collections.singletonList(conditionData));
        return ruleData;
    }

    /**
     * build mock PluginVO.
     *
     * @return PluginVO
     */
    private PluginVO buildPluginVO() {
        String dateTime = DateUtils.localDateTimeToString(LocalDateTime.now());
        return new PluginVO(
                ID,
                5,
                1,
                "divide",
                null,
                true,
                dateTime,
                dateTime
        );
    }
}
