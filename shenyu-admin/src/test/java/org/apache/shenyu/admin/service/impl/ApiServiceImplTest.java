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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ApiServiceImpl} registration cleanup.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ApiServiceImplTest {

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private ApiMapper apiMapper;

    @Mock
    private TagRelationMapper tagRelationMapper;

    @Mock
    private TagMapper tagMapper;

    private ApiServiceImpl apiServiceImpl;

    @BeforeEach
    public void setUp() {
        this.apiServiceImpl = new ApiServiceImpl(selectorService, ruleService, metaDataService,
                apiMapper, tagRelationMapper, tagMapper);
    }

    @Test
    public void deleteRemovesOnlyTheRuleRegisteredForTheExactApiPath() {
        final ApiDO apiDO = ApiDO.builder()
                .id("api-1")
                .apiPath("/order/get")
                .contextPath("/order")
                .build();
        when(apiMapper.selectByIds(anyList())).thenReturn(Collections.singletonList(apiDO));
        when(apiMapper.deleteByIds(anyList())).thenReturn(1);
        when(tagRelationMapper.deleteByApiIds(anyList())).thenReturn(1);

        // the rule search matches by substring, so it also returns the rule of the
        // longer sibling path /order/get/detail — that rule belongs to another API
        final RuleVO exactRule = new RuleVO();
        exactRule.setId("rule-exact");
        exactRule.setName("/order/get");
        final RuleVO longerRule = new RuleVO();
        longerRule.setId("rule-longer");
        longerRule.setName("/order/get/detail");
        when(ruleService.searchByCondition(any(RuleQueryCondition.class)))
                .thenReturn(Arrays.asList(exactRule, longerRule));

        when(selectorService.findByNameAndPluginNamesAndNamespaceId(eq("/order"), anyList(), anyString()))
                .thenReturn(Collections.emptyList());
        when(metaDataService.findByPathAndNamespaceId(anyString(), anyString())).thenReturn(null);

        apiServiceImpl.delete(Collections.singletonList("api-1"));

        final ArgumentCaptor<List<String>> ruleIdsCaptor = ArgumentCaptor.forClass(List.class);
        verify(ruleService).deleteByIdsAndNamespaceId(ruleIdsCaptor.capture(), anyString());
        assertEquals(Collections.singletonList("rule-exact"), ruleIdsCaptor.getValue(),
                "deleting one API must not delete rules registered for longer paths that merely contain it");
    }
}
