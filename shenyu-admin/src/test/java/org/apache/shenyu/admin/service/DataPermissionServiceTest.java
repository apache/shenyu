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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.DataPermissionPageVO;
import org.apache.shenyu.admin.service.impl.DataPermissionServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for DataPermissionService.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public final class DataPermissionServiceTest {

    @InjectMocks
    private DataPermissionServiceImpl dataPermissionService;

    @Mock
    private DataPermissionMapper dataPermissionMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private SelectorMapper selectorMapper;

    private RuleDTO ruleDTO;

    private DataPermissionDTO dataPermissionDTO;

    private SelectorDTO selectorDTO;

    @Before
    public void setup() {
        ruleDTO = new RuleDTO("1", "1", 1, "test", true, true, 1, "test", null);
        dataPermissionDTO = new DataPermissionDTO();
        dataPermissionDTO.setId("1");
        dataPermissionDTO.setUserId("1001");
        dataPermissionDTO.setDataId("1");
        dataPermissionDTO.setIsSelected(true);
        selectorDTO = new SelectorDTO("1", "1", "test", 1, 1, 1, true, false, true, "test", null);
    }

    @Test
    public void assertGetUserDataPermissionList() {
        List<DataPermissionDO> list = Collections.singletonList(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        given(dataPermissionMapper.listByUserId("1001")).willReturn(list);
        assertThat(dataPermissionService.getUserDataPermissionList("1001"), is(list));
    }

    @Test
    public void assertGetDataPermission() {
        List<DataPermissionDO> list = Collections.singletonList(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        given(dataPermissionMapper.listByUserId("1001")).willReturn(list);
        List<String> target = Collections.singletonList("1");
        assertThat(dataPermissionService.getDataPermission("1001"), is(target));
    }

    @Test
    public void assertCreateSelector() {
        List<RuleDO> list = new LinkedList<>();
        list.add(RuleDO.buildRuleDO(ruleDTO));
        given(ruleMapper.findBySelectorId("1")).willReturn(list);
        assertThat(dataPermissionService.createSelector(dataPermissionDTO), is(2));
    }

    @Test
    public void assertDeleteSelector() {
        List<RuleDO> list = new LinkedList<>();
        list.add(RuleDO.buildRuleDO(ruleDTO));
        given(ruleMapper.findBySelectorId("1")).willReturn(list);
        given(dataPermissionMapper.deleteByDataIdsAndUserId(Collections.singletonList("1"), "1001", 1)).willReturn(1);
        given(dataPermissionMapper.deleteByUniqueKey("1", "1001", 0)).willReturn(1);
        assertThat(dataPermissionService.deleteSelector(dataPermissionDTO), is(2));
    }

    @Test
    public void assertListSelectorsByPage() {
        SelectorQuery selectorQuery = new SelectorQuery("1", null, new PageParameter(1, 10));
        given(selectorMapper.countByQuery(selectorQuery)).willReturn(100);
        given(selectorMapper.selectByQuery(selectorQuery)).willReturn(Collections.singletonList(SelectorDO.buildSelectorDO(selectorDTO)));
        given(dataPermissionMapper.selectDataIds(Collections.singletonList("1"), "1001", 0)).willReturn(Collections.singletonList("1"));
        assertThat(dataPermissionService.listSelectorsByPage(selectorQuery, "1001"),
                is(PageResultUtils.result(selectorQuery.getPageParameter(), () -> 100, () -> Collections.singletonList(new DataPermissionPageVO("1", "test", true)))));
    }

    @Test
    public void assertListRulesByPage() {
        RuleQuery ruleQuery = new RuleQuery("1", null, new PageParameter(1, 10));
        given(ruleMapper.countByQuery(ruleQuery)).willReturn(100);
        given(ruleMapper.selectByQuery(ruleQuery)).willReturn(Collections.singletonList(RuleDO.buildRuleDO(ruleDTO)));
        given(dataPermissionMapper.selectDataIds(Collections.singletonList("1"), "1001", 1)).willReturn(Collections.singletonList("1"));
        assertThat(dataPermissionService.listRulesByPage(ruleQuery, "1001"),
                is(PageResultUtils.result(ruleQuery.getPageParameter(), () -> 100, () -> Collections.singletonList(new DataPermissionPageVO("1", "test", true)))));
    }

    @Test
    public void assertCreateRule() {
        given(ruleMapper.selectById("1")).willReturn(RuleDO.buildRuleDO(ruleDTO));
        given(dataPermissionMapper.insertSelective(any(DataPermissionDO.class))).willReturn(1);
        assertThat(dataPermissionService.createRule(dataPermissionDTO), is(2));
    }

    @Test
    public void assertDeleteRule() {
        given(dataPermissionMapper.deleteByUniqueKey("1", "1001", 1)).willReturn(1);
        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
        dataPermissionDTO.setDataId("1");
        dataPermissionDTO.setUserId("1001");
        assertThat(dataPermissionService.deleteRule(dataPermissionDTO), is(1));
    }
}
