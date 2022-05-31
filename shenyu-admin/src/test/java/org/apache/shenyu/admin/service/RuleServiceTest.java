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

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.impl.RuleServiceImpl;
import org.apache.shenyu.admin.service.publish.RuleEventPublisher;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.mockito.stubbing.Answer;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test cases for RuleService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class RuleServiceTest {

    @InjectMocks
    private RuleServiceImpl ruleService;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private RuleConditionMapper ruleConditionMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;
    
    @Mock
    private DataPermissionMapper dataPermissionMapper;
    
    @Mock
    private RuleEventPublisher ruleEventPublisher;

    @BeforeEach
    public void setUp() {
        when(dataPermissionMapper.listByUserId("1")).thenReturn(Collections.singletonList(DataPermissionDO.buildPermissionDO(new DataPermissionDTO())));
        ruleService = new RuleServiceImpl(ruleMapper, ruleConditionMapper, selectorMapper, pluginMapper, ruleEventPublisher);
    }

    @Test
    public void testRegister() {
        publishEvent();
        testRegisterCreate();
        testRegisterUpdate();
    }

    @Test
    public void testCreateOrUpdate() {
        publishEvent();
        testUpdate();
        testCreate();
    }

    @Test
    public void testDelete() {
        publishEvent();
        RuleDO ruleDO = buildRuleDO("123");
        given(this.ruleMapper.selectById("123")).willReturn(ruleDO);
        final List<String> ids = Collections.singletonList(ruleDO.getId());
        given(this.ruleMapper.deleteByIds(ids)).willReturn(ids.size());
        assertEquals(this.ruleService.delete(ids), ids.size());
    }

    @Test
    public void testFindById() {
        RuleDO ruleDO = buildRuleDO("123");
        given(this.ruleMapper.selectById("123")).willReturn(ruleDO);
        RuleConditionQuery ruleConditionQuery = buildRuleConditionQuery();
        RuleConditionDO ruleCondition = buildRuleConditionDO();
        given(this.ruleConditionMapper.selectByQuery(ruleConditionQuery)).willReturn(Collections.singletonList(ruleCondition));
        RuleVO ruleVO = buildRuleVO();
        final RuleVO ruleVOById = this.ruleService.findById("123");
        assertNotNull(ruleVOById);
        assertEquals(ruleVOById.getId(), ruleVO.getId());
    }

    @Test
    public void testListByPage() {
        PageParameter parameter = new PageParameter();
        parameter.setPageSize(5);
        parameter.setTotalCount(10);
        parameter.setTotalPage(parameter.getTotalCount() / parameter.getPageSize());
        RuleQuery ruleQuery = new RuleQuery("456", null, parameter);
        List<RuleDO> ruleDOList = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            RuleDO ruleDO = buildRuleDO(String.valueOf(i));
            ruleDOList.add(ruleDO);
        }
        given(this.ruleMapper.selectByQuery(ruleQuery)).willReturn(ruleDOList);
        final CommonPager<RuleVO> ruleVOCommonPager = this.ruleService.listByPage(ruleQuery);
        assertEquals(ruleVOCommonPager.getDataList().size(), ruleDOList.size());
    }

    @Test
    public void testListAll() {
        publishEvent();
        checkListAll(null);
    }

    @Test
    public void testListAllWithSelectorNull() {
        mockFindSelectorIsNull();
        checkListAll(0);
    }

    private void mockFindSelectorIsNull() {
        given(this.selectorMapper.selectById("456")).willReturn(null);
        given(this.pluginMapper.selectById("789")).willReturn(buildPluginDO());
    }

    @Test
    public void testListAllWithPluginNull() {
        mockFindPluginIsNull();
        checkListAll(0);
    }

    private void mockFindPluginIsNull() {
        given(this.selectorMapper.selectById("456")).willReturn(buildSelectorDO());
        given(this.pluginMapper.selectById("789")).willReturn(null);
    }

    private void checkListAll(final Integer expected) {
        RuleConditionQuery ruleConditionQuery = buildRuleConditionQuery();
        RuleConditionDO ruleCondition = buildRuleConditionDO();
        given(this.ruleConditionMapper.selectByQuery(ruleConditionQuery)).willReturn(Collections.singletonList(ruleCondition));
        RuleDO ruleDO = buildRuleDO("123");
        List<RuleDO> ruleDOList = Collections.singletonList(ruleDO);
        given(this.ruleMapper.selectAll()).willReturn(ruleDOList);
        List<RuleData> dataList = this.ruleService.listAll();
        assertNotNull(dataList);
        assertEquals(Optional.ofNullable(expected).orElse(ruleDOList.size()), dataList.size());
    }

    @Test
    public void testFindBySelectorId() {
        publishEvent();
        RuleConditionQuery ruleConditionQuery = buildRuleConditionQuery();
        RuleConditionDO ruleCondition = buildRuleConditionDO();
        given(this.ruleConditionMapper.selectByQuery(ruleConditionQuery)).willReturn(Collections.singletonList(ruleCondition));
        RuleDO ruleDO = buildRuleDO("123");
        List<RuleDO> ruleDOList = Collections.singletonList(ruleDO);
        given(this.ruleMapper.findBySelectorId("456")).willReturn(ruleDOList);
        List<RuleData> dataList = this.ruleService.findBySelectorId("456");
        assertNotNull(dataList);
        assertEquals(ruleDOList.size(), dataList.size());
    }

    @Test
    public void testFindBySelectorIdList() {
        publishEvent();
        RuleConditionQuery ruleConditionQuery = buildRuleConditionQuery();
        RuleConditionDO ruleCondition = buildRuleConditionDO();
        given(this.ruleConditionMapper.selectByQuery(ruleConditionQuery)).willReturn(Collections.singletonList(ruleCondition));
        RuleDO ruleDO = buildRuleDO("123");
        List<RuleDO> ruleDOList = Collections.singletonList(ruleDO);
        given(this.ruleMapper.findBySelectorIds(Collections.singletonList("456"))).willReturn(ruleDOList);
        List<RuleData> dataList = this.ruleService.findBySelectorIdList(Collections.singletonList("456"));
        assertNotNull(dataList);
        assertEquals(ruleDOList.size(), dataList.size());
    }

    private void publishEvent() {
        PluginDO pluginDO = buildPluginDO();
        SelectorDO selectorDO = buildSelectorDO();
        given(this.selectorMapper.selectById("456")).willReturn(selectorDO);
        given(this.pluginMapper.selectById("789")).willReturn(pluginDO);
        given(this.selectorMapper.selectByIdSet(Sets.newHashSet("456"))).willReturn(Collections.singletonList(selectorDO));
        given(this.pluginMapper.selectByIds(Lists.newArrayList("789"))).willReturn(Collections.singletonList(pluginDO));
        given(this.ruleConditionMapper.selectByRuleIdSet(Sets.newHashSet("123"))).willReturn(Collections.singletonList(buildRuleConditionDO()));
    }

    private void testRegisterCreate() {
        RuleDTO ruleDTO = buildRuleDTO("");
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        String ruleId = this.ruleService.registerDefault(ruleDTO);
        assertNotNull(ruleId);
        assertEquals(ruleId.length(), ruleDO.getId().length());
    }

    private void testRegisterUpdate() {
        RuleDTO ruleDTO = buildRuleDTO("123");
        String ruleId = this.ruleService.registerDefault(ruleDTO);
        assertNotNull(ruleId);
        assertEquals(ruleId, ruleDTO.getId());
    }

    private void testCreate() {
        try (MockedStatic<JwtUtils> mocked = mockStatic(JwtUtils.class)) {
            mocked.when(JwtUtils::getUserInfo)
                    .thenAnswer((Answer<UserInfo>) invocation -> UserInfo.builder().userId("1").userName("admin").build());
            RuleDTO ruleDTO = buildRuleDTO("");
            given(this.ruleMapper.insertSelective(any())).willReturn(1);
            assertThat(this.ruleService.createOrUpdate(ruleDTO), greaterThan(0));
        }
    }

    private void testUpdate() {
        RuleDTO ruleDTO = buildRuleDTO("123");
        given(this.ruleMapper.selectById("123")).willReturn(RuleDO.builder().id("123").build());
        given(this.ruleMapper.updateSelective(any())).willReturn(1);
        assertThat(this.ruleService.createOrUpdate(ruleDTO), greaterThan(0));
    }

    private RuleDO buildRuleDO(final String id) {
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId("456")
                .matchMode(0)
                .handle("{\"test1\":\"\"}")
                .build();
        if (StringUtils.isNotBlank(id)) {
            ruleDTO.setId(id);
        }
        RuleConditionDTO ruleConditionDTO1 = RuleConditionDTO.builder().id("111").build();
        RuleConditionDTO ruleConditionDTO2 = RuleConditionDTO.builder().id("222").build();
        ruleDTO.setRuleConditions(Arrays.asList(ruleConditionDTO1, ruleConditionDTO2));
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        ruleDO.setDateCreated(now);
        ruleDO.setDateUpdated(now);
        return ruleDO;
    }

    private RuleDTO buildRuleDTO(final String id) {
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId("456")
                .matchMode(0)
                .handle("{\"test1\":\"\"}")
                .build();
        if (StringUtils.isNotBlank(id)) {
            ruleDTO.setId(id);
        }
        RuleConditionDTO ruleConditionDTO1 = RuleConditionDTO.builder().id("111").build();
        RuleConditionDTO ruleConditionDTO2 = RuleConditionDTO.builder().id("222").build();
        RuleConditionDTO ruleConditionDTO3 = RuleConditionDTO.builder().ruleId("333").build();
        ruleDTO.setRuleConditions(Arrays.asList(ruleConditionDTO1, ruleConditionDTO2, ruleConditionDTO3));
        return ruleDTO;
    }

    private RuleVO buildRuleVO() {
        RuleVO ruleVO = new RuleVO();
        ruleVO.setId("123");
        return ruleVO;
    }

    private PluginDO buildPluginDO() {
        return PluginDO.builder()
                .name("test")
                .id("789")
                .build();
    }

    private SelectorDO buildSelectorDO() {
        return SelectorDO.builder()
                .pluginId("789")
                .id("456")
                .build();
    }

    private RuleConditionDO buildRuleConditionDO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        return RuleConditionDO.builder()
                .ruleId("123")
                .paramType("post")
                .operator("match")
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }

    private RuleConditionQuery buildRuleConditionQuery() {
        RuleConditionQuery ruleConditionQuery = new RuleConditionQuery();
        ruleConditionQuery.setRuleId("123");
        return ruleConditionQuery;
    }
}
