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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.DataPermissionMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.SelectorConditionVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.admin.service.publish.SelectorEventPublisher;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
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
import org.springframework.context.ApplicationEventPublisher;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test cases for SelectorService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SelectorServiceTest {
    
    @InjectMocks
    private SelectorServiceImpl selectorService;
    
    @Mock
    private SelectorMapper selectorMapper;
    
    @Mock
    private SelectorConditionMapper selectorConditionMapper;
    
    @Mock
    private PluginMapper pluginMapper;
    
    @Mock
    private RuleMapper ruleMapper;
    
    @Mock
    private DataPermissionMapper dataPermissionMapper;
    
    @Mock
    private ApplicationEventPublisher eventPublisher;
    
    @Mock
    private SelectorEventPublisher selectorEventPublisher;
    
    @BeforeEach
    public void setUp() {
        when(dataPermissionMapper.listByUserId("1")).thenReturn(Collections.singletonList(DataPermissionDO.buildPermissionDO(new DataPermissionDTO())));
        selectorService = new SelectorServiceImpl(selectorMapper, selectorConditionMapper, pluginMapper, eventPublisher, selectorEventPublisher);
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
        final String correctId = "456";
        
        // mock basic objects for delete.
        SelectorDO mockedSelectorDO = buildSelectorDO();
        PluginDO mockedPluginDO = buildPluginDO();
        given(pluginMapper.selectByIds(Collections.singletonList(mockedSelectorDO.getPluginId()))).willReturn(Collections.singletonList(mockedPluginDO));
        given(selectorMapper.selectByIdSet(Stream.of(correctId).collect(Collectors.toSet()))).willReturn(Collections.singletonList(mockedSelectorDO));
        
        // mock for test if divide selector delete.
//        when(mockedPluginDO.getName()).thenReturn(PluginEnum.DIVIDE.getName());
//        when(mockedSelectorDO.getName()).thenReturn("anyString");
        
        // mock objects for test delete rule and ruleCondition.
        List<RuleDO> mockedRuleDOList = mock(List.class);
        given(ruleMapper.findBySelectorIds(Collections.singletonList(correctId))).willReturn(mockedRuleDOList);
        
        // mock for test for-each statement.
//        RuleDO mockedRuleDo = mock(RuleDO.class);
//        when(ruleMapper.deleteByIds(Collections.singletonList(mockedRuleDo.getId()))).thenReturn(1);
//        when(ruleConditionMapper.deleteByRuleIds(Collections.singletonList(mockedRuleDo.getId()))).thenReturn(1);
        
        final List<String> ids = Collections.singletonList(correctId);
        given(selectorMapper.deleteByIds(ids)).willReturn(ids.size());
        assertEquals(selectorService.delete(ids), ids.size());
    }
    
    @Test
    public void testFindById() {
        SelectorDO selectorDO = buildSelectorDO();
        given(this.selectorMapper.selectById(eq("123"))).willReturn(selectorDO);
        SelectorVO selectorVO = this.selectorService.findById("123");
        assertNotNull(selectorDO);
        assertEquals(selectorVO.getId(), selectorDO.getId());
        
        List<SelectorConditionVO> selectorConditions = selectorVO.getSelectorConditions();
        selectorConditions.forEach(selectorConditionVO -> assertEquals(selectorConditionVO.getSelectorId(), selectorDO.getId()));
    }
    
    @Test
    public void testFindByName() {
        SelectorDO selectorDO1 = buildSelectorDO();
        given(this.selectorMapper.selectByName(eq("kuan"))).willReturn(selectorDO1);
        SelectorDO selectorDO2 = this.selectorService.findByName("kuan");
        assertNotNull(selectorDO2);
        assertEquals(selectorDO1.getId(), selectorDO2.getId());
    }
    
    @Test
    public void testListByPage() {
        final List<SelectorDO> selectorDOs = buildSelectorDOList();
        given(this.selectorMapper.selectByQuery(any())).willReturn(selectorDOs);
        SelectorQuery params = buildSelectorQuery();
        final CommonPager<SelectorVO> result = this.selectorService.listByPageWithPermission(params);
        assertThat(result, notNullValue());
        assertEquals(selectorDOs.size(), result.getDataList().size());
    }
    
    @Test
    public void testFindByPluginId() {
        
        List<SelectorData> res = this.selectorService.findByPluginId("789");
        res.forEach(selectorData -> assertEquals("789", selectorData.getPluginId()));
    }
    
    @Test
    public void testListAll() {
        final List<SelectorDO> selectorDOs = buildSelectorDOList();
        given(this.selectorMapper.selectAll()).willReturn(selectorDOs);
        given(this.pluginMapper.selectByIds(any())).willReturn(Collections.singletonList(buildPluginDO()));
        List<SelectorData> dataList = this.selectorService.listAll();
        assertNotNull(dataList);
        assertEquals(selectorDOs.size(), dataList.size());
    }
    
    @Test
    public void testHandlerSelectorNeedUpstreamCheck() {
        publishEvent();
        
        // Test the situation where the selector cannot be found based on the contextPath.
        given(pluginMapper.selectByName("test")).willReturn(buildPluginDO());
        assertNotNull(selectorService.registerDefault(buildMetaDataRegisterDTO(), "test", ""));
    }
    
    private void testUpdate() {
        SelectorDTO selectorDTO = buildSelectorDTO("456");
        selectorDTO.setPluginId("789");
        given(this.selectorMapper.updateSelective(any())).willReturn(1);
        assertThat(this.selectorService.createOrUpdate(selectorDTO), greaterThan(0));
    }
    
    private void testCreate() {
        try (MockedStatic<JwtUtils> mocked = mockStatic(JwtUtils.class)) {
            mocked.when(JwtUtils::getUserInfo)
                    .thenAnswer((Answer<UserInfo>) invocation -> UserInfo.builder().userId("1").userName("admin").build());
            SelectorDTO selectorDTO = buildSelectorDTO("");
            given(this.selectorMapper.insertSelective(any())).willReturn(1);
            // Because of the lack of shiro configuration, there would be a NullPointerException here.
            assertThat(this.selectorService.createOrUpdate(selectorDTO), greaterThan(0));
        }
    }
    
    private void testRegisterCreate() {
        SelectorDTO selectorDTO = buildSelectorDTO("");
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        String selectorId = this.selectorService.registerDefault(selectorDTO);
        assertNotNull(selectorId);
        assertEquals(selectorId.length(), selectorDO.getId().length());
    }
    
    private void testRegisterUpdate() {
        SelectorDTO selectorDTO = buildSelectorDTO("456");
        String selectorId = this.selectorService.registerDefault(selectorDTO);
        assertNotNull(selectorId);
        assertEquals(selectorId, selectorDTO.getId());
    }
    
    private void publishEvent() {
        PluginDO pluginDO = buildPluginDO();
        given(this.pluginMapper.selectById(anyString())).willReturn(pluginDO);
    }
    
    private PluginDO buildPluginDO() {
        PluginDO pluginDO = new PluginDO();
        pluginDO.setName("test");
        pluginDO.setId("789");
        return pluginDO;
    }
    
    private SelectorDO buildSelectorDO() {
        SelectorDTO selectorDTO = new SelectorDTO();
        selectorDTO.setId("456");
        selectorDTO.setPluginId("789");
        selectorDTO.setName("kuan");
        selectorDTO.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorDTO.setHandle("[{\"upstreamHost\": \"127.0.0.1\", \"protocol\": \"http://\", \"upstreamUrl\": \"anotherUrl\"}]");
        SelectorConditionDTO selectorConditionDTO1 = new SelectorConditionDTO();
        selectorConditionDTO1.setId("111");
        selectorConditionDTO1.setSelectorId("456");
        SelectorConditionDTO selectorConditionDTO2 = new SelectorConditionDTO();
        selectorConditionDTO2.setId("222");
        selectorConditionDTO2.setSelectorId("456");
        selectorDTO.setSelectorConditions(Arrays.asList(selectorConditionDTO1, selectorConditionDTO2));
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        selectorDO.setDateCreated(now);
        selectorDO.setDateUpdated(now);
        return selectorDO;
    }
    
    private SelectorDTO buildSelectorDTO(final String id) {
        SelectorDTO selectorDTO = new SelectorDTO();
        if (StringUtils.isNotBlank(id)) {
            selectorDTO.setId(id);
        }
        selectorDTO.setName("test-name-" + new Random().nextInt());
        selectorDTO.setEnabled(true);
        selectorDTO.setHandle("[{\"upstreamHost\": \"127.0.0.1\", \"protocol\": \"http://\", \"upstreamUrl\": \"anotherUrl\"}]");
        selectorDTO.setPluginId("789");
        selectorDTO.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorDTO.setLoged(true);
        selectorDTO.setMatchMode(1);
        selectorDTO.setContinued(true);
        selectorDTO.setSort(1);
        selectorDTO.setLoged(true);
        SelectorConditionDTO selectorConditionDTO1 = new SelectorConditionDTO();
        selectorConditionDTO1.setId("111");
        SelectorConditionDTO selectorConditionDTO2 = new SelectorConditionDTO();
        selectorConditionDTO2.setId("222");
        SelectorConditionDTO selectorConditionDTO3 = new SelectorConditionDTO();
        selectorConditionDTO3.setId("");
        selectorDTO.setSelectorConditions(Arrays.asList(selectorConditionDTO1, selectorConditionDTO2, selectorConditionDTO3));
        return selectorDTO;
    }
    
    private List<SelectorDO> buildSelectorDOList() {
        return Collections.singletonList(buildSelectorDO());
    }
    
    private SelectorQuery buildSelectorQuery() {
        SelectorQuery selectorQuery = new SelectorQuery();
        selectorQuery.setPluginId("789");
        selectorQuery.setPageParameter(new PageParameter());
        return selectorQuery;
    }
    
    private MetaDataRegisterDTO buildMetaDataRegisterDTO() {
        MetaDataRegisterDTO metaDataRegisterDTO = new MetaDataRegisterDTO();
        metaDataRegisterDTO.setAppName("test");
        metaDataRegisterDTO.setPath("/test");
        metaDataRegisterDTO.setHost("127.0.0.1");
        metaDataRegisterDTO.setPort(13307);
        metaDataRegisterDTO.setRpcType("test");
        return metaDataRegisterDTO;
    }
}
