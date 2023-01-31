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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.impl.PluginServiceImpl;
import org.apache.shenyu.admin.service.publish.PluginEventPublisher;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * Test cases for PluginService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class PluginServiceTest {
    
    @InjectMocks
    private PluginServiceImpl pluginService;
    
    @Mock
    private PluginMapper pluginMapper;
    
    @Mock
    private SelectorMapper selectorMapper;
    
    @Mock
    private PluginEventPublisher modelDataEventPublisher;
    
    @BeforeEach
    public void setUp() {
        pluginService = new PluginServiceImpl(pluginMapper, modelDataEventPublisher);
    }
    
    @Test
    public void testCreateOrUpdate() {
        publishEvent();
        testCreate();
        testUpdate();
    }
    
    @Test
    public void testDelete() {
        List<PluginDO> plugins = Collections.singletonList(buildPluginDO("123"));
        when(pluginMapper.selectByIds(Collections.singletonList("123"))).thenReturn(plugins);
        when(pluginMapper.deleteByIds(Collections.singletonList("123"))).thenReturn(1);
        
        final List<SelectorDO> selectorDOList = new ArrayList<>();
        selectorDOList.add(SelectorDO.builder().id("101").build());
        when(selectorMapper.findByPluginIds(Collections.singletonList("101"))).thenReturn(selectorDOList);
        assertEquals(StringUtils.EMPTY, pluginService.delete(Collections.singletonList("123")));
    }
    
    @Test
    public void testDeleteShouldSysPluginIdNotExist() {
        when(pluginMapper.selectById(any())).thenReturn(null);
        
        PluginDO pluginDO = buildPluginDO("123");
        final List<String> ids = Collections.singletonList(pluginDO.getId());
        assertEquals(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST, pluginService.delete(ids));
    }
    
    @Test
    public void testEnable() {
        
        List<String> idList = Lists.list("123", "1234");
        publishEvent();
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setIds(idList);
        given(this.pluginMapper.selectByIds(idList)).willReturn(Lists.list(buildPluginDO(), buildPluginDO()));
        assertThat(this.pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()), equalTo(StringUtils.EMPTY));
    }
    
    @Test
    public void testEnableShouldSysPluginIdNotExist() {
        when(pluginMapper.selectById(any())).thenReturn(null);
        
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setIds(Collections.singletonList("123"));
        
        given(this.pluginMapper.updateEnable(any())).willReturn(1);
        
        assertThat(this.pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()), equalTo(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST));
    }
    
    @Test
    public void testFindById() {
        PluginDO pluginDO = buildPluginDO();
        given(this.pluginMapper.selectById(eq("123"))).willReturn(pluginDO);
        PluginVO pluginVO = this.pluginService.findById("123");
        assertNotNull(pluginVO);
        assertEquals(pluginDO.getId(), pluginVO.getId());
    }
    
    @Test
    public void testListByPage() {
        PageParameter pageParameter = new PageParameter();
        pageParameter.setPageSize(5);
        pageParameter.setTotalCount(10);
        pageParameter.setTotalPage(pageParameter.getTotalCount() / pageParameter.getPageSize());
        PluginQuery pluginQuery = new PluginQuery("sofa", 1, pageParameter);
        List<PluginDO> pluginDOList = IntStream.range(0, 10).mapToObj(i -> buildPluginDO(String.valueOf(i))).collect(Collectors.toList());
        given(this.pluginMapper.selectByQuery(pluginQuery)).willReturn(pluginDOList);
        final CommonPager<PluginVO> pluginDOCommonPager = this.pluginService.listByPage(pluginQuery);
        assertEquals(pluginDOCommonPager.getDataList().size(), pluginDOList.size());
    }
    
    @Test
    public void testListAll() {
        PluginDO pluginDO = buildPluginDO("123");
        List<PluginDO> pluginDOList = Collections.singletonList(pluginDO);
        given(this.pluginMapper.selectAll()).willReturn(pluginDOList);
        List<PluginData> dataList = this.pluginService.listAll();
        assertNotNull(dataList);
        assertEquals(pluginDOList.size(), dataList.size());
    }
    
    private void publishEvent() {
        PluginDO pluginDO = buildPluginDO();
        given(this.pluginMapper.selectById("123")).willReturn(pluginDO);
    }
    
    private void testCreate() {
        PluginDTO pluginDTO = buildPluginDTO("");
        when(pluginMapper.nameExisted(pluginDTO.getName())).thenReturn(null);
        when(pluginMapper.insert(any())).thenReturn(1);
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, this.pluginService.createOrUpdate(pluginDTO));
    }
    
    private void testUpdate() {
        PluginDO pluginDO = buildPluginDO();
        when(pluginMapper.selectByName(any())).thenReturn(pluginDO);
        
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId("123");
        pluginDTO.setName("test");
        when(pluginMapper.nameExistedExclude(pluginDTO.getName(), Collections.singletonList(pluginDO.getId()))).thenReturn(null);
        when(pluginMapper.update(any())).thenReturn(1);
        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, this.pluginService.createOrUpdate(pluginDTO));
    }
    
    private PluginDTO buildPluginDTO() {
        return buildPluginDTO("123");
    }
    
    private PluginDTO buildPluginDTO(final String id) {
        PluginDTO pluginDTO = new PluginDTO();
        if (StringUtils.isNotBlank(id)) {
            pluginDTO.setId(id);
        }
        pluginDTO.setName("test");
        pluginDTO.setConfig("{\"protocol\":\"zookeeper\",\"register\":\"127.0.0.1:2181\"}");
        pluginDTO.setRole("0");
        pluginDTO.setEnabled(true);
        return pluginDTO;
    }
    
    private PluginDO buildPluginDO() {
        PluginDO pluginDO = PluginDO.buildPluginDO(buildPluginDTO());
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        pluginDO.setDateCreated(now);
        pluginDO.setDateUpdated(now);
        return pluginDO;
    }
    
    private PluginDO buildPluginDO(final String id) {
        PluginDTO pluginDTO = new PluginDTO();
        if (StringUtils.isNotBlank(id)) {
            pluginDTO.setId(id);
        }
        pluginDTO.setName("test");
        pluginDTO.setRole("1");
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        pluginDO.setDateCreated(now);
        pluginDO.setDateUpdated(now);
        return pluginDO;
    }
}
