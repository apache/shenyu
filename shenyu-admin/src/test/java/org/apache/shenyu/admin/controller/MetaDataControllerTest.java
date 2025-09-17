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

package org.apache.shenyu.admin.controller;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for MetaDataController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class MetaDataControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private MetaDataController metaDataController;

    @Mock
    private MetaDataService metaDataService;
    
    @Mock
    private MetaDataMapper metaDataMapper;

    @Mock
    private NamespaceMapper namespaceMapper;

    private final MetaDataVO metaDataVO = new MetaDataVO("appName", "appPath", "desc", "rpcType", "serviceName", "methodName", "types", "rpcExt",
            "1", DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()),
            true, SYS_DEFAULT_NAMESPACE_ID);

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(metaDataController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
    }

    @Test
    public void testQueryList() throws Exception {
        final PageParameter pageParameter = new PageParameter();
        List<MetaDataVO> metaDataVOS = new ArrayList<>();
        metaDataVOS.add(metaDataVO);
        final CommonPager<MetaDataVO> commonPager = new CommonPager<>();
        commonPager.setPage(pageParameter);
        commonPager.setDataList(metaDataVOS);
        final MetaDataQuery metaDataQuery = new MetaDataQuery("path", pageParameter, SYS_DEFAULT_NAMESPACE_ID);
        given(this.metaDataService.listByPage(metaDataQuery)).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/queryList")
                .param("path", "path")
                .param("currentPage", String.valueOf(pageParameter.getCurrentPage()))
                .param("pageSize", String.valueOf(pageParameter.getPageSize()))
                .param("namespaceId", SYS_DEFAULT_NAMESPACE_ID))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testFindAll() throws Exception {
        List<MetaDataVO> metaDataVOS = new ArrayList<>();
        metaDataVOS.add(metaDataVO);
        given(this.metaDataService.findAll()).willReturn(metaDataVOS);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/findAll"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data[0].appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testFindAllGroup() throws Exception {
        final Map<String, List<MetaDataVO>> result = new HashMap<>();
        String groupName = "groupName-1";
        List<MetaDataVO> metaDataVOS = new ArrayList<>();
        metaDataVOS.add(metaDataVO);
        result.put(groupName, metaDataVOS);
        given(this.metaDataService.findAllGroup()).willReturn(result);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/findAllGroup"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data." + groupName + "[0].appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testEditor() throws Exception {
        given(this.metaDataService.findById("1")).willReturn(metaDataVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/meta-data/{id}", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.appName", is(metaDataVO.getAppName())))
                .andReturn();
    }

    @Test
    public void testCreateOrUpdate() throws Exception {
        final MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("0001");
        metaDataDTO.setAppName("aname-01");
        metaDataDTO.setContextPath("path");
        metaDataDTO.setPath("/path");
        metaDataDTO.setRpcType("rpcType");
        metaDataDTO.setMethodName("methodName");
        metaDataDTO.setServiceName("serviceName");
        metaDataDTO.setRuleName("ruleName");
        metaDataDTO.setEnabled(false);
        metaDataDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(MetaDataMapper.class)).thenReturn(metaDataMapper);
        when(metaDataMapper.existed(metaDataDTO.getId())).thenReturn(true);
        given(this.metaDataService.createOrUpdate(metaDataDTO)).willReturn(ShenyuResultMessage.UPDATE_SUCCESS);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/createOrUpdate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testCreateOrUpdateWithError() throws Exception {
        final MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("0001");
        metaDataDTO.setAppName("aname-01");
        metaDataDTO.setContextPath("path");
        metaDataDTO.setPath("/path");
        metaDataDTO.setRpcType("rpcType");
        metaDataDTO.setMethodName("methodName");
        metaDataDTO.setServiceName("serviceName");
        metaDataDTO.setRuleName("ruleName");
        metaDataDTO.setEnabled(false);
        metaDataDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(MetaDataMapper.class)).thenReturn(metaDataMapper);
        when(metaDataMapper.existed(metaDataDTO.getId())).thenReturn(null);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/createOrUpdate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(500)))
                .andExpect(jsonPath("$.message", is("Request error! invalid argument [id: meta data is not existed]")))
                .andReturn();
    }

    @Test
    public void testBatchDeleted() throws Exception {
        final List<String> ids = new ArrayList<>(2);
        ids.add("1");
        ids.add("2");
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        given(this.metaDataService.deleteByIdsAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID)).willReturn(2);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        final BatchNamespaceCommonDTO batchNamespaceCommonDTO = new BatchNamespaceCommonDTO();
        batchNamespaceCommonDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        batchNamespaceCommonDTO.setIds(ids);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/meta-data/batchDeleted")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchNamespaceCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(2)))
                .andReturn();
    }

    @Test
    public void testBatchEnabled() throws Exception {
        final BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setIds(Arrays.asList("1", "2"));
        batchCommonDTO.setEnabled(true);
        given(this.metaDataService.enabledByIdsAndNamespaceId(batchCommonDTO.getIds(), batchCommonDTO.getEnabled(), SYS_DEFAULT_NAMESPACE_ID)).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/batchEnabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.ENABLE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testBatchEnabledWithError() throws Exception {
        final BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setIds(Arrays.asList("1", "2"));
        batchCommonDTO.setEnabled(true);
        batchCommonDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        given(this.metaDataService.enabledByIdsAndNamespaceId(batchCommonDTO.getIds(), batchCommonDTO.getEnabled(), SYS_DEFAULT_NAMESPACE_ID)).willReturn(AdminConstants.ID_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/batchEnabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.ID_NOT_EXIST)))
                .andReturn();
    }

    @Test
    public void testSyncData() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.post("/meta-data/syncData"))
                .andExpect(status().isOk())
                .andReturn();
    }
}
