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

import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.vo.RuleConditionVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
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
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test case for RuleController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class RuleControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private RuleController ruleController;

    @Mock
    private RuleService ruleService;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private NamespaceMapper namespaceMapper;

    private final RuleConditionVO rCondition1 = new RuleConditionVO(
            "888", "666", "uri", "Uniform", "match", "match", "/", "/http/test/**", DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now())
    );

    private final List<RuleConditionVO> rcList = new ArrayList<>(Collections.singletonList(rCondition1));

    private final RuleVO ruleVO = new RuleVO("666", "168", 0, "zero mode", "/http/test/**", true, true, 1, "{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}", false,
            rcList, DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()));

    private final CommonPager<RuleVO> commonPager = new CommonPager<>(new PageParameter(), Collections.singletonList(ruleVO));

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(ruleController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
        // mock login user
        final UserInfo mockLoginUser = new UserInfo();
        mockLoginUser.setUserId("1");
        mockLoginUser.setUserName("admin");
        SessionUtil.setLocalVisitor(mockLoginUser);
    }

    @Test
    public void testQueryRules() throws Exception {
        given(this.ruleService.searchByPageToPager(any())).willReturn(commonPager);
        String urlTemplate = "/rule?selectorId={selectorId}&name={name}&currentPage={currentPage}&pageSize={pageSize}";
        this.mockMvc.perform(MockMvcRequestBuilders.get(urlTemplate, "168", "/http/test/**", 1, 12))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].id", is(ruleVO.getId())))
                .andReturn();

    }

    @Test
    public void testDetailRule() throws Exception {
        given(this.ruleService.findById("666")).willReturn(ruleVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/rule/{id}", "666"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(ruleVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreateRule() throws Exception {
        RuleConditionDTO ruleConditionDTO = RuleConditionDTO.builder()
                .id("888")
                .ruleId("666")
                .paramType("uri")
                .operator("match")
                .paramName("/")
                .paramValue("test")
                .paramType("/http/order/save")
                .build();
        List<RuleConditionDTO> conList = new ArrayList<>();
        conList.add(ruleConditionDTO);

        RuleDTO ruleDTO = RuleDTO.builder()
                .id("666")
                .selectorId("168")
                .matchMode(0)
                .name("/http/order/save")
                .enabled(true)
                .loged(true)
                .matchRestful(false)
                .sort(1)
                .handle("{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}")
                .ruleConditions(conList)
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(RuleMapper.class)).thenReturn(ruleMapper);
        when(ruleMapper.existed(ruleDTO.getId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(SelectorMapper.class)).thenReturn(selectorMapper);
        when(selectorMapper.existed(ruleDTO.getSelectorId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        given(this.ruleService.createOrUpdate(ruleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/rule", ruleDTO)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ruleDTO))
                )
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdateRule() throws Exception {
        RuleConditionDTO ruleConditionDTO = RuleConditionDTO.builder()
                .id("888")
                .ruleId("666")
                .paramType("uri")
                .operator("match")
                .paramName("/")
                .paramValue("/http/order/update")
                .build();
        List<RuleConditionDTO> conList = new ArrayList<>();
        conList.add(ruleConditionDTO);

        RuleDTO ruleDTO = RuleDTO.builder()
                .id("666")
                .selectorId("168")
                .matchMode(0)
                .name("/http/order/update")
                .enabled(true)
                .loged(true)
                .matchRestful(false)
                .sort(1)
                .handle("{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}")
                .ruleConditions(conList)
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(RuleMapper.class)).thenReturn(ruleMapper);
        when(ruleMapper.existed(ruleDTO.getId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(SelectorMapper.class)).thenReturn(selectorMapper);
        when(selectorMapper.existed(ruleDTO.getSelectorId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        given(this.ruleService.createOrUpdate(ruleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/rule/{id}", "666")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ruleDTO))
                )
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeleteRules() throws Exception {
        final BatchNamespaceCommonDTO batchNamespaceCommonDTO = new BatchNamespaceCommonDTO();
        batchNamespaceCommonDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        batchNamespaceCommonDTO.setIds(Collections.singletonList("111"));
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        given(this.ruleService.deleteByIdsAndNamespaceId(Collections.singletonList("111"), SYS_DEFAULT_NAMESPACE_ID)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/rule/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getGson().toJson(batchNamespaceCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testEnableRule() throws Exception {
        RuleDTO ruleDTO = RuleDTO.builder()
            .id("666")
            .selectorId("168")
            .matchMode(0)
            .name("/http/order/update")
            .enabled(true)
            .loged(true)
            .matchRestful(false)
            .sort(1)
            .handle("{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}")
            .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
            .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(RuleMapper.class)).thenReturn(ruleMapper);
        when(ruleMapper.existed(ruleDTO.getId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(SelectorMapper.class)).thenReturn(selectorMapper);
        when(selectorMapper.existed(ruleDTO.getSelectorId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        given(this.ruleService.enabledByIdsAndNamespaceId(Arrays.asList(ruleDTO.getId()), false, SYS_DEFAULT_NAMESPACE_ID)).willReturn(true);
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setIds(Arrays.asList(ruleDTO.getId()));
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/rule/batchEnabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO))
            )
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.ENABLE_SUCCESS)))
            .andReturn();
    }

}
