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
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
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
import java.util.Collections;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for {@link SelectorController}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SelectorControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private SelectorController selectorController;

    @Mock
    private SelectorService selectorService;
    
    @Mock
    private SelectorMapper selectorMapper;
    
    @Mock
    private PluginMapper pluginMapper;
    
    private final SelectorVO selectorVO = new SelectorVO("1", "2", "selector-1", MatchModeEnum.AND.getCode(),
            MatchModeEnum.AND.getName(), SelectorTypeEnum.FULL_FLOW.getCode(), SelectorTypeEnum.FULL_FLOW.getName(),
            1, true, true, true, "handle", Collections.emptyList(),
            DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()));

    private final CommonPager<SelectorVO> commonPager = new CommonPager<>(new PageParameter(), Collections.singletonList(selectorVO));

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(selectorController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
        // mock login user
        final UserInfo mockLoginUser = new UserInfo();
        mockLoginUser.setUserId("1");
        mockLoginUser.setUserName("admin");
        SessionUtil.setLocalVisitor(mockLoginUser);
    }

    @Test
    public void querySelectors() throws Exception {
        given(this.selectorService.searchByPageToPager(any())).willReturn(commonPager);
        String urlTemplate = "/selector?pluginId={pluginId}&name={name}&currentPage={currentPage}&pageSize={pageSize}";
        this.mockMvc.perform(MockMvcRequestBuilders.get(urlTemplate, "2", "selector-1", 1, 12))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].id", is(selectorVO.getId())))
                .andReturn();
    }

    @Test
    public void createSelector() throws Exception {
        SelectorDTO selectorDTO = SelectorDTO.builder()
                .id("123")
                .name("test123")
                .continued(true)
                .type(1)
                .loged(true)
                .enabled(true)
                .pluginId("2")
                .sort(1)
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(SelectorMapper.class)).thenReturn(selectorMapper);
        when(selectorMapper.existed(selectorDTO.getId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(PluginMapper.class)).thenReturn(pluginMapper);
        when(pluginMapper.existed(selectorDTO.getPluginId())).thenReturn(true);
        given(this.selectorService.createOrUpdate(selectorDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/selector")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(selectorDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void updateSelector() throws Exception {
        SelectorDTO selectorDTO = SelectorDTO.builder()
                .id("123")
                .name("test123")
                .continued(true)
                .type(1)
                .loged(true)
                .enabled(true)
                .pluginId("2")
                .sort(1)
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(SelectorMapper.class)).thenReturn(selectorMapper);
        when(selectorMapper.existed(selectorDTO.getId())).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(PluginMapper.class)).thenReturn(pluginMapper);
        when(pluginMapper.existed(selectorDTO.getPluginId())).thenReturn(true);
        given(this.selectorService.createOrUpdate(selectorDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/selector/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(selectorDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void deleteSelector() throws Exception {
        given(this.selectorService.delete(Collections.singletonList("123"))).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/selector/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void detailSelector() throws Exception {
        given(this.selectorService.findById("1")).willReturn(selectorVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/selector/{id}", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(selectorVO.getId())))
                .andReturn();
    }
}
