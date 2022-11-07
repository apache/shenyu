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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.shenyu.admin.model.dto.AlertTemplateDTO;
import org.apache.shenyu.admin.model.entity.AlertTemplateDO;
import org.apache.shenyu.admin.model.vo.AlertTemplateVO;
import org.apache.shenyu.admin.service.AlertTemplateService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for AlertTemplateControllerTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AlertTemplateControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private AlertTemplateController alertTemplateController;

    @Mock
    private AlertTemplateService alertTemplateService;

    private final AlertTemplateDTO alertTemplateDTO = build();

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(alertTemplateController)
                .build();
    }

    @Test
    public void testAddTemplate() throws Exception {

        given(this.alertTemplateService.addTemplate(alertTemplateDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/alertTemplate/addTemplate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(alertTemplateDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeleteTemplate() throws Exception {
        final List<String> ids = new ArrayList<>(2);
        ids.add("0001");
        ids.add("0002");
        given(this.alertTemplateService.deleteTemplate(ids)).willReturn(2);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/alertTemplate/deleteTemplate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ids)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(2)))
                .andReturn();
    }

    @Test
    public void testUpdateTemplate() throws Exception {
        AlertTemplateDTO alertTemplateDTO = build();
        given(this.alertTemplateService.updateTemplate(any())).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/alertTemplate/updateTemplate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(alertTemplateDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)))
                .andReturn();
    }

    @Test
    public void testGetAll() throws Exception {
        List<AlertTemplateVO> alertTemplateVOS = new ArrayList<>();
        alertTemplateVOS.add(buildAlertTemplateVO());
        given(this.alertTemplateService.getAll()).willReturn(alertTemplateVOS);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/alertTemplate/getAll"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data[0].id", is(10)))
                .andReturn();
    }

    @Test
    public void testDetail() throws Exception {
        List<AlertTemplateVO> alertTemplateVOS = new ArrayList<>();
        alertTemplateVOS.add(buildAlertTemplateVO());
        given(this.alertTemplateService.detail(10L)).willReturn(buildAlertTemplateDO());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/alertTemplate/detail/10")
                        .param("id", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.name", is("123")))
                .andReturn();
    }

    private AlertTemplateDTO build() {
        AlertTemplateDTO alertTemplateDTO = new AlertTemplateDTO();
        alertTemplateDTO.setId(10L);
        alertTemplateDTO.setContent("1111");
        alertTemplateDTO.setStrategyName("ddd");
        alertTemplateDTO.setName("123");
        return alertTemplateDTO;
    }

    private AlertTemplateVO buildAlertTemplateVO() {
        AlertTemplateVO alertTemplateVO = new AlertTemplateVO();
        alertTemplateVO.setId(10L);
        alertTemplateVO.setStrategyName("ddd");
        alertTemplateVO.setName("123");
        alertTemplateVO.setDateCreated(new Date().toString());
        alertTemplateVO.setDateUpdated(new Date().toString());
        return alertTemplateVO;
    }

    private AlertTemplateDO buildAlertTemplateDO() {
        AlertTemplateDO alertTemplateDO = new AlertTemplateDO();
        alertTemplateDO.setId(10L);
        alertTemplateDO.setStrategyName("ddd");
        alertTemplateDO.setName("123");
        alertTemplateDO.setContent("12334");
        alertTemplateDO.setDateCreated(new Date());
        alertTemplateDO.setDateUpdated(new Date());
        return alertTemplateDO;
    }

}
