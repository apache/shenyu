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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.RuleConditionDTO;
import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.RuleConditionVO;
import org.dromara.soul.admin.vo.RuleVO;
import org.dromara.soul.common.utils.DateUtils;
import org.dromara.soul.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test case for RuleController.
 *
 * @author shijie666
 */
@RunWith(MockitoJUnitRunner.class)
public final class RuleControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private RuleController ruleController;

    @Mock
    private RuleService ruleService;

    private RuleConditionVO rCondition1 = new RuleConditionVO(
            "1334784248953081888", "1334784248944693666", "uri", "统一资源定位符", "match", "匹配", "/", "/http/test/**", DateUtils.localDateTimeToString(LocalDateTime.now()),DateUtils.localDateTimeToString(LocalDateTime.now())
    );
    private RuleConditionVO rCondition2 = new RuleConditionVO(
            "1334784248953081999", "1334784248944693555", "uri", "统一资源定位符", "match", "匹配", "/", "/http/test/**", DateUtils.localDateTimeToString(LocalDateTime.now()),DateUtils.localDateTimeToString(LocalDateTime.now())
    );
    private final List<RuleConditionVO> rclist = new ArrayList<>(Arrays.asList(rCondition1, rCondition2));

    private final RuleVO ruleVO = new RuleVO("1334784248944693666", "1334784248336519168", 0, "模式零", "/http/test/**", true, true, 1, "{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}",
            rclist, DateUtils.localDateTimeToString(LocalDateTime.now()),DateUtils.localDateTimeToString(LocalDateTime.now()));


    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(ruleController).build();
    }

    @Test
    public void testDetailRule() throws Exception {
        given(this.ruleService.findById("1334784248944693666")).willReturn(ruleVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(ruleVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreateRule() throws Exception {
        RuleConditionDTO ruleConditionDTO = new RuleConditionDTO();
        ruleConditionDTO.setId("1334784248953081888");
        ruleConditionDTO.setRuleId("1334784248944693666");
        ruleConditionDTO.setParamType("uri");
        ruleConditionDTO.setOperator("match");
        ruleConditionDTO.setParamName("/");
        ruleConditionDTO.setParamValue("/http/order/save");

        List<RuleConditionDTO> conList = new ArrayList<>();
        conList.add(ruleConditionDTO);

        RuleDTO ruleDTO = new RuleDTO();
        ruleDTO.setId("1334784248944693666");
        ruleDTO.setSelectorId("1334784248336519168");
        ruleDTO.setMatchMode(0);
        ruleDTO.setName("/http/order/save");
        ruleDTO.setEnabled(true);
        ruleDTO.setLoged(true);
        ruleDTO.setSort(1);
        ruleDTO.setHandle("{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}");

        ruleDTO.setRuleConditions(conList);


        given(this.ruleService.createOrUpdate(ruleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("", ruleDTO)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ruleDTO))
                )
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdateRule() throws Exception {
        RuleConditionDTO ruleConditionDTO = new RuleConditionDTO();
        ruleConditionDTO.setId("1334784248953081888");
        ruleConditionDTO.setRuleId("1334784248944693666");
        ruleConditionDTO.setParamType("uri");
        ruleConditionDTO.setOperator("match");
        ruleConditionDTO.setParamName("/");
        ruleConditionDTO.setParamValue("/http/order/update");

        List<RuleConditionDTO> conList = new ArrayList<>();
        conList.add(ruleConditionDTO);

        RuleDTO ruleDTO = new RuleDTO();
        ruleDTO.setId("1334784248944693666");
        ruleDTO.setSelectorId("1334784248336519168");
        ruleDTO.setMatchMode(0);
        ruleDTO.setName("/http/order/update");
        ruleDTO.setEnabled(true);
        ruleDTO.setLoged(true);
        ruleDTO.setSort(1);
        ruleDTO.setHandle("{\"loadBalance\":\"random\",\"retry\":0,\"timeout\":3000}");

        ruleDTO.setRuleConditions(conList);

        given(this.ruleService.createOrUpdate(ruleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/{id}", ruleDTO)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ruleDTO))
                )
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeleteRules() throws Exception {
        List<String> list = new ArrayList<>();
        list.add("111");
        list.add("222");
        list.add("333");

        given(this.ruleService.delete(list)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/batch", list))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }

}
