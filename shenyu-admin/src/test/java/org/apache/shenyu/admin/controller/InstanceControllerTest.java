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

import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualVO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
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

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class InstanceControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private InstanceController instanceController;

    @Mock
    private InstanceInfoService instanceInfoService;

    @Mock
    private InstanceCheckService instanceCheckService;


    private InstanceInfoVO vo;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(instanceController)
                .build();
        this.vo = buildVO();
    }

    @Test
    public void testQueryPlugins() throws Exception {
        final PageParameter pageParameter = new PageParameter(1, 10);
        List<InstanceInfoVO> vos = new ArrayList<>();
        vos.add(vo);
        InstanceInfoVO second = buildVO();
        second.setId("instance-2");
        vos.add(second);
        CommonPager<InstanceInfoVO> pager = new CommonPager<>();
        pager.setPage(pageParameter);
        pager.setDataList(vos);
        given(this.instanceInfoService.listByPage(any(InstanceQuery.class))).willReturn(pager);
        given(this.instanceCheckService.getInstanceKey((InstanceInfoVO) any())).willReturn("key");
        InstanceInfoVO health = buildVO();
        health.setInstanceState(2);
        health.setLastHeartBeatTime(123L);
        Timestamp healthDateUpdated = new Timestamp(456L);
        health.setDateUpdated(healthDateUpdated);
        given(this.instanceCheckService.getInstanceHealthBeatInfo(anyString())).willReturn(health);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/instance")
                        .param("instanceType", vo.getInstanceType())
                        .param("instanceIp", vo.getInstanceIp())
                        .param("instancePort", vo.getInstancePort())
                        .param("namespaceId", vo.getNamespaceId())
                        .param("currentPage", String.valueOf(pageParameter.getCurrentPage()))
                        .param("pageSize", String.valueOf(pageParameter.getPageSize())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].instanceIp", is(vo.getInstanceIp())))
                .andExpect(jsonPath("$.data.dataList[0].id", is("instance-1")))
                .andExpect(jsonPath("$.data.dataList[1].id", is("instance-2")));
        assertEquals(health.getLastHeartBeatTime(), vo.getLastHeartBeatTime());
        assertEquals(health.getInstanceState(), vo.getInstanceState());
        assertEquals(healthDateUpdated, vo.getDateUpdated());
    }

    @Test
    public void testQueryPluginsWithMissingHealthBeatInfo() throws Exception {
        final PageParameter pageParameter = new PageParameter(1, 10);
        Timestamp dateUpdated = new Timestamp(123L);
        vo.setInstanceState(2);
        vo.setLastHeartBeatTime(0L);
        vo.setDateUpdated(dateUpdated);
        InstanceInfoVO persistedHeartbeatVO = buildVO();
        persistedHeartbeatVO.setInstanceState(3);
        persistedHeartbeatVO.setLastHeartBeatTime(456L);
        CommonPager<InstanceInfoVO> pager = new CommonPager<>();
        pager.setPage(pageParameter);
        pager.setDataList(List.of(vo, persistedHeartbeatVO));
        given(this.instanceInfoService.listByPage(any(InstanceQuery.class))).willReturn(pager);
        given(this.instanceCheckService.getInstanceKey((InstanceInfoVO) any())).willReturn("key");
        given(this.instanceCheckService.getInstanceHealthBeatInfo(anyString())).willReturn(null);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/instance")
                        .param("instanceType", vo.getInstanceType())
                        .param("instanceIp", vo.getInstanceIp())
                        .param("instancePort", vo.getInstancePort())
                        .param("namespaceId", vo.getNamespaceId())
                        .param("currentPage", String.valueOf(pageParameter.getCurrentPage()))
                        .param("pageSize", String.valueOf(pageParameter.getPageSize())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.dataList[0].instanceState", is(2)))
                .andExpect(jsonPath("$.data.dataList[0].lastHeartBeatTime", is(123)))
                .andExpect(jsonPath("$.data.dataList[1].instanceState", is(3)))
                .andExpect(jsonPath("$.data.dataList[1].lastHeartBeatTime", is(456)));
        assertEquals(dateUpdated, vo.getDateUpdated());
    }

    @Test
    public void testDetailInstanceInfo() throws Exception {
        given(this.instanceInfoService.findById("123")).willReturn(vo);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/instance/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.instanceIp", is(vo.getInstanceIp())))
                .andExpect(jsonPath("$.data.id", is("instance-1")));
    }

    @Test
    public void testBeat() throws Exception {
        String json = "{\"instanceIp\":\"127.0.0.1\",\"instancePort\":\"8080\",\"instanceType\":\"grpc\",\"instanceInfo\":\"info\",\"namespaceId\":\"ns\"}";
        this.mockMvc.perform(MockMvcRequestBuilders.post("/instance/beat")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json))
                .andExpect(status().isOk());
        verify(instanceCheckService).handleBeatInfo(any());
    }

    @Test
    public void testGetInstanceDataVisual() throws Exception {
        given(this.instanceCheckService.getInstanceDataVisual("ns")).willReturn(new InstanceDataVisualVO());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/instance/analysis/{namespaceId}", "ns"))
                .andExpect(status().isOk());
    }

    private InstanceInfoVO buildVO() {
        InstanceInfoVO v = new InstanceInfoVO();
        v.setId("instance-1");
        v.setInstanceIp("127.0.0.1");
        v.setInstancePort("8080");
        v.setInstanceType("grpc");
        v.setInstanceInfo("info");
        v.setNamespaceId("ns");
        v.setInstanceState(1);
        v.setLastHeartBeatTime(System.currentTimeMillis());
        v.setDateCreated(new Timestamp(System.currentTimeMillis()));
        v.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return v;
    }
}
