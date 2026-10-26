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
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
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

import java.util.List;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for DiscoveryController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DiscoveryControllerTest {

    private static final String NAMESPACE_ID = "default";

    private MockMvc mockMvc;

    @InjectMocks
    private DiscoveryController discoveryController;

    @Mock
    private DiscoveryService discoveryService;

    @Mock
    private NamespaceMapper namespaceMapper;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(discoveryController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
    }

    @Test
    public void testTypeEnumsShouldReturnServiceResult() throws Exception {
        when(discoveryService.typeEnums()).thenReturn(List.of("zookeeper", "etcd"));

        this.mockMvc.perform(MockMvcRequestBuilders.get("/discovery/typeEnums"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data[0]").value("zookeeper"))
                .andExpect(jsonPath("$.data[1]").value("etcd"));
    }

    @Test
    public void testDiscoveryShouldDelegateToService() throws Exception {
        DiscoveryVO discoveryVO = new DiscoveryVO();
        discoveryVO.setPluginName("divide");
        when(discoveryService.discovery("divide", "proxy", NAMESPACE_ID)).thenReturn(discoveryVO);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/discovery")
                        .param("pluginName", "divide")
                        .param("level", "proxy")
                        .param("namespaceId", NAMESPACE_ID))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.pluginName").value("divide"));
    }

    @Test
    public void testCreateOrUpdateShouldDelegateToService() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(true);
        DiscoveryDTO discoveryDTO = buildDiscoveryDTO();
        DiscoveryVO discoveryVO = new DiscoveryVO();
        discoveryVO.setDiscoveryName(discoveryDTO.getName());
        when(discoveryService.createOrUpdate(any(DiscoveryDTO.class))).thenReturn(discoveryVO);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/discovery/insertOrUpdate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(discoveryDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.discoveryName").value("test-discovery"));
        verify(discoveryService).createOrUpdate(any(DiscoveryDTO.class));
    }

    @Test
    public void testCreateOrUpdateWithMissingNameShouldFail() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(true);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/discovery/insertOrUpdate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"level\":\"proxy\",\"pluginName\":\"divide\",\"type\":\"zookeeper\","
                                + "\"serverList\":\"localhost:2181\",\"props\":\"{}\",\"namespaceId\":\"default\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", containsString("name not null")));
    }

    @Test
    public void testCreateOrUpdateWithNonExistentNamespaceShouldFail() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(false);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/discovery/insertOrUpdate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(buildDiscoveryDTO())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", containsString("namespaceId is not existed")));
    }

    @Test
    public void testDeleteWithExistentNamespaceShouldSucceed() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(true);
        when(discoveryService.delete("discovery-1", NAMESPACE_ID)).thenReturn("success");

        this.mockMvc.perform(MockMvcRequestBuilders.delete("/discovery/{discoveryId}", "discovery-1")
                        .param("namespaceId", NAMESPACE_ID))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data").value("success"));
        verify(discoveryService).delete("discovery-1", NAMESPACE_ID);
    }

    private DiscoveryDTO buildDiscoveryDTO() {
        DiscoveryDTO discoveryDTO = new DiscoveryDTO();
        discoveryDTO.setId("discovery-1");
        discoveryDTO.setLevel("proxy");
        discoveryDTO.setName("test-discovery");
        discoveryDTO.setPluginName("divide");
        discoveryDTO.setType("zookeeper");
        discoveryDTO.setServerList("localhost:2181");
        discoveryDTO.setProps("{}");
        discoveryDTO.setNamespaceId(NAMESPACE_ID);
        return discoveryDTO;
    }
}
