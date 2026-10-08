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
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
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

import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ProxySelectorController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ProxySelectorControllerTest {

    private static final String NAMESPACE_ID = "default";

    private MockMvc mockMvc;

    @InjectMocks
    private ProxySelectorController proxySelectorController;

    @Mock
    private ProxySelectorService proxySelectorService;

    @Mock
    private NamespaceMapper namespaceMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(proxySelectorController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(SpringBeanUtils.getInstance().getBean(ProxySelectorMapper.class)).thenReturn(proxySelectorMapper);
    }

    @Test
    public void testQueryProxySelectorShouldReturnPage() throws Exception {
        ProxySelectorVO proxySelectorVO = new ProxySelectorVO();
        proxySelectorVO.setName("test-proxy-selector");
        CommonPager<ProxySelectorVO> commonPager = new CommonPager<>(new PageParameter(1, 12),
                Collections.singletonList(proxySelectorVO));
        when(proxySelectorService.listByPage(any())).thenReturn(commonPager);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/proxy-selector")
                        .param("name", "test-proxy-selector")
                        .param("currentPage", "1")
                        .param("pageSize", "12")
                        .param("namespaceId", NAMESPACE_ID))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.dataList[0].name").value("test-proxy-selector"));
    }

    @Test
    public void testAddProxySelectorShouldDelegateToService() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(true);
        when(proxySelectorService.create(any(ProxySelectorAddDTO.class))).thenReturn("success");

        this.mockMvc.perform(MockMvcRequestBuilders.post("/proxy-selector/addProxySelector")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(buildProxySelectorJson()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.message").value("success"));
        verify(proxySelectorService).create(any(ProxySelectorAddDTO.class));
    }

    @Test
    public void testAddProxySelectorWithMissingNameShouldFail() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.post("/proxy-selector/addProxySelector")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"pluginName\":\"divide\",\"type\":\"zookeeper\",\"namespaceId\":\"default\","
                                + "\"discovery\":{\"discoveryType\":\"zookeeper\",\"serverList\":\"localhost:2181\"}}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", containsString("name: must not be blank")));
    }

    @Test
    public void testAddProxySelectorWithNonExistentNamespaceShouldFail() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(false);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/proxy-selector/addProxySelector")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(buildProxySelectorJson()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", containsString("namespaceId is not existed")));
    }

    @Test
    public void testUpdateProxySelectorShouldSetIdFromPath() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(true);
        when(proxySelectorMapper.existed("proxy-selector-1")).thenReturn(true);
        when(proxySelectorService.createOrUpdate(any(ProxySelectorAddDTO.class))).thenReturn("success");

        this.mockMvc.perform(MockMvcRequestBuilders.put("/proxy-selector/{id}", "proxy-selector-1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(buildProxySelectorJson()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.message").value("success"));

        ArgumentCaptor<ProxySelectorAddDTO> captor = ArgumentCaptor.forClass(ProxySelectorAddDTO.class);
        verify(proxySelectorService).createOrUpdate(captor.capture());
        assertEquals("proxy-selector-1", captor.getValue().getId());
    }

    @Test
    public void testDeleteProxySelectorsShouldDelegateToService() throws Exception {
        List<String> ids = List.of("id-1", "id-2");
        when(proxySelectorService.delete(ids)).thenReturn("success");

        this.mockMvc.perform(MockMvcRequestBuilders.delete("/proxy-selector/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("[\"id-1\",\"id-2\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.message").value("success"));
        verify(proxySelectorService).delete(ids);
    }

    @Test
    public void testFetchDataShouldDelegateToService() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.put("/proxy-selector/fetch/{discoveryHandlerId}", "handler-1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200));
        verify(proxySelectorService).fetchData("handler-1");
    }

    @Test
    public void testBindingSelectorShouldDelegateToService() throws Exception {
        when(namespaceMapper.existed(NAMESPACE_ID)).thenReturn(true);
        when(proxySelectorService.bindingDiscoveryHandler(any(ProxySelectorAddDTO.class))).thenReturn("success");

        this.mockMvc.perform(MockMvcRequestBuilders.post("/proxy-selector/binding")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(buildProxySelectorJson()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.message").value("success"));
        verify(proxySelectorService).bindingDiscoveryHandler(any(ProxySelectorAddDTO.class));
    }

    private String buildProxySelectorJson() {
        return "{\"name\":\"test-proxy-selector\",\"pluginName\":\"divide\",\"type\":\"zookeeper\","
                + "\"forwardPort\":8080,\"props\":\"{}\",\"namespaceId\":\"default\","
                + "\"discovery\":{\"discoveryType\":\"zookeeper\",\"serverList\":\"localhost:2181\",\"props\":\"{}\"}}";
    }
}
