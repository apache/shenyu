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
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
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

import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.Matchers.containsString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DiscoveryUpstreamControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private DiscoveryUpstreamController discoveryUpstreamController;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private NamespaceMapper namespaceMapper;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(discoveryUpstreamController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
    }

    @Test
    public void testCreateWithNonExistentDiscoveryHandlerIdShouldFail() throws Exception {
        DiscoveryUpstreamDTO dto = buildDTO("nonExistentHandlerId");
        when(SpringBeanUtils.getInstance().getBean(DiscoveryHandlerMapper.class)).thenReturn(discoveryHandlerMapper);
        when(discoveryHandlerMapper.existed("nonExistentHandlerId")).thenReturn(false);
        when(SpringBeanUtils.getInstance().getBean(DiscoveryUpstreamMapper.class)).thenReturn(discoveryUpstreamMapper);
        when(discoveryUpstreamMapper.existed(dto.getId())).thenReturn(false);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/discovery-upstream")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(dto)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", containsString("discoveryHandlerId is not existed")));
    }

    @Test
    public void testCreateWithExistentDiscoveryHandlerIdShouldSucceed() throws Exception {
        DiscoveryUpstreamDTO dto = buildDTO("existentHandlerId");
        when(SpringBeanUtils.getInstance().getBean(DiscoveryHandlerMapper.class)).thenReturn(discoveryHandlerMapper);
        when(discoveryHandlerMapper.existed("existentHandlerId")).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(DiscoveryUpstreamMapper.class)).thenReturn(discoveryUpstreamMapper);
        when(discoveryUpstreamMapper.existed(dto.getId())).thenReturn(false);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        given(discoveryUpstreamService.createOrUpdate(dto)).willReturn("success");

        this.mockMvc.perform(MockMvcRequestBuilders.post("/discovery-upstream")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(dto)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200));
    }

    @Test
    public void testUpdateWithExistentDiscoveryHandlerIdShouldSucceed() throws Exception {
        DiscoveryUpstreamDTO dto = buildDTO("existentHandlerId");
        List<DiscoveryUpstreamDTO> dtoList = Collections.singletonList(dto);
        when(SpringBeanUtils.getInstance().getBean(DiscoveryHandlerMapper.class)).thenReturn(discoveryHandlerMapper);
        when(discoveryHandlerMapper.existed("existentHandlerId")).thenReturn(true);
        when(SpringBeanUtils.getInstance().getBean(DiscoveryUpstreamMapper.class)).thenReturn(discoveryUpstreamMapper);
        when(discoveryUpstreamMapper.existed(dto.getId())).thenReturn(false);
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        given(discoveryUpstreamService.updateBatch("existentHandlerId", dtoList)).willReturn(0);

        this.mockMvc.perform(MockMvcRequestBuilders.put("/discovery-upstream/{discoveryHandlerId}", "existentHandlerId")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(dtoList)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200));
    }

    private DiscoveryUpstreamDTO buildDTO(final String discoveryHandlerId) {
        DiscoveryUpstreamDTO dto = new DiscoveryUpstreamDTO();
        dto.setDiscoveryHandlerId(discoveryHandlerId);
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        dto.setUrl("http://localhost:8080");
        dto.setStatus(0);
        dto.setWeight(50);
        dto.setProps("{}");
        return dto;
    }
}
