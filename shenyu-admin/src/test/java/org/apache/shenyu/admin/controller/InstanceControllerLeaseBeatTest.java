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

import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.protocol.RegistrationLeaseProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
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
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * The beat endpoint keeps the exact legacy plain-text response for v1 callers and
 * switches to the structured lease response only for a caller that presents the
 * {@code registration-lease-v1} capability.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class InstanceControllerLeaseBeatTest {

    private static final String BEAT_BODY = "{\"instanceIp\":\"10.0.0.1\",\"instancePort\":\"8080\","
            + "\"instanceType\":\"http\",\"namespaceId\":\"default\"}";

    private MockMvc mockMvc;

    @InjectMocks
    private InstanceController instanceController;

    @Mock
    private InstanceCheckService instanceCheckService;

    @BeforeEach
    void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(instanceController).build();
    }

    @Test
    void legacyCallerKeepsTheExactLegacyResponse() throws Exception {
        when(instanceCheckService.handleBeatInfo(any(InstanceBeatInfoDTO.class))).thenReturn(true);

        this.mockMvc.perform(post("/instance/beat").contentType(MediaType.APPLICATION_JSON).content(BEAT_BODY))
                .andExpect(status().isOk())
                .andExpect(content().string("success"));

        verify(instanceCheckService, never()).handleLeaseBeat(any(InstanceBeatInfoDTO.class));
    }

    @Test
    void leaseCallerReceivesTheStructuredLeaseResponse() throws Exception {
        when(instanceCheckService.handleLeaseBeat(any(InstanceBeatInfoDTO.class)))
                .thenReturn(RegistrationLeaseProtocol.LeaseResponse.ok("lease-1", 5, 16_000L));

        this.mockMvc.perform(post("/instance/beat")
                        .header(RegistrationProtocol.CAPABILITIES_HEADER, RegistrationLeaseProtocol.CAPABILITY)
                        .contentType(MediaType.APPLICATION_JSON).content(BEAT_BODY))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(RegistrationLeaseProtocol.OK))
                .andExpect(jsonPath("$.leaseId").value("lease-1"))
                .andExpect(jsonPath("$.intervalSeconds").value(5))
                .andExpect(jsonPath("$.expiryDeadlineMs").value(16_000));

        verify(instanceCheckService, never()).handleBeatInfo(any(InstanceBeatInfoDTO.class));
    }

    @Test
    void otherCapabilitiesKeepTheLegacyResponse() throws Exception {
        when(instanceCheckService.handleBeatInfo(any(InstanceBeatInfoDTO.class))).thenReturn(false);

        this.mockMvc.perform(post("/instance/beat")
                        .header(RegistrationProtocol.CAPABILITIES_HEADER, RegistrationProtocol.V1_CAPABILITIES)
                        .contentType(MediaType.APPLICATION_JSON).content(BEAT_BODY))
                .andExpect(status().isOk())
                .andExpect(content().string("parameter error"));

        verify(instanceCheckService, never()).handleLeaseBeat(any(InstanceBeatInfoDTO.class));
    }
}
