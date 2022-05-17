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

import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
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

import static org.mockito.Mockito.doNothing;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for {@link ShenyuClientHttpRegistryController}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuHttpRegistryControllerTest {

    private MockMvc mockMvc;

    @Mock
    private ShenyuClientServerRegisterPublisher publisher;

    @InjectMocks
    private ShenyuClientHttpRegistryController shenyuHttpRegistryController;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(shenyuHttpRegistryController).build();
    }

    @Test
    public void testRegisterMetadata() throws Exception {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder()
                .appName("app")
                .enabled(true)
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .host("127.0.0.1")
                .port(8080)
                .path("/register")
                .build();
        doNothing().when(publisher).publish(metaDataRegisterDTO);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/register-metadata")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(metaDataRegisterDTO)))
                .andExpect(status().isOk())
                .andExpect(content().string(ShenyuResultMessage.SUCCESS))
                .andReturn();
    }

    @Test
    public void testRegisterURI() throws Exception {
        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                .appName("app")
                .host("127.0.0.1")
                .port(8080)
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .build();
        doNothing().when(publisher).publish(uriRegisterDTO);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/register-uri")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(uriRegisterDTO)))
                .andExpect(status().isOk())
                .andExpect(content().string(ShenyuResultMessage.SUCCESS))
                .andReturn();
    }
}
