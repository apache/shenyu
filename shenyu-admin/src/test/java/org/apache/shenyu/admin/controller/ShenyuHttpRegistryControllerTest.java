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
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.register.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.admin.service.impl.NamespaceServiceImpl;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.transfer.NamespaceTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
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

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
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

    @Mock
    private NamespaceServiceImpl namespaceService;

    @Mock
    private NamespaceMapper namespaceMapper;

    @InjectMocks
    private ShenyuClientHttpRegistryController shenyuHttpRegistryController;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(shenyuHttpRegistryController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();

        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
    }

    @Test
    public void testRegisterMetadata() throws Exception {
        given(namespaceMapper.insertSelective(buildNamespaceDO())).willReturn(1);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        when(namespaceService.findByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(buildNamespaceVo());
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder()
                .appName("app")
                .enabled(true)
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .host("127.0.0.1")
                .port(8080)
                .path("/register")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
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
        given(namespaceMapper.insertSelective(buildNamespaceDO())).willReturn(1);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(NamespaceMapper.class)).thenReturn(namespaceMapper);
        when(namespaceMapper.existed(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(true);
        when(namespaceService.findByNamespaceId(SYS_DEFAULT_NAMESPACE_ID)).thenReturn(buildNamespaceVo());
        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                .appName("app")
                .host("127.0.0.1")
                .port(8080)
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
                .build();
        doNothing().when(publisher).publish(uriRegisterDTO);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/register-uri")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(uriRegisterDTO)))
                .andExpect(status().isOk())
                .andExpect(content().string(ShenyuResultMessage.SUCCESS))
                .andReturn();
    }

    private NamespaceDO buildNamespaceDO() {
        return NamespaceDO.builder()
                .id("1")
                .name("test")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
                .description("test")
                .build();
    }

    private NamespaceVO buildNamespaceVo() {
        return NamespaceTransfer.INSTANCE.mapToVo(buildNamespaceDO());
    }
}
