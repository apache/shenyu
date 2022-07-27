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

package org.apache.shenyu.web.controller;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.LinkedList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link LocalMetadataController}.
 */
@ExtendWith(MockitoExtension.class)
public final class LocalMetadataControllerTest {

    private MockMvc mockMvc;

    private MockMvc mockMvcSubscribersNull;

    private MetaData metaData;

    private List<MetaDataSubscriber> subscribers;

    @BeforeEach
    public void setUp() {
        subscribers = new LinkedList<>();
        subscribers.add(mock(MetaDataSubscriber.class));
        subscribers.add(mock(MetaDataSubscriber.class));
        LocalMetadataController metadataController = new LocalMetadataController(new TestObjectProvider<>(subscribers));
        this.mockMvc = MockMvcBuilders.standaloneSetup(metadataController).build();
        LocalMetadataController appAuthControllerSubNull = new LocalMetadataController(new TestObjectProvider<>(null));
        this.mockMvcSubscribersNull = MockMvcBuilders.standaloneSetup(appAuthControllerSubNull).build();
        metaData = initMetaData();
    }

    @Test
    public void testSaveOrUpdate() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu/meta/saveOrUpdate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(metaData)))
                .andReturn().getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        subscribers.forEach(subscriber -> verify(subscriber).onSubscribe(metaData));
        final MockHttpServletResponse subNullResponse = this.mockMvcSubscribersNull.perform(MockMvcRequestBuilders.post("/shenyu/meta/saveOrUpdate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaData)))
                .andReturn().getResponse();
        assertThat(subNullResponse.getStatus()).isEqualTo(HttpStatus.OK.value());
    }

    @Test
    public void testClean() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu/meta/delete")
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("id", "id")
                        .param("path", "path")
                        .param("rpcType", "rpcType"))
                .andReturn().getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        MetaData metaData = new MetaData();
        metaData.setId("id");
        metaData.setPath("path");
        metaData.setRpcType("rpcType");
        subscribers.forEach(subscriber -> verify(subscriber).unSubscribe(metaData));
        final MockHttpServletResponse subNullResponse = this.mockMvcSubscribersNull.perform(MockMvcRequestBuilders.get("/shenyu/meta/delete")
                .contentType(MediaType.APPLICATION_JSON)
                .param("id", "id")
                .param("path", "path")
                .param("rpcType", "rpcType"))
                .andReturn().getResponse();
        assertThat(subNullResponse.getStatus()).isEqualTo(HttpStatus.OK.value());
    }

    private MetaData initMetaData() {
        MetaData metaData = new MetaData();
        metaData.setId("id");
        metaData.setAppName("appName");
        metaData.setContextPath("contextPath");
        metaData.setPath("path");
        metaData.setRpcType("rpcType");
        metaData.setServiceName("serviceName");
        metaData.setMethodName("methodName");
        metaData.setParameterTypes("parameterTypes");
        metaData.setRpcExt("rpcExt");
        metaData.setEnabled(Boolean.TRUE);
        return metaData;
    }

}
