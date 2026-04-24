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

package org.apache.shenyu.admin.disruptor.subscriber;

import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ApiDocExecutorSubscriber}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ApiDocExecutorSubscriberTest {

    @InjectMocks
    private ApiDocExecutorSubscriber apiDocExecutorSubscriber;

    @Mock
    private Map<String, ShenyuClientRegisterService> shenyuClientRegisterService;

    @Test
    void testGetType() {
        assertEquals(DataType.API_DOC, apiDocExecutorSubscriber.getType());
    }

    @Test
    void testExecutorWithEmptyList() {
        List<ApiDocRegisterDTO> list = new ArrayList<>();
        apiDocExecutorSubscriber.executor(list);
        assertTrue(list.isEmpty());
    }

    @Test
    void testExecutorWithValidData() {
        List<ApiDocRegisterDTO> list = new ArrayList<>();
        ApiDocRegisterDTO apiDoc = ApiDocRegisterDTO.builder()
                .contextPath("/test")
                .apiPath("/api/test")
                .httpMethod(0)
                .rpcType("http")
                .build();
        list.add(apiDoc);

        ShenyuClientRegisterService service = mock(ShenyuClientRegisterService.class);
        when(shenyuClientRegisterService.get("http")).thenReturn(service);

        apiDocExecutorSubscriber.executor(list);

        verify(service, times(1)).registerApiDoc(any(ApiDocRegisterDTO.class));
    }

    @Test
    void testExecutorWithMultipleData() {
        List<ApiDocRegisterDTO> list = new ArrayList<>();
        ApiDocRegisterDTO apiDoc1 = ApiDocRegisterDTO.builder()
                .contextPath("/test1")
                .apiPath("/api/test1")
                .httpMethod(0)
                .rpcType("http")
                .build();
        ApiDocRegisterDTO apiDoc2 = ApiDocRegisterDTO.builder()
                .contextPath("/test2")
                .apiPath("/api/test2")
                .httpMethod(2)
                .rpcType("http")
                .build();
        list.add(apiDoc1);
        list.add(apiDoc2);

        ShenyuClientRegisterService service = mock(ShenyuClientRegisterService.class);
        when(shenyuClientRegisterService.get("http")).thenReturn(service);

        apiDocExecutorSubscriber.executor(list);

        verify(service, times(2)).registerApiDoc(any(ApiDocRegisterDTO.class));
    }

    @Test
    void testExecutorWithNullService() {
        List<ApiDocRegisterDTO> list = new ArrayList<>();
        ApiDocRegisterDTO apiDoc = ApiDocRegisterDTO.builder()
                .contextPath("/test")
                .apiPath("/api/test")
                .httpMethod(0)
                .rpcType("http")
                .build();
        list.add(apiDoc);

        when(shenyuClientRegisterService.get("http")).thenReturn(null);

        apiDocExecutorSubscriber.executor(list);

        verify(shenyuClientRegisterService, times(1)).get("http");
    }

    @Test
    void testExecutorWithDifferentRpcTypes() {
        List<ApiDocRegisterDTO> list = new ArrayList<>();
        ApiDocRegisterDTO httpDoc = ApiDocRegisterDTO.builder()
                .contextPath("/test1")
                .apiPath("/api/test1")
                .httpMethod(0)
                .rpcType("http")
                .build();
        ApiDocRegisterDTO dubboDoc = ApiDocRegisterDTO.builder()
                .contextPath("/test2")
                .apiPath("/api/test2")
                .httpMethod(2)
                .rpcType("dubbo")
                .build();
        list.add(httpDoc);
        list.add(dubboDoc);

        ShenyuClientRegisterService httpService = mock(ShenyuClientRegisterService.class);
        ShenyuClientRegisterService dubboService = mock(ShenyuClientRegisterService.class);
        when(shenyuClientRegisterService.get("http")).thenReturn(httpService);
        when(shenyuClientRegisterService.get("dubbo")).thenReturn(dubboService);

        apiDocExecutorSubscriber.executor(list);

        verify(httpService, times(1)).registerApiDoc(any(ApiDocRegisterDTO.class));
        verify(dubboService, times(1)).registerApiDoc(any(ApiDocRegisterDTO.class));
    }

    @Test
    void testExecutorWithUnknownRpcType() {
        List<ApiDocRegisterDTO> list = new ArrayList<>();
        ApiDocRegisterDTO apiDoc = ApiDocRegisterDTO.builder()
                .contextPath("/test")
                .apiPath("/api/test")
                .httpMethod(0)
                .rpcType("unknown")
                .build();
        list.add(apiDoc);

        when(shenyuClientRegisterService.get("unknown")).thenReturn(null);

        apiDocExecutorSubscriber.executor(list);

        verify(shenyuClientRegisterService, times(1)).get("unknown");
    }
}
