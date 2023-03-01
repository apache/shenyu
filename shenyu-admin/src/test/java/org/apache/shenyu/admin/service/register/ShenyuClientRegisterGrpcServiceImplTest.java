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
    
package org.apache.shenyu.admin.service.register;
    
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.converter.GrpcSelectorHandleConverter;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;
import com.google.gson.JsonParser;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
    
/**
 * Test cases for {@link ShenyuClientRegisterGrpcServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClientRegisterGrpcServiceImplTest {
    
    @InjectMocks
    private ShenyuClientRegisterGrpcServiceImpl shenyuClientRegisterGrpcService;
    
    @Mock
    private MetaDataServiceImpl metaDataService;

    @BeforeEach
    public void setUp() {
        GrpcSelectorHandleConverter grpcSelectorHandleConverter = new GrpcSelectorHandleConverter();
        ReflectionTestUtils.setField(shenyuClientRegisterGrpcService, "grpcSelectorHandleConverter", grpcSelectorHandleConverter);
    }

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.GRPC.getName(), shenyuClientRegisterGrpcService.rpcType());
    }
    
    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterGrpcService.selectorHandler(metaDataRegisterDTO));
    }
    
    @Test
    public void testRuleHandler() {
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterGrpcService.ruleHandler());
    }
    
    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByPath(any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().path("/test").build();
        shenyuClientRegisterGrpcService.registerMetadata(metaDataDTO);
        verify(metaDataService).findByPath("/test");
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }
    
    @Test
    public void testBuildHandle() {
        shenyuClientRegisterGrpcService = spy(shenyuClientRegisterGrpcService);
        
        final String returnStr = "[{upstreamUrl='localhost:8090',weight=1,status=true,timestamp=1637826588267},"
                + "{upstreamUrl='localhost:8091',weight=2,status=true,timestamp=1637826588267}]";
        final String expected = "[{\"weight\":1,\"upstreamUrl\":\"localhost:8090\",\"status\":true,\"timestamp\":1637826588267},"
                + "{\"weight\":2,\"upstreamUrl\":\"localhost:8091\",\"status\":true,\"timestamp\":1637826588267}]";
    
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8090).build());
        list.add(URIRegisterDTO.builder().appName("test2").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8091).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        String actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        assertEquals(JsonParser.parseString(expected), JsonParser.parseString(actual));
        List<GrpcUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, GrpcUpstream.class);
        assertEquals(resultList.size(), 2);
    
        //list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8092).build());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn("");
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, GrpcUpstream.class);
        assertEquals(resultList.size(), 3);
        assertEquals(resultList.stream().anyMatch(CommonUpstream::isStatus), true);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8090).build());
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, GrpcUpstream.class);
        assertEquals(resultList.size(), 2);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(CommonUpstream::isStatus), true);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8090).eventType(EventType.DELETED).build());
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, GrpcUpstream.class);
        assertEquals(resultList.size(), 2);
        assertEquals(resultList.stream().anyMatch(r -> !r.isStatus() && r.getUpstreamUrl().equals("localhost:8090")), true);
        assertEquals(resultList.stream().anyMatch(r -> r.isStatus() && !r.getUpstreamUrl().equals("localhost:8090")), true);

        list.clear();
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, GrpcUpstream.class);
        assertEquals(resultList.stream().allMatch(r -> !r.isStatus()), true);
    }
    
    @Test
    public void testBuildGrpcUpstreamList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8090).build());
        list.add(URIRegisterDTO.builder().appName("test2").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8091).build());
        try {
            Method testMethod = shenyuClientRegisterGrpcService.getClass().getDeclaredMethod("buildGrpcUpstreamList", List.class);
            testMethod.setAccessible(true);
            List<TarsUpstream> result = (List<TarsUpstream>) testMethod.invoke(shenyuClientRegisterGrpcService, list);
            assertEquals(result.size(), 2);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }
}
