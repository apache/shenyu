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

import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.converter.SpringCloudSelectorHandleConverter;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.RpcTypeEnum;
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
 * Test cases for {@link ShenyuClientRegisterSpringCloudServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClientRegisterSpringCloudServiceImplTest {
    
    public static final String HOST = "localhost";
    
    @InjectMocks
    private ShenyuClientRegisterSpringCloudServiceImpl shenyuClientRegisterSpringCloudService;
    
    @Mock
    private MetaDataServiceImpl metaDataService;

    @BeforeEach
    public void setUp() {
        SpringCloudSelectorHandleConverter springCloudSelectorHandleConverter = new SpringCloudSelectorHandleConverter();
        ReflectionTestUtils.setField(shenyuClientRegisterSpringCloudService, "springCloudSelectorHandleConverter", springCloudSelectorHandleConverter);
    }

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.SPRING_CLOUD.getName(), shenyuClientRegisterSpringCloudService.rpcType());
    }
    
    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().appName("testSelectorHandler").build();
        assertEquals("{\"serviceId\":\"testSelectorHandler\",\"gray\":false}", 
                shenyuClientRegisterSpringCloudService.selectorHandler(metaDataRegisterDTO));
    }
    
    @Test
    public void testRuleHandler() {
        assertEquals(new SpringCloudRuleHandle().toJson(), shenyuClientRegisterSpringCloudService.ruleHandler());
    }
    
    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByPath(any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().path("/contextPath/test").build();
        shenyuClientRegisterSpringCloudService.registerMetadata(metaDataDTO);
        verify(metaDataService).findByPath("/contextPath/test");
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }
    
    @Test
    public void testBuildHandle() {
        shenyuClientRegisterSpringCloudService = spy(shenyuClientRegisterSpringCloudService);
        
        final String returnStr = "{serviceId:'test1',gray:false,divideUpstreams:["
                + "{weight:50,warmup:10,protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8090',status:'true',timestamp:1637909490935},"
                + "{weight:50,warmup:10,protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8091',status:'true',timestamp:1637909490935}]}";
        final String expected = "{\"serviceId\":\"test1\",\"gray\":false,\"divideUpstreams\":["
                + "{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8090\",\"status\":true,\"timestamp\":1637909490935},"
                + "{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8091\",\"status\":false,\"timestamp\":1637909490935}]}";
        final URIRegisterDTO dto1 = URIRegisterDTO.builder().appName("test2").rpcType(RpcTypeEnum.SPRING_CLOUD.getName()).host(HOST).port(8090).build();
        final URIRegisterDTO dto2 = URIRegisterDTO.builder().appName("test2").rpcType(RpcTypeEnum.SPRING_CLOUD.getName()).host(HOST).port(8091).build();
        
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(dto1);
        SelectorDO selectorDO = mock(SelectorDO.class);
        doReturn(false).when(shenyuClientRegisterSpringCloudService).doSubmit(any(), any());
        when(selectorDO.getHandle()).thenReturn(returnStr);
        String actual = shenyuClientRegisterSpringCloudService.buildHandle(list, selectorDO);
        assertEquals(JsonParser.parseString(expected.replaceAll("\\d{13}", "0")), JsonParser.parseString(actual.replaceAll("\\d{13}", "0")));
        SpringCloudSelectorHandle handle = GsonUtils.getInstance().fromJson(actual, SpringCloudSelectorHandle.class);
        assertEquals(handle.getDivideUpstreams().size(), 2);
        assertEquals(handle.getDivideUpstreams().stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(CommonUpstream::isStatus), true);
        assertEquals(handle.getDivideUpstreams().stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(dto1);
        list.add(dto2);
        selectorDO = mock(SelectorDO.class);
        doReturn(false).when(shenyuClientRegisterSpringCloudService).doSubmit(any(), any());
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterSpringCloudService.buildHandle(list, selectorDO);
        handle = GsonUtils.getInstance().fromJson(actual, SpringCloudSelectorHandle.class);
        assertEquals(handle.getDivideUpstreams().size(), 2);
        assertEquals(handle.getDivideUpstreams().stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(CommonUpstream::isStatus), true);
        assertEquals(handle.getDivideUpstreams().stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(dto1);
        selectorDO = mock(SelectorDO.class);
        doReturn(false).when(shenyuClientRegisterSpringCloudService).doSubmit(any(), any());
        when(selectorDO.getHandle()).thenReturn("{serviceId:'test1',gray:false,divideUpstreams:[]}");
        actual = shenyuClientRegisterSpringCloudService.buildHandle(list, selectorDO);
        handle = GsonUtils.getInstance().fromJson(actual, SpringCloudSelectorHandle.class);
        assertEquals(handle.getDivideUpstreams().size(), 1);
        assertEquals(handle.getDivideUpstreams().stream().anyMatch(r -> r.isStatus() && r.getUpstreamUrl().equals(CommonUpstreamUtils.buildUrl(dto1.getHost(), dto1.getPort()))), true);

        list.clear();
        dto1.setEventType(EventType.DELETED);
        list.add(dto1);
        selectorDO = mock(SelectorDO.class);
        doReturn(false).when(shenyuClientRegisterSpringCloudService).doSubmit(any(), any());
        when(selectorDO.getHandle()).thenReturn("{serviceId:'test1',gray:false,divideUpstreams:[]}");
        actual = shenyuClientRegisterSpringCloudService.buildHandle(list, selectorDO);
        handle = GsonUtils.getInstance().fromJson(actual, SpringCloudSelectorHandle.class);
        assertEquals(handle.getDivideUpstreams().size(), 1);
        assertEquals(handle.getDivideUpstreams().stream().anyMatch(r -> !r.isStatus() && r.getUpstreamUrl().equals(CommonUpstreamUtils.buildUrl(dto1.getHost(), dto1.getPort()))), true);

        list.clear();
        doReturn(false).when(shenyuClientRegisterSpringCloudService).doSubmit(any(), any());
        actual = shenyuClientRegisterSpringCloudService.buildHandle(list, selectorDO);
        handle = GsonUtils.getInstance().fromJson(actual, SpringCloudSelectorHandle.class);
        assertEquals(handle.getDivideUpstreams().stream().allMatch(r -> !r.isStatus()), true);
    }
}
