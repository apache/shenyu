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
import org.apache.shenyu.admin.service.converter.DubboSelectorHandleConverter;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
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
 * Test cases for {@link ShenyuClientRegisterDubboServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClientRegisterDubboServiceImplTest {
    
    public static final String LOCALHOST = "localhost";
    
    @InjectMocks
    private ShenyuClientRegisterDubboServiceImpl shenyuClientRegisterDubboService;
    
    @Mock
    private MetaDataServiceImpl metaDataService;

    @BeforeEach
    public void setUp() {
        DubboSelectorHandleConverter dubboSelectorHandleConverter = new DubboSelectorHandleConverter();
        ReflectionTestUtils.setField(shenyuClientRegisterDubboService, "dubboSelectorHandleConverter", dubboSelectorHandleConverter);
    }

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.DUBBO.getName(), shenyuClientRegisterDubboService.rpcType());
    }
    
    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterDubboService.selectorHandler(metaDataRegisterDTO));
    }
    
    @Test
    public void testRuleHandler() {
        assertEquals(new DubboRuleHandle().toJson(), shenyuClientRegisterDubboService.ruleHandler());
    }
    
    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByPath(any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().registerMetaData(true).build();
        shenyuClientRegisterDubboService.registerMetadata(metaDataDTO);
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }
    
    @Test
    public void testBuildHandle() {
        shenyuClientRegisterDubboService = spy(shenyuClientRegisterDubboService);
    
        final String returnStr = "[{protocol:'dubbo://',upstreamHost:'localhost',upstreamUrl:'localhost:8090',warmup:10,weight:50,status:true,timestamp:1637826588267},"
                + "{protocol:'dubbo://',upstreamHost:'localhost',upstreamUrl:'localhost:8091',warmup:10,weight:50,status:false,timestamp:" + (System.currentTimeMillis() + 60000) + "}]";
        final String expected = "[{\"port\":0,\"weight\":50,\"warmup\":10,\"protocol\":\"dubbo://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8090\","
                + "\"status\":true,\"timestamp\":1637826588267},{\"port\":0,\"weight\":50,\"warmup\":10,\"protocol\":\"dubbo://\",\"upstreamHost\":\"localhost\","
                + "\"upstreamUrl\":\"localhost:8091\",\"status\":false,\"timestamp\":1637826588267}]";
        
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.DUBBO.getName()).host(LOCALHOST).port(8090).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDubboService).doSubmit(any(), any());
        String actual = shenyuClientRegisterDubboService.buildHandle(list, selectorDO);
        assertEquals(expected.replaceAll("\\d{13}", "0"), actual.replaceAll("\\d{13}", "0"));
        List<DubboUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, DubboUpstream.class);
        assertEquals(resultList.size(), 2);

        //list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.DUBBO.getName()).host(LOCALHOST).port(8092).build());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDubboService).doSubmit(any(), any());
        actual = shenyuClientRegisterDubboService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DubboUpstream.class);
        assertEquals(resultList.size(), 3);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> r.isStatus()), true);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.DUBBO.getName()).host(LOCALHOST).port(8091).build());
        doReturn(false).when(shenyuClientRegisterDubboService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterDubboService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DubboUpstream.class);
        assertEquals(resultList.size(), 2);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> r.isStatus()), true);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.DUBBO.getName()).host(LOCALHOST).port(8090).eventType(EventType.DELETED).build());
        doReturn(false).when(shenyuClientRegisterDubboService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterDubboService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DubboUpstream.class);
        assertEquals(resultList.size(), 2);
        assertEquals(resultList.stream().anyMatch(r -> !r.isStatus()), true);

        list.clear();
        doReturn(false).when(shenyuClientRegisterDubboService).doSubmit(any(), any());
        actual = shenyuClientRegisterDubboService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DubboUpstream.class);
        assertEquals(resultList.stream().allMatch(r -> !r.isStatus()), true);
    }

    @Test
    public void testBuildDivideUpstreamList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1")
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .host(LOCALHOST).port(8090).build());
        list.add(URIRegisterDTO.builder().appName("test2")
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .host(LOCALHOST).port(8091).build());
        try {
            Method testMethod = shenyuClientRegisterDubboService.getClass().getDeclaredMethod("buildDubboUpstreamList", List.class);
            testMethod.setAccessible(true);
            List<DivideUpstream> result = (List<DivideUpstream>) testMethod.invoke(shenyuClientRegisterDubboService, list);
            assertEquals(result.size(), 2);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }
}
