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
import org.apache.shenyu.admin.service.converter.DivideSelectorHandleConverter;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
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
 * Test cases for {@link ShenyuClientRegisterDivideServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClientRegisterDivideServiceImplTest {
    
    public static final String LOCALHOST = "localhost";
    
    @InjectMocks
    private ShenyuClientRegisterDivideServiceImpl shenyuClientRegisterDivideService;
    
    @Mock
    private MetaDataServiceImpl metaDataService;

    @BeforeEach
    public void setUp() {
        DivideSelectorHandleConverter divideSelectorHandleConverter = new DivideSelectorHandleConverter();
        ReflectionTestUtils.setField(shenyuClientRegisterDivideService, "divideSelectorHandleConverter", divideSelectorHandleConverter);
    }

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.HTTP.getName(), shenyuClientRegisterDivideService.rpcType());
    }
    
    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterDivideService.selectorHandler(metaDataRegisterDTO));
    }
    
    @Test
    public void testRuleHandler() {
        assertEquals(new DivideRuleHandle().toJson(), shenyuClientRegisterDivideService.ruleHandler());
    }
    
    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByPath(any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().registerMetaData(true).build();
        shenyuClientRegisterDivideService.registerMetadata(metaDataDTO);
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }
    
    @Test
    public void testBuildHandle() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);
        
        final String returnStr = "[{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8090',warmup:10,weight:50,status:false,timestamp:1637826588267},"
                + "{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8091',warmup:10,weight:50,status:true,timestamp:1637826588267},"
                + "{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8092',warmup:10,weight:50,status:false,timestamp:" + (System.currentTimeMillis() + 60000) + "}]";
        final String expected = "[{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8090\",\"status\":true,\"timestamp\":1637826588267},"
                + "{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8091\",\"status\":true,\"timestamp\":1637826588267},"
                + "{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8092\",\"status\":false,\"timestamp\":1637826588267}]";
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8090).build());
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8091).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        String actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        assertEquals(JsonParser.parseString(expected.replaceAll("\\d{13}", "0")), JsonParser.parseString(actual.replaceAll("\\d{13}", "0")));
        List<DivideUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        assertEquals(resultList.size(), 3);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(CommonUpstream::isStatus), true);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8093).build());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        //localhost:8090 was removed because the stop time was too long
        assertEquals(resultList.size(), 3);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .anyMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(CommonUpstream::isStatus), true);
        assertEquals(resultList.stream().filter(r -> list.stream().map(dto -> CommonUpstreamUtils.buildUrl(dto.getHost(), dto.getPort()))
                .noneMatch(url -> url.equals(r.getUpstreamUrl()))).allMatch(r -> !r.isStatus()), true);

        list.clear();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8091)/*.eventType(EventType.DELETED)*/.build());
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        assertEquals(resultList.size(), 2);
        assertEquals(resultList.stream().anyMatch(r -> !r.isStatus() /*&& r.getUpstreamUrl().equals("localhost:8091")*/), true);

        list.clear();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8090).eventType(EventType.DELETED).build());
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        assertEquals(resultList.size(), 2);
        assertEquals(resultList.stream().anyMatch(r -> r.isStatus() && r.getUpstreamUrl().equals("localhost:8091")), true);
        assertEquals(resultList.stream().anyMatch(r -> !r.isStatus() && r.getUpstreamUrl().equals("localhost:8092")), true);

        doReturn(true).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        assertEquals(actual, null);

        list.clear();
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        assertEquals(resultList.stream().allMatch(r -> !r.isStatus()), true);
    }
    
    @Test
    public void testBuildDivideUpstreamList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8090).build());
        list.add(URIRegisterDTO.builder().appName("test2").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8091).build());
        try {
            Method testMethod = shenyuClientRegisterDivideService.getClass().getDeclaredMethod("buildDivideUpstreamList", List.class);
            testMethod.setAccessible(true);
            List<DivideUpstream> result = (List<DivideUpstream>) testMethod.invoke(shenyuClientRegisterDivideService, list);
            assertEquals(result.size(), 2);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }
}
