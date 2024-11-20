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
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
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
        when(metaDataService.findByPathAndNamespaceId(any(), any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO
                .builder()
                .path("/test")
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .build();
        shenyuClientRegisterGrpcService.registerMetadata(metaDataDTO);
        verify(metaDataService).findByPathAndNamespaceId("/test", SYS_DEFAULT_NAMESPACE_ID);
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }

    @Test
    public void testBuildHandle() {
        shenyuClientRegisterGrpcService = spy(shenyuClientRegisterGrpcService);

        final String returnStr = "[{upstreamUrl='localhost:8090',weight=1,status=true,timestamp=1637826588267,\"gray\":false},"
                + "{upstreamUrl='localhost:8091',weight=2,status=true,timestamp=1637826588267,\"gray\":false}]";
        final String expected = "[{\"weight\":1,\"upstreamUrl\":\"localhost:8090\",\"status\":true,\"timestamp\":1637826588267,\"gray\":false},"
                + "{\"weight\":2,\"upstreamUrl\":\"localhost:8091\",\"status\":true,\"timestamp\":1637826588267,\"gray\":false}]";

        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8090).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        String actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);

        assertEquals(actual, expected);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(resultList.size(), 2);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8092).build());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(resultList.size(), 3);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.GRPC.getName()).host("localhost").port(8090).build());
        doReturn(false).when(shenyuClientRegisterGrpcService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        actual = shenyuClientRegisterGrpcService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(resultList.size(), 1);
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
