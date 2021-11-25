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
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ShenyuClientRegisterTarsServiceImpl}.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public class ShenyuClientRegisterTarsServiceImplTest {

    @InjectMocks
    private ShenyuClientRegisterTarsServiceImpl shenyuClientRegisterTarsService;

    @Mock
    private MetaDataServiceImpl metaDataService;

    @Test
    public void testRpcType() {
        Assert.assertEquals(RpcTypeEnum.TARS.getName(), shenyuClientRegisterTarsService.rpcType());
    }

    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        Assert.assertEquals(StringUtils.EMPTY, shenyuClientRegisterTarsService.selectorHandler(metaDataRegisterDTO));
    }

    @Test
    public void testRuleHandler() {
        Assert.assertEquals(StringUtils.EMPTY, shenyuClientRegisterTarsService.ruleHandler());
    }

    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByServiceNameAndMethodName(any(), any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().serviceName("metaDataService").
                methodName("registerMetadata").build();
        shenyuClientRegisterTarsService.registerMetadata(metaDataDTO);
        verify(metaDataService).findByServiceNameAndMethodName("metaDataService", "registerMetadata");
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }

    @Test
    public void testBuildHandle() {
        shenyuClientRegisterTarsService = spy(shenyuClientRegisterTarsService);

        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType("http").host("localhost").port(8090).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn("[{upstreamUrl='localhost:8090',weight=1,warmup=10,status=true,timestamp=1637826588267}," +
                "{upstreamUrl='localhost:8091',weight=2,warmup=10,status=true,timestamp=1637826588267}]");
        doNothing().when(shenyuClientRegisterTarsService).doSubmit(any(), any());
        String handler = shenyuClientRegisterTarsService.buildHandle(list, selectorDO);
        String result = "[{\"weight\":1,\"warmup\":10,\"upstreamUrl\":\"localhost:8090\",\"status\":true,\"timestamp\":1637826588267}," +
                "{\"weight\":2,\"warmup\":10,\"upstreamUrl\":\"localhost:8091\",\"status\":true,\"timestamp\":1637826588267}]";
        assertEquals(handler, result);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromCurrentList(handler, TarsUpstream.class);
        assertEquals(resultList.size(), 2);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType("http").host("localhost").port(8092).build());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn("[{upstreamUrl='localhost:8090',weight=1,warmup=10,status=true,timestamp=1637826588267}," +
                "{upstreamUrl='localhost:8091',weight=2,warmup=10,status=true,timestamp=1637826588267}]");
        doNothing().when(shenyuClientRegisterTarsService).doSubmit(any(), any());
        handler = shenyuClientRegisterTarsService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(handler, TarsUpstream.class);
        assertEquals(resultList.size(), 3);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType("http").host("localhost").port(8090).build());
        doNothing().when(shenyuClientRegisterTarsService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        handler = shenyuClientRegisterTarsService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(handler, TarsUpstream.class);
        assertEquals(resultList.size(), 1);
    }

    @Test
    public void testBuildTarsUpstreamList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType("http").host("localhost").port(8090).build());
        list.add(URIRegisterDTO.builder().appName("test1").rpcType("http").host("localhost").port(8091).build());
        try {
            Method testMethod = shenyuClientRegisterTarsService.getClass().getDeclaredMethod("buildTarsUpstreamList", List.class);
            testMethod.setAccessible(true);
            List<TarsUpstream> result = (List<TarsUpstream>) testMethod.invoke(shenyuClientRegisterTarsService, list);
            assertEquals(result.size(), 2);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

}
