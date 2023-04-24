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

import com.google.gson.internal.LinkedTreeMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.enums.RpcTypeEnum;
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

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;

/**
 * Test cases for ShenyuClientRegisterWebSocketServiceImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ShenyuClientRegisterWebSocketServiceImplTest {

    @InjectMocks
    private ShenyuClientRegisterWebSocketServiceImpl shenyuClientRegisterWebSocketService;

    @Mock
    private MetaDataServiceImpl metaDataService;

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.WEB_SOCKET.getName(), shenyuClientRegisterWebSocketService.rpcType());
    }

    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterWebSocketService.selectorHandler(metaDataRegisterDTO));
    }

    @Test
    public void testRuleHandler() {
        assertEquals(new WebSocketRuleHandle().toJson(), shenyuClientRegisterWebSocketService.ruleHandler());
    }

    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByPath(any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().registerMetaData(true).build();
        shenyuClientRegisterWebSocketService.registerMetadata(metaDataDTO);
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }

    @Test
    public void testBuildHandle() {
        URIRegisterDTO dto = new URIRegisterDTO();
        dto.setPort(8080);
        dto.setHost("host");
        dto.setProtocol("http");
        List<URIRegisterDTO> uriList = Collections.singletonList(dto);
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(upstreamCheckService.checkAndSubmit(any(), any())).thenReturn(true);
        WebSocketUpstream.builder().host("localhost").protocol("http").upstreamUrl("host:8080").weight(50).warmup(10)
                .status(true).build();
        List result = GsonUtils.getInstance().fromJson(shenyuClientRegisterWebSocketService.buildHandle(uriList, selectorDO), List.class);
        LinkedTreeMap<String, Object> webSocketUpstreamResult = (LinkedTreeMap<String, Object>) result.get(0);
        assertEquals(webSocketUpstreamResult.get("host"), "localhost");
        assertEquals(webSocketUpstreamResult.get("url"), "host:8080");
        assertEquals(webSocketUpstreamResult.get("weight"), 50.0);
        assertEquals(webSocketUpstreamResult.get("warmup"), 10.0);
        assertEquals(webSocketUpstreamResult.get("upstreamHost"), "localhost");
        assertEquals(webSocketUpstreamResult.get("status"), true);

        WebSocketUpstream webSocketUpstreamnew = WebSocketUpstream.builder().host("localhost").protocol("http").upstreamUrl("host:8090").weight(50).warmup(10)
                .status(true).build();
        SelectorDO selectorDO1 = new SelectorDO();
        selectorDO1.setHandle(GsonUtils.getGson().toJson(Collections.singletonList(webSocketUpstreamnew)));
        result = GsonUtils.getInstance().fromJson(shenyuClientRegisterWebSocketService.buildHandle(uriList, selectorDO1), List.class);
        webSocketUpstreamResult = (LinkedTreeMap<String, Object>) result.get(0);
        assertEquals(webSocketUpstreamResult.get("host"), "localhost");
        assertEquals(webSocketUpstreamResult.get("url"), "host:8090");
        assertEquals(webSocketUpstreamResult.get("weight"), 50.0);
        assertEquals(webSocketUpstreamResult.get("warmup"), 10.0);
        assertEquals(webSocketUpstreamResult.get("upstreamHost"), "localhost");
        assertEquals(webSocketUpstreamResult.get("status"), true);
        webSocketUpstreamResult = (LinkedTreeMap<String, Object>) result.get(1);
        assertEquals(webSocketUpstreamResult.get("host"), "localhost");
        assertEquals(webSocketUpstreamResult.get("url"), "host:8080");
        assertEquals(webSocketUpstreamResult.get("weight"), 50.0);
        assertEquals(webSocketUpstreamResult.get("warmup"), 10.0);
        assertEquals(webSocketUpstreamResult.get("upstreamHost"), "localhost");
        assertEquals(webSocketUpstreamResult.get("status"), true);
    }
}
