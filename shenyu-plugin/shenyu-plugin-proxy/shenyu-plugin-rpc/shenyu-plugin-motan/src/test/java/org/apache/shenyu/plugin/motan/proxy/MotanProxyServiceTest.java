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

package org.apache.shenyu.plugin.motan.proxy;

import com.weibo.api.motan.config.RefererConfig;
import com.weibo.api.motan.proxy.CommonClient;
import com.weibo.api.motan.rpc.Request;
import com.weibo.api.motan.rpc.ResponseFuture;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.MotanRegisterConfig;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.motan.cache.ApplicationConfigCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * MotanProxyServiceTest.
 */
@ExtendWith(MockitoExtension.class)
public class MotanProxyServiceTest {

    private MetaData metaData;

    private ServerWebExchange exchange;

    @BeforeEach
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath("/motan/findAll");
        metaData.setServiceName("org.apache.shenyu.test.motan.api.service.MotanTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.MOTAN.getName());
        metaData.setRpcExt("{\"loadbalance\": \"loadbalance\"}");
    }

    @AfterEach
    public void after() {
        ApplicationConfigCache.getInstance().invalidateAll();
    }

    @Test
    @SuppressWarnings("all")
    public void testGenericInvoker() {

        ApplicationConfigCache.getInstance().init(new MotanRegisterConfig());
        SelectorData selectorData = mock(SelectorData.class);

        RefererConfig<CommonClient> reference = mock(RefererConfig.class);
        CommonClient commonClient = mock(CommonClient.class);
        when(reference.getRef()).thenReturn(commonClient);
        when(reference.getServiceInterface()).thenReturn("org.apache.shenyu.test.motan.api.service.MotanTestService");

        MotanProxyService motanProxyService = spy(new MotanProxyService());
        doReturn(reference).when(motanProxyService).getConsumerConfig(selectorData, metaData);

        ResponseFuture responseFuture = mock(ResponseFuture.class);
        try {
            when(commonClient.asyncCall(any(Request.class), eq(Object.class))).thenReturn(responseFuture);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }

        motanProxyService.genericInvoker("", metaData, exchange, selectorData);
    }

}
