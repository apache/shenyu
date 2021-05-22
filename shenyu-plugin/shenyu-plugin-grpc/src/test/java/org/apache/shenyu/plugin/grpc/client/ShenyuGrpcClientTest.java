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

package org.apache.shenyu.plugin.grpc.client;

import io.grpc.CallOptions;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.concurrent.TimeUnit;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link ShenyuGrpcClient}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ShenyuGrpcClientTest {

    private ShenyuGrpcClient shenyuGrpcClient;

    private MetaData metaData;

    @Before
    public void setUp() {
        metaData = new MetaData();
        metaData.setId("1332017977771636096");
        metaData.setAppName("grpc");
        metaData.setContextPath("/grpc");
        metaData.setPath("/grpc/echo");
        metaData.setServiceName("echo.EchoService");
        metaData.setMethodName("echo");
        metaData.setRpcType(RpcTypeEnum.GRPC.getName());
        metaData.setRpcExt("{timeout:5000}");
        metaData.setEnabled(true);
        SelectorData selector = mock(SelectorData.class);
        when(selector.getName()).thenReturn("/grpc");
        when(selector.getHandle()).thenReturn("[{\"upstreamUrl\":\"localhost:8080\",\"weight\":50,\"status\":true}]");
        ApplicationConfigCache.getInstance().initPrx(selector);
        GrpcClientCache.initGrpcClient(selector.getName());

        shenyuGrpcClient = GrpcClientCache.getGrpcClient(selector.getName());
    }

    @Test(expected = RuntimeException.class)
    public void call() {
        CallOptions callOptions = CallOptions.DEFAULT.withDeadlineAfter(5000, TimeUnit.MILLISECONDS);;
        String requestJsons = "{'message':'1'}";
        shenyuGrpcClient.call(metaData, callOptions, requestJsons);
    }
}
