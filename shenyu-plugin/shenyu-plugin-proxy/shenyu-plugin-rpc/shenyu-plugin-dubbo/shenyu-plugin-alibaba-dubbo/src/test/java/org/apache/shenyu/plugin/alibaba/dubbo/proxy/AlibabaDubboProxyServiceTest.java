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

package org.apache.shenyu.plugin.alibaba.dubbo.proxy;

import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.remoting.exchange.ResponseFuture;
import com.alibaba.dubbo.remoting.exchange.support.SimpleFuture;
import com.alibaba.dubbo.rpc.RpcContext;
import com.alibaba.dubbo.rpc.RpcResult;
import com.alibaba.dubbo.rpc.protocol.dubbo.FutureAdapter;
import com.alibaba.dubbo.rpc.service.GenericService;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.alibaba.dubbo.cache.AlibabaDubboConfigCache;
import org.apache.shenyu.plugin.dubbo.common.param.DubboParamResolveService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * AlibabaDubboProxyServiceTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AlibabaDubboProxyServiceTest {
    private static final String PATH = "/sofa/findAll";

    private static final String METHOD_NAME = "findAll";

    private static final String[] LEFT = new String[]{};

    private static final Object[] RIGHT = new Object[]{};

    private MetaData metaData;

    @Mock
    private ReferenceConfig<GenericService> referenceConfig;

    @BeforeEach
    public void setup() {
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath(PATH);
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName(METHOD_NAME);
        metaData.setRpcType(RpcTypeEnum.SOFA.getName());
    }

    @AfterEach
    public void after() {
        AlibabaDubboConfigCache.getInstance().invalidateAll();
    }

    @Test
    public void testGenericInvoker() throws Exception {
        GenericService genericService = mock(GenericService.class);
        String sample = String.format("%x", System.nanoTime());
        when(referenceConfig.get()).thenReturn(genericService);
        when(genericService.$invoke(METHOD_NAME, LEFT, RIGHT))
                .then(invocationOnMock -> {
                    RpcContext.getContext().setFuture(new FutureAdapter<>(new SimpleFuture(new RpcResult(sample))));
                    return sample;
                });
        try (MockedStatic<AlibabaDubboConfigCache> ignored = mockStatic(AlibabaDubboConfigCache.class)) {
            AlibabaDubboConfigCache alibabaDubboConfigCache = mock(AlibabaDubboConfigCache.class);
            when(AlibabaDubboConfigCache.getInstance()).thenReturn(alibabaDubboConfigCache);
            when(alibabaDubboConfigCache.initRef(metaData)).thenReturn(referenceConfig);

            AlibabaDubboProxyService alibabaDubboProxyService = new AlibabaDubboProxyService(new BodyParamResolveServiceImpl());

            ResponseFuture responseFuture = alibabaDubboProxyService.genericInvoker("", metaData);
            assertNotNull(responseFuture);
            assertEquals(sample, RpcContext.getContext().getFuture().get());
        }
    }

    static class BodyParamResolveServiceImpl implements DubboParamResolveService {

        @Override
        public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
            return new ImmutablePair<>(LEFT, RIGHT);
        }
    }
}
