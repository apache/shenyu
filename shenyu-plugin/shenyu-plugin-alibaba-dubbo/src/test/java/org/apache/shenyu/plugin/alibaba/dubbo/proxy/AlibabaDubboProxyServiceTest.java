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
import com.alibaba.dubbo.rpc.service.GenericService;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import org.apache.shenyu.plugin.alibaba.dubbo.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.api.param.BodyParamResolveService;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * AlibabaDubboProxyServiceTest.
 */
@RunWith(MockitoJUnitRunner.class)
public final class AlibabaDubboProxyServiceTest {
    private static final String PATH = "/sofa/findAll";

    private static final String METHOD_NAME = "findAll";

    private static final String[] LEFT = new String[]{};

    private static final Object[] RIGHT = new Object[]{};

    private MetaData metaData;

    @Before
    public void setup() {
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath(PATH);
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName(METHOD_NAME);
        metaData.setRpcType(RpcTypeEnum.SOFA.getName());
    }

    @After
    public void after() {
        ApplicationConfigCache.getInstance().invalidateAll();
    }

    @Test
    public void testGenericInvoker() {
        ReferenceConfig referenceConfig = mock(ReferenceConfig.class);
        GenericService genericService = mock(GenericService.class);
        when(referenceConfig.get()).thenReturn(genericService);
        when(genericService.$invoke(METHOD_NAME, LEFT, RIGHT)).thenReturn(null);
        try (MockedStatic<ApplicationConfigCache> applicationConfigCacheMockedStatic = mockStatic(ApplicationConfigCache.class)) {
            ApplicationConfigCache applicationConfigCache = mock(ApplicationConfigCache.class);
            applicationConfigCacheMockedStatic.when(() -> ApplicationConfigCache.getInstance()).thenReturn(applicationConfigCache);
            when(applicationConfigCache.initRef(metaData)).thenReturn(referenceConfig);

            AlibabaDubboProxyService alibabaDubboProxyService = new AlibabaDubboProxyService(new BodyParamResolveServiceImpl());
            Assert.assertNull(alibabaDubboProxyService.genericInvoker("", metaData));
        }
    }

    class BodyParamResolveServiceImpl implements BodyParamResolveService {

        @Override
        public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
            return new ImmutablePair<>(LEFT, RIGHT);
        }
    }
}
