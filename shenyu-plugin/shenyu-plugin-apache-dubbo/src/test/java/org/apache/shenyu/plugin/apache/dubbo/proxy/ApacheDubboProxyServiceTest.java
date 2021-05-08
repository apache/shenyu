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

package org.apache.shenyu.plugin.apache.dubbo.proxy;

import com.google.common.cache.LoadingCache;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.dubbo.common.URL;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.service.GenericService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.api.param.BodyParamResolveService;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.CompletableFuture;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The Test Case For ApacheDubboProxyService.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApacheDubboProxyServiceTest {
    private static final String PATH = "/duubo/findAll";

    private static final String METHOD_NAME = "findAll";

    private static final String[] LEFT = new String[]{};

    private static final Object[] RIGHT = new Object[]{};

    private MetaData metaData;

    private ServerWebExchange exchange;

    @Before
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath(PATH);
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName(METHOD_NAME);
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());
    }

    @After
    public void after() {
        ApplicationConfigCache.getInstance().invalidateAll();
    }

    @Test
    public void genericInvokerTest() throws IllegalAccessException, NoSuchFieldException {
        ReferenceConfig referenceConfig = mock(ReferenceConfig.class);
        Invoker invoker = mock(Invoker.class);
        URL url = mock(URL.class);
        final MockedStatic<ReflectUtils> reflectUtilsMockedStatic = mockStatic(ReflectUtils.class);
        when(ReflectUtils.getFieldValue(referenceConfig, Constants.DUBBO_REFRENCE_INVOKER)).thenReturn(invoker);
        when(invoker.getUrl()).thenReturn(url);
        when(url.getParameter(anyString())).thenReturn("2.7.3");

        GenericService genericService = mock(GenericService.class);
        when(referenceConfig.get()).thenReturn(genericService);
        when(referenceConfig.getInterface()).thenReturn(PATH);
        CompletableFuture<Object> future = new CompletableFuture<>();
        when(genericService.$invokeAsync(METHOD_NAME, LEFT, RIGHT)).thenReturn(future);
        when(genericService.$invoke(METHOD_NAME, LEFT, RIGHT)).thenReturn(new Object());
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        Field field = ApplicationConfigCache.class.getDeclaredField("cache");
        field.setAccessible(true);
        ((LoadingCache) field.get(applicationConfigCache)).put(PATH, referenceConfig);
        ApacheDubboProxyService apacheDubboProxyService = new ApacheDubboProxyService(new BodyParamResolveServiceImpl());
        apacheDubboProxyService.genericInvoker("", metaData, exchange);
        future.complete("success");
        verify(genericService, times(1)).$invokeAsync(METHOD_NAME, LEFT, RIGHT);

        when(url.getParameter(anyString())).thenReturn("2.7.2");
        apacheDubboProxyService.genericInvoker("", metaData, exchange);
        verify(genericService, times(1)).$invoke(METHOD_NAME, LEFT, RIGHT);
        reflectUtilsMockedStatic.close();
    }

    @Test
    public void isProviderSupportAsyncTest() throws IllegalAccessException,
            NoSuchMethodException, InvocationTargetException {
        ReferenceConfig referenceConfig = mock(ReferenceConfig.class);
        Invoker invoker = mock(Invoker.class);
        final URL url = mock(URL.class);
        final MockedStatic<ReflectUtils> reflectUtilsMockedStatic = mockStatic(ReflectUtils.class);
        when(ReflectUtils.getFieldValue(referenceConfig, Constants.DUBBO_REFRENCE_INVOKER))
                .thenReturn(invoker);
        when(invoker.getUrl()).thenReturn(url);
        when(url.getParameter(anyString())).thenReturn("2.7.3");
        ApacheDubboProxyService apacheDubboProxyService = mock(ApacheDubboProxyService.class);
        Method isProviderSupportAsyncMethod = ApacheDubboProxyService.class
                .getDeclaredMethod("isProviderSupportAsync", ReferenceConfig.class);
        isProviderSupportAsyncMethod.setAccessible(true);
        Assert.assertTrue((Boolean) isProviderSupportAsyncMethod.invoke(apacheDubboProxyService, referenceConfig));
        when(url.getParameter(anyString())).thenReturn("2.7.2");
        Assert.assertFalse((Boolean) isProviderSupportAsyncMethod.invoke(apacheDubboProxyService, referenceConfig));
        reflectUtilsMockedStatic.close();
    }

    static class BodyParamResolveServiceImpl implements BodyParamResolveService {

        @Override
        public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
            return new ImmutablePair<>(LEFT, RIGHT);
        }
    }
}
