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

package org.apache.shenyu.plugin.sofa.proxy;

import com.alipay.sofa.rpc.api.GenericService;
import com.alipay.sofa.rpc.config.ConsumerConfig;
import com.alipay.sofa.rpc.context.RpcInvokeContext;
import com.google.common.cache.LoadingCache;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.SofaRegisterConfig;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.sofa.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.sofa.param.SofaParamResolveService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.lang.NonNull;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * SofaProxyServiceTest.
 */
@ExtendWith(MockitoExtension.class)
public final class SofaProxyServiceTest {
    
    private static final String PATH = "/sofa/findAll";
    
    private static final String METHOD_NAME = "findAll";
    
    private static final String[] LEFT = new String[]{};
    
    private static final Object[] RIGHT = new Object[]{};
    
    private MetaData metaData;
    
    private ServerWebExchange exchange;
    
    @BeforeEach
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath(PATH);
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName(METHOD_NAME);
        metaData.setRpcType(RpcTypeEnum.SOFA.getName());
        metaData.setRpcExt("{\"loadbalance\": \"loadbalance\"}");
    }
    
    @AfterEach
    public void after() {
        ApplicationConfigCache.getInstance().invalidateAll();
    }
    
    @Test
    @SuppressWarnings("all")
    public void testGenericInvoker() throws IllegalAccessException {
        ConsumerConfig consumerConfig = mock(ConsumerConfig.class);
        GenericService genericService = mock(GenericService.class);
        when(consumerConfig.refer()).thenReturn(genericService);
        when(consumerConfig.getInterfaceId()).thenReturn(PATH);
        when(genericService.$genericInvoke(METHOD_NAME, LEFT, RIGHT)).thenReturn(null);
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        final Field cacheField = FieldUtils.getDeclaredField(ApplicationConfigCache.class, "cache", true);
        assertNotNull(cacheField);
        final Object cache = cacheField.get(applicationConfigCache);
        assertTrue(cache instanceof LoadingCache);
        ((LoadingCache) cache).put(PATH, consumerConfig);
        SofaProxyService sofaProxyService = new SofaProxyService(new SofaParamResolveServiceImpl());
        sofaProxyService.genericInvoker("", metaData, exchange);
        RpcInvokeContext.getContext().getResponseCallback().onAppResponse("success", null, null);
        final SofaRegisterConfig sofaRegisterConfig = new SofaRegisterConfig();
        sofaRegisterConfig.setThreadpool(Constants.SHARED);
        applicationConfigCache.init(sofaRegisterConfig);
    }

    @Test
    public void applicationConfigCacheTest() throws NoSuchFieldException, IllegalAccessException {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        final ShenyuThreadPoolExecutor shenyuThreadPoolExecutor = mock(ShenyuThreadPoolExecutor.class);
        when(context.getBean(ShenyuThreadPoolExecutor.class)).thenReturn(shenyuThreadPoolExecutor);
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        final SofaRegisterConfig sofaRegisterConfig = new SofaRegisterConfig();
        sofaRegisterConfig.setThreadpool(Constants.SHARED);
        assertDoesNotThrow(() -> applicationConfigCache.init(sofaRegisterConfig));
        final Field threadPool = ApplicationConfigCache.class.getDeclaredField("threadPool");
        threadPool.setAccessible(true);
        threadPool.set(applicationConfigCache, null);
        sofaRegisterConfig.setThreadpool(Constants.FIXED);
        assertThrows(UnsupportedOperationException.class, () -> applicationConfigCache.init(sofaRegisterConfig));
        threadPool.set(applicationConfigCache, null);
        sofaRegisterConfig.setThreadpool(Constants.EAGER);
        assertThrows(UnsupportedOperationException.class, () -> applicationConfigCache.init(sofaRegisterConfig));
        threadPool.set(applicationConfigCache, null);
        sofaRegisterConfig.setThreadpool(Constants.LIMITED);
        assertThrows(UnsupportedOperationException.class, () -> applicationConfigCache.init(sofaRegisterConfig));
        threadPool.set(applicationConfigCache, null);
        sofaRegisterConfig.setThreadpool(Constants.CACHED);
        assertDoesNotThrow(() -> applicationConfigCache.init(sofaRegisterConfig));
        threadPool.set(applicationConfigCache, null);
        sofaRegisterConfig.setThreadpool("other");
        assertDoesNotThrow(() -> applicationConfigCache.init(sofaRegisterConfig));
    }

    @Test
    public void buildTest() {
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        final SofaRegisterConfig sofaRegisterConfig = new SofaRegisterConfig();
        applicationConfigCache.init(sofaRegisterConfig);
        applicationConfigCache.build(metaData);
    }
    
    static class SofaParamResolveServiceImpl implements SofaParamResolveService {
        
        @Override
        @NonNull
        public Pair<String[], Object[]> buildParameter(final String body, final String parameterTypes) {
            return new ImmutablePair<>(LEFT, RIGHT);
        }
    }
}
