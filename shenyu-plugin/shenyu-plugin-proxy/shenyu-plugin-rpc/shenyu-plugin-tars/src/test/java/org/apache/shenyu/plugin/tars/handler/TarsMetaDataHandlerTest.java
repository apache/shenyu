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

package org.apache.shenyu.plugin.tars.handler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.tars.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrx;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrxList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link org.apache.shenyu.plugin.tars.handler.TarsMetaDataHandler}.
 */
@ExtendWith(MockitoExtension.class)
public class TarsMetaDataHandlerTest {

    private TarsMetaDataHandler tarsMetaDataHandler;

    private MetaData metaData;

    @BeforeEach
    public void setUp() {
        metaData = new MetaData("id", "testApp", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "serviceName", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"left\":\"int\",\"right\":\"param1\"},"
                + "{\"left\":\"java.lang.Integer\",\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}", false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        tarsMetaDataHandler = new TarsMetaDataHandler();
    }

    @Test
    public void testOnSubscribe() {
        tarsMetaDataHandler.handle(metaData);
        /**
         * test for cache;
         */
        tarsMetaDataHandler.handle(metaData);
    }

    @Test
    public void testUnSubscribe() {
        tarsMetaDataHandler.remove(metaData);
    }

    @Test
    public void testHandleDoesNotReinitializeExistingApplication() {
        final ApplicationConfigCache cache = mock(ApplicationConfigCache.class);
        final TarsInvokePrxList invokePrxList = new TarsInvokePrxList();
        invokePrxList.addTarsInvokePrxList(Collections.singletonList(
                new TarsInvokePrx(new Object(), "127.0.0.1:8080", metaData.getAppName())));
        when(cache.get(anyString())).thenReturn(invokePrxList);

        try (MockedStatic<ApplicationConfigCache> cacheStatic = mockStatic(ApplicationConfigCache.class)) {
            cacheStatic.when(ApplicationConfigCache::getInstance).thenReturn(cache);
            tarsMetaDataHandler.handle(metaData);
            clearInvocations(cache);
            tarsMetaDataHandler.handle(metaData);
            verify(cache, never()).initPrx(metaData);
        }
    }

    @Test
    public void testHandleRefreshesUpdatedService() {
        final ApplicationConfigCache cache = mock(ApplicationConfigCache.class);
        final TarsInvokePrxList invokePrxList = new TarsInvokePrxList();
        invokePrxList.addTarsInvokePrxList(Collections.singletonList(
                new TarsInvokePrx(new Object(), "127.0.0.1:8080", metaData.getAppName())));
        when(cache.get(anyString())).thenReturn(invokePrxList);
        final MetaData updated = new MetaData("id", "testApp", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "updatedService", "method1",
                "parameterTypes", metaData.getRpcExt(), false, Constants.SYS_DEFAULT_NAMESPACE_ID);

        try (MockedStatic<ApplicationConfigCache> cacheStatic = mockStatic(ApplicationConfigCache.class)) {
            cacheStatic.when(ApplicationConfigCache::getInstance).thenReturn(cache);
            tarsMetaDataHandler.handle(metaData);
            clearInvocations(cache);
            tarsMetaDataHandler.handle(updated);
            verify(cache).initPrx(updated);
        }
    }

    @Test
    public void testRemoveMatchesApplicationName() {
        final ApplicationConfigCache cache = mock(ApplicationConfigCache.class);
        final TarsInvokePrxList invokePrxList = new TarsInvokePrxList();
        invokePrxList.addTarsInvokePrxList(Collections.singletonList(
                new TarsInvokePrx(new Object(), "127.0.0.1:8080", metaData.getAppName())));
        invokePrxList.addTarsInvokePrxList(Collections.singletonList(
                new TarsInvokePrx(new Object(), "127.0.0.2:8080", "otherApp")));
        when(cache.get(anyString())).thenReturn(invokePrxList);

        try (MockedStatic<ApplicationConfigCache> cacheStatic = mockStatic(ApplicationConfigCache.class)) {
            cacheStatic.when(ApplicationConfigCache::getInstance).thenReturn(cache);
            tarsMetaDataHandler.remove(metaData);
            assertEquals(1, invokePrxList.getTarsInvokePrxList().size());
            assertEquals("otherApp", invokePrxList.getTarsInvokePrxList().get(0).getAppName());
        }
    }
}
