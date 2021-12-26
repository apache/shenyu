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

package org.apache.shenyu.plugin.apache.dubbo.subscriber;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApacheDubboConfigCache;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.mockito.MockedStatic;
import org.powermock.core.classloader.annotations.PowerMockIgnore;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;

/**
 * The Test Case For ApacheDubboMetaDataSubscriber.
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(ApacheDubboConfigCache.class)
@PowerMockIgnore("org.apache.dubbo.*")
public final class ApacheDubboMetaDataSubscriberTest {

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    private ApacheDubboMetaDataSubscriber apacheDubboMetaDataSubscriber;

    private MetaData metaData;

    @BeforeClass
    public static void beforeClass() {
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(ApacheDubboConfigCache.class))
                .thenReturn(mock(Logger.class));
    }

    @AfterClass
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
    }

    @Before
    public void setUp() {
        apacheDubboMetaDataSubscriber = new ApacheDubboMetaDataSubscriber();
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath("/dubbo/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());
        metaData.setRpcExt("{\"group\":\"Group\",\"version\":\"2.7.5\",\"loadbalance\":\"Balance\",\"url\":\"http://192.168.55.113/dubbo\"}");
        metaData.setParameterTypes("parameterTypes");
    }

    @Test
    public void testOnSubscribe() {
        apacheDubboMetaDataSubscriber.onSubscribe(metaData);
        MetaData metaData = MetaData.builder()
                .id("1332017966661636096")
                .appName("dubbo")
                .path("/dubbo/findAll")
                .serviceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService")
                .methodName("findById")
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .rpcExt("{\"group\":\"Group\",\"version\":\"2.7.5\",\"loadbalance\":\"Balance\",\"url\":\"http://192.168.55.113/dubbo\"}")
                .parameterTypes("parameterTypes").build();
        ApacheDubboMetaDataSubscriber apacheDubboMetaDataSubscriberMock = mock(ApacheDubboMetaDataSubscriber.class);
        doNothing().when(apacheDubboMetaDataSubscriberMock).onSubscribe(metaData);
        apacheDubboMetaDataSubscriberMock.onSubscribe(metaData);
        apacheDubboMetaDataSubscriber.unSubscribe(metaData);
    }
}
