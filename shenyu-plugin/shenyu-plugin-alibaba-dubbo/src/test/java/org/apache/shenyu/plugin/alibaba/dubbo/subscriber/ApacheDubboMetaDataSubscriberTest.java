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

package org.apache.shenyu.plugin.alibaba.dubbo.subscriber;

import com.alibaba.dubbo.config.ReferenceConfig;
import com.google.common.cache.LoadingCache;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.alibaba.dubbo.cache.ApplicationConfigCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.lang.reflect.Field;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doNothing;


/**
 * The Test Case For AlibabaDubboMetaDataSubscriber.
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApacheDubboMetaDataSubscriberTest {

    private AlibabaDubboMetaDataSubscriber alibabaDubboMetaDataSubscriber;

    private MetaData metaData;

    @Before
    public void setUp() {
        alibabaDubboMetaDataSubscriber = new AlibabaDubboMetaDataSubscriber();
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath("/dubbo/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());
        metaData.setRpcExt("{\"group\":\"Group\",\"version\":\"2.6.5\",\"url\":\"http://192.168.55.113/dubbo\"}");
        metaData.setParameterTypes("parameterTypes");
    }

    @Test
    public void testOnSubscribe() throws NoSuchFieldException, IllegalAccessException {
        ReferenceConfig referenceConfig = mock(ReferenceConfig.class);
        when(referenceConfig.getInterface()).thenReturn("/dubbo/findAll");
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        Field field = ApplicationConfigCache.class.getDeclaredField("cache");
        field.setAccessible(true);
        ((LoadingCache) field.get(applicationConfigCache)).put("/dubbo/findAll", referenceConfig);
        alibabaDubboMetaDataSubscriber.onSubscribe(metaData);
        MetaData metaData = MetaData.builder()
                .id("1332017966661636096")
                .appName("dubbo")
                .path("/dubbo/findAll")
                .serviceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService")
                .methodName("findById")
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .rpcExt("{\"group\":\"Group\",\"version\":\"2.6.5\",\"loadbalance\":\"roundrobin\",\"url\":\"http://192.168.55.113/dubbo\"}")
                .parameterTypes("parameterTypes").build();
        AlibabaDubboMetaDataSubscriber alibabaDubboMetaDataSubscriberMock = mock(AlibabaDubboMetaDataSubscriber.class);
        doNothing().when(alibabaDubboMetaDataSubscriberMock).onSubscribe(metaData);
        alibabaDubboMetaDataSubscriberMock.onSubscribe(metaData);
        alibabaDubboMetaDataSubscriber.unSubscribe(metaData);
    }
}
