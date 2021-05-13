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

package org.apache.shenyu.plugin.sofa.subscriber;

import com.alipay.sofa.rpc.core.exception.SofaRpcRuntimeException;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * SofaMetaDataSubscriberTest.
 */
@RunWith(MockitoJUnitRunner.class)
public final class SofaMetaDataSubscriberTest {
    private SofaMetaDataSubscriber sofaPluginDataHandler;

    private MetaData metaData;

    @Before
    public void setUp() {
        sofaPluginDataHandler = new SofaMetaDataSubscriber();
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath("/sofa/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.SOFA.getName());
    }

    @Test(expected = SofaRpcRuntimeException.class)
    public void testOnSubscribe() {
        sofaPluginDataHandler.onSubscribe(metaData);
    }
}
