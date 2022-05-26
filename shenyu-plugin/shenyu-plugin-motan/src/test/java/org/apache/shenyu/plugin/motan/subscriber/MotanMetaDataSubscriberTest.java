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

package org.apache.shenyu.plugin.motan.subscriber;

import org.apache.shenyu.common.dto.MetaData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentMap;

/**
 * The Test Case For MotanMetaDataSubscriber.
 */
public final class MotanMetaDataSubscriberTest {

    private MotanMetaDataSubscriber motanMetaDataSubscriber;

    private MetaData metaData;

    @BeforeEach
    public void setUp() {
        this.motanMetaDataSubscriber = new MotanMetaDataSubscriber();
        this.metaData = new MetaData();
        this.metaData.setAppName("motan");
        this.metaData.setContextPath("/motan");
        this.metaData.setPath("/motan/hello");
        this.metaData.setRpcType("motan");
        this.metaData.setServiceName("org.apache.shenyu.examples.motan.service.MotanDemoService");
        this.metaData.setMethodName("hello");
        this.metaData.setParameterTypes("java.lang.String");
        this.metaData.setEnabled(true);
        metaData.setRpcExt("{\"methodInfo\":[{\"methodName\":\"hello\",\"params\":[{\"left\":\"java.lang.String\",\"right\":\"name\"}]}],\"group\":\"motan-shenyu-rpc\"}");
    }

    @Test
    public void testSubscribe() throws NoSuchFieldException, IllegalAccessException {
        motanMetaDataSubscriber.onSubscribe(metaData);
        Field field1 = motanMetaDataSubscriber.getClass().getDeclaredField("META_DATA");
        field1.setAccessible(true);
        ConcurrentMap<String, MetaData> metaData1 = (ConcurrentMap<String, MetaData>) field1.get("META_DATA");
        Assertions.assertEquals(metaData1.get("/motan/hello").getRpcType(), "motan");
        motanMetaDataSubscriber.unSubscribe(metaData);
        Assertions.assertEquals(metaData1.get("/motan/hello"), null);
    }
}
