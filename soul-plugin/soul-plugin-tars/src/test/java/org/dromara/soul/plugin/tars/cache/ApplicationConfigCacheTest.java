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

package org.dromara.soul.plugin.tars.cache;

import com.qq.tars.protocol.annotation.Servant;
import org.assertj.core.util.Lists;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.tars.proxy.TarsInvokePrxList;
import org.dromara.soul.plugin.tars.util.PrxInfoUtil;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Test case for {@link ApplicationConfigCache}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApplicationConfigCacheTest {

    private ApplicationConfigCache applicationConfigCacheUnderTest;

    @Before
    public void setUp() {
        applicationConfigCacheUnderTest = ApplicationConfigCache.getInstance();
    }

    @Test
    public void testGet() throws ClassNotFoundException {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "serviceName", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        applicationConfigCacheUnderTest.initPrx(metaData);
        final TarsInvokePrxList result = applicationConfigCacheUnderTest.get("path");
        assertNotNull(result);
        assertEquals("promise_method1", result.getMethod().getName());
        assertEquals(2, result.getParamTypes().length);
        assertEquals(2, result.getParamNames().length);
        Class<?> prxClazz = Class.forName(PrxInfoUtil.getPrxName(metaData));
        assertTrue(Arrays.stream(prxClazz.getAnnotations()).anyMatch(annotation -> annotation instanceof Servant));
    }

    @Test
    public void testConcurrentInitPrx() throws InterruptedException {
        final MetaData metaData1 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path1", RpcTypeEnum.TARS.getName(), "serviceName1", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        final MetaData metaData2 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path2", RpcTypeEnum.TARS.getName(), "serviceName2", "method2",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method2\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        final MetaData metaData3 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path3", RpcTypeEnum.TARS.getName(), "serviceName3", "method3",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method3\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        final MetaData metaData4 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path4", RpcTypeEnum.TARS.getName(), "serviceName4", "method4",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method4\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        List<MetaData> metaDataList = Lists.list(metaData1, metaData2, metaData3, metaData4);
        ExecutorService executorService = Executors.newFixedThreadPool(4,
                SoulThreadFactory.create("ApplicationConfigCache-tars-initPrx", false));
        CountDownLatch countDownLatch = new CountDownLatch(4);
        metaDataList.forEach(metaData -> executorService.execute(() -> {
            applicationConfigCacheUnderTest.initPrx(metaData);
            countDownLatch.countDown();
        }));
        countDownLatch.await();
        assertEquals("promise_method1", applicationConfigCacheUnderTest.get("path1").getMethod().getName());
        assertEquals("promise_method2", applicationConfigCacheUnderTest.get("path2").getMethod().getName());
        assertEquals("promise_method3", applicationConfigCacheUnderTest.get("path3").getMethod().getName());
        assertEquals("promise_method4", applicationConfigCacheUnderTest.get("path4").getMethod().getName());
    }

    @Test
    public void testInitPrx() {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path", RpcTypeEnum.TARS.getName(), "serviceName", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"key\":\"int\",\"value\":\"param1\"},{\"key\":\"java.lang.Integer\",\"value\":\"param2\"}]}]}", false);
        applicationConfigCacheUnderTest.initPrx(metaData);
        final TarsInvokePrxList result = applicationConfigCacheUnderTest.get("path");
        assertEquals("promise_method1", result.getMethod().getName());
    }

    @Test
    public void testGetClassMethodKey() {
        assertEquals("className_methodName", ApplicationConfigCache.getClassMethodKey("className", "methodName"));
    }

    @Test
    public void testGetInstance() {
        final ApplicationConfigCache result = ApplicationConfigCache.getInstance();
        assertNotNull(result);
    }
}
