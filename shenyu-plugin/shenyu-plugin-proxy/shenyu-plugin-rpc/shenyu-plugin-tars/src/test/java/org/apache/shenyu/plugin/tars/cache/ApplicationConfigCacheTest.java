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

package org.apache.shenyu.plugin.tars.cache;

import com.qq.tars.protocol.annotation.Servant;
import com.qq.tars.client.Communicator;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.tars.handler.TarsPluginDataHandler;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrx;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrxList;
import org.apache.shenyu.plugin.tars.util.PrxInfoUtil;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.ReentrantLock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

/**
 * Test case for {@link ApplicationConfigCache}.
 */
@ExtendWith(MockitoExtension.class)
public final class ApplicationConfigCacheTest {

    private ApplicationConfigCache applicationConfigCacheUnderTest;

    @BeforeEach
    public void setUp() {
        applicationConfigCacheUnderTest = ApplicationConfigCache.getInstance();
    }

    @Test
    public void testGet() throws ClassNotFoundException {
        final String rpcExt = "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":"
                + "[{\"left\":\"int\",\"right\":\"param1\"},{\"left\":\"java.lang.Integer\","
                + "\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}";

        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path5", RpcTypeEnum.TARS.getName(), "serviceName5", "method1",
                "parameterTypes", rpcExt, false, Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertThrows(NullPointerException.class, () -> {
            applicationConfigCacheUnderTest.initPrx(metaData);
            final TarsInvokePrxList result = applicationConfigCacheUnderTest.get("path5");
            assertNotNull(result);
            assertEquals("promise_method1", result.getMethod().getName());
            assertEquals(2, result.getParamTypes().length);
            assertEquals(2, result.getParamNames().length);
            Class<?> prxClazz = Class.forName(PrxInfoUtil.getPrxName(metaData));
            assertTrue(Arrays.stream(prxClazz.getAnnotations()).anyMatch(annotation -> annotation instanceof Servant));

        });
    }

    @Test
    public void testConcurrentInitPrx() {
        final String rpcExt1 = "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":"
                + "[{\"left\":\"int\",\"right\":\"param1\"},{\"left\":\"java.lang.Integer\","
                + "\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}";
        final String rpcExt2 = "{\"methodInfo\":[{\"methodName\":\"method2\",\"params\":"
                + "[{\"left\":\"int\",\"right\":\"param1\"},{\"left\":\"java.lang.Integer\","
                + "\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}";
        final String rpcExt3 = "{\"methodInfo\":[{\"methodName\":\"method3\",\"params\":"
                + "[{\"left\":\"int\",\"right\":\"param1\"},{\"left\":\"java.lang.Integer\","
                + "\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}";
        final String rpcExt4 = "{\"methodInfo\":[{\"methodName\":\"method4\",\"params\":"
                + "[{\"left\":\"int\",\"right\":\"param1\"},{\"left\":\"java.lang.Integer\","
                + "\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}";

        final MetaData metaData1 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path1", RpcTypeEnum.TARS.getName(), "serviceName1", "method1",
                "parameterTypes", rpcExt1, false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        final MetaData metaData2 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path2", RpcTypeEnum.TARS.getName(), "serviceName2", "method2",
                "parameterTypes", rpcExt2, false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        final MetaData metaData3 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path3", RpcTypeEnum.TARS.getName(), "serviceName3", "method3",
                "parameterTypes", rpcExt3, false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        final MetaData metaData4 = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path4", RpcTypeEnum.TARS.getName(), "serviceName4", "method4",
                "parameterTypes", rpcExt4, false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        List<MetaData> metaDataList = Lists.list(metaData1, metaData2, metaData3, metaData4);
        assertThrows(NullPointerException.class, () -> {
            ExecutorService executorService = Executors.newFixedThreadPool(4,
                    ShenyuThreadFactory.create("ApplicationConfigCache-tars-initPrx", false));
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
        });
    }

    @Test
    public void testInitPrxWaitsForInitializationLock() throws Exception {
        final Field lockField = ApplicationConfigCache.class.getDeclaredField("LOCK");
        lockField.setAccessible(true);
        final ReentrantLock lock = (ReentrantLock) lockField.get(null);
        final MetaData metaData = new MetaData("id", "appName", "contextPath", "waitingPath",
                RpcTypeEnum.TARS.getName(), "serviceName", "methodName", "parameterTypes",
                null, false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        final Thread initThread = new Thread(() -> applicationConfigCacheUnderTest.initPrx(metaData));

        lock.lock();
        try {
            initThread.start();
            for (int i = 0; i < 100 && !lock.hasQueuedThread(initThread); i++) {
                Thread.sleep(10L);
            }
            assertTrue(lock.hasQueuedThread(initThread));
        } finally {
            lock.unlock();
        }
        initThread.join(1000L);
        assertFalse(initThread.isAlive());
    }

    @Test
    public void testInitPrx() {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "contextPath",
                "path6", RpcTypeEnum.TARS.getName(), "serviceName6", "method1",
                "parameterTypes", "{\"methodInfo\":[{\"methodName\":\"method1\",\"params\":[{\"left\":\"int\",\"right\":\"param1\"},"
                + "{\"left\":\"java.lang.Integer\",\"right\":\"param2\"}],\"returnType\":\"java.lang.String\"}]}", false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        assertThrows(NullPointerException.class, () -> {
            applicationConfigCacheUnderTest.initPrx(metaData);       
            final TarsInvokePrxList result = applicationConfigCacheUnderTest.get("path6");
            assertEquals("promise_method1", result.getMethod().getName());
        });
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

    @Test
    @SuppressWarnings("unchecked")
    public void testRefreshPublishesCompleteSnapshot() throws Exception {
        final String path = "snapshot-refresh";
        final Map<String, Class<?>> classes = (Map<String, Class<?>>) ReflectionTestUtils.getField(applicationConfigCacheUnderTest, "prxClassCache");
        final Communicator original = (Communicator) ReflectionTestUtils.getField(applicationConfigCacheUnderTest, "communicator");
        final Communicator communicator = mock(Communicator.class);
        final TarsInvokePrxList previous = applicationConfigCacheUnderTest.get(path);
        previous.setMethod(Object.class.getMethod("toString"));
        previous.addTarsInvokePrxList(Collections.singletonList(new TarsInvokePrx(new Object(), "old")));
        final MetaData metadata = new MetaData();
        metadata.setPath(path);
        metadata.setServiceName("service");
        final TarsUpstream upstream = TarsUpstream.builder().upstreamUrl("127.0.0.1:8080").build();
        classes.put(path, Object.class);
        ReflectionTestUtils.setField(applicationConfigCacheUnderTest, "communicator", communicator);
        try {
            when(communicator.stringToProxy(eq(Object.class), anyString())).thenAnswer(invocation -> {
                assertSame(previous, applicationConfigCacheUnderTest.get(path));
                assertEquals(1, previous.getTarsInvokePrxList().size());
                return new Object();
            });
            ReflectionTestUtils.invokeMethod(applicationConfigCacheUnderTest, "refreshTarsInvokePrxList", metadata, Collections.singletonList(upstream));
            assertNotSame(previous, applicationConfigCacheUnderTest.get(path));
            assertEquals(1, previous.getTarsInvokePrxList().size());
            assertEquals("old", previous.getTarsInvokePrxList().get(0).getHost());
            assertEquals("127.0.0.1:8080", applicationConfigCacheUnderTest.get(path).getTarsInvokePrxList().get(0).getHost());
            final TarsInvokePrxList current = applicationConfigCacheUnderTest.get(path);
            org.mockito.Mockito.doThrow(new IllegalStateException("proxy unavailable")).when(communicator).stringToProxy(eq(Object.class), anyString());
            assertThrows(IllegalStateException.class, () -> ReflectionTestUtils.invokeMethod(applicationConfigCacheUnderTest,
                    "refreshTarsInvokePrxList", metadata, Collections.singletonList(upstream)));
            assertSame(current, applicationConfigCacheUnderTest.get(path));
            assertEquals(1, current.getTarsInvokePrxList().size());
        } finally {
            classes.remove(path);
            ReflectionTestUtils.setField(applicationConfigCacheUnderTest, "communicator", original);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testInvalidateRemovesCompanionCaches() throws Exception {
        final MetaData metaData = new MetaData("id", "127.0.0.1:8080", "/demo", "/demo/test",
                RpcTypeEnum.TARS.getName(), "service", "method", "", "", false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        final Map<String, List<MetaData>> ctxPathCache = (Map<String, List<MetaData>>) getField("ctxPathCache");
        final Map<String, Class<?>> prxClassCache = (Map<String, Class<?>>) getField("prxClassCache");
        final Map<String, ApplicationConfigCache.TarsParamInfo> prxParamCache =
                (Map<String, ApplicationConfigCache.TarsParamInfo>) getField("prxParamCache");
        final Map<String, List<?>> refreshUpstreamCache = (Map<String, List<?>>) getField("refreshUpstreamCache");
        ctxPathCache.clear();
        prxClassCache.clear();
        prxParamCache.clear();
        refreshUpstreamCache.clear();
        ctxPathCache.put(metaData.getContextPath(), Collections.singletonList(metaData));
        prxClassCache.put(metaData.getPath(), ApplicationConfigCacheTest.class);
        final String paramKey = PrxInfoUtil.getPrxName(metaData) + "_" + metaData.getMethodName();
        prxParamCache.put(paramKey, new ApplicationConfigCache.TarsParamInfo(new Class<?>[0], new String[0]));
        refreshUpstreamCache.put(metaData.getContextPath(), Collections.emptyList());
        final TarsInvokePrxList cached = applicationConfigCacheUnderTest.get(metaData.getPath());

        applicationConfigCacheUnderTest.invalidate(metaData.getContextPath());

        assertTrue(ctxPathCache.isEmpty());
        assertTrue(prxClassCache.isEmpty());
        assertTrue(prxParamCache.isEmpty());
        assertTrue(refreshUpstreamCache.isEmpty());
        assertNotSame(cached, applicationConfigCacheUnderTest.get(metaData.getPath()));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    @SuppressWarnings("unchecked")
    void deletedUpstreamsCannotBeRepublishedByMetadata(final boolean emptyUpdate) throws Exception {
        final String context = "/deleted" + emptyUpdate;
        final MetaData metadata = new MetaData("id", "app", context, context + "/path", RpcTypeEnum.TARS.getName(),
                "deletedService" + emptyUpdate, "method", "", "{\"methodInfo\":[]}", false, Constants.SYS_DEFAULT_NAMESPACE_ID);
        final Map<String, List<MetaData>> contexts = (Map<String, List<MetaData>>) getField("ctxPathCache");
        final Map<String, Class<?>> classes = (Map<String, Class<?>>) getField("prxClassCache");
        final Map<String, List<TarsUpstream>> upstreams = (Map<String, List<TarsUpstream>>) getField("refreshUpstreamCache");
        final Communicator original = (Communicator) getField("communicator");
        final Communicator communicator = mock(Communicator.class);
        contexts.put(context, List.of(metadata));
        classes.put(metadata.getPath(), Object.class);
        upstreams.put(context, List.of(TarsUpstream.builder().upstreamUrl("127.0.0.1:8080").build()));
        applicationConfigCacheUnderTest.get(metadata.getPath()).addTarsInvokePrxList(List.of(new TarsInvokePrx(new Object(), "old")));
        ReflectionTestUtils.setField(applicationConfigCacheUnderTest, "communicator", communicator);
        try {
            SelectorData selector = new SelectorData();
            selector.setName(context);
            selector.setHandle("[]");
            if (emptyUpdate) {
                applicationConfigCacheUnderTest.initPrxClass(selector);
            } else {
                new TarsPluginDataHandler().removeSelector(selector);
            }
            assertFalse(upstreams.containsKey(context));
            applicationConfigCacheUnderTest.initPrx(metadata);
            assertTrue(classes.containsKey(metadata.getPath()), "Metadata must initialize successfully after deletion");
            assertTrue(applicationConfigCacheUnderTest.get(metadata.getPath()).getTarsInvokePrxList().isEmpty());
            verifyNoInteractions(communicator);
        } finally {
            applicationConfigCacheUnderTest.invalidate(context);
            ReflectionTestUtils.setField(applicationConfigCacheUnderTest, "communicator", original);
        }
    }

    private Object getField(final String fieldName) throws NoSuchFieldException, IllegalAccessException {
        java.lang.reflect.Field field = ApplicationConfigCache.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(applicationConfigCacheUnderTest);
    }
}
