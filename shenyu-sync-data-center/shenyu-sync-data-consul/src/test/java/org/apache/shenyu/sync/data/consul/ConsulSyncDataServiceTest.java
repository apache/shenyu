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

package org.apache.shenyu.sync.data.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link ConsulSyncDataService}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ConsulSyncDataServiceTest {

    private static final int WAIT_TIME = 1000;

    private static final int WATCH_DELAY = 1000;

    private static final long INDEX = 1L;

    @Mock
    private ConsulClient consulClient;

    @Mock
    private ConsulConfig consulConfig;

    @Mock
    private ShenyuConfig shenyuConfig;

    @InjectMocks
    private ConsulSyncDataService consulSyncDataService;

    @Mock
    private GetValue getValue;

    @Mock
    private Response response;

    @Test
    public void testWatchConfigKeyValues() throws NoSuchMethodException, IllegalAccessException, NoSuchFieldException {
        final Method watchConfigKeyValues = ConsulSyncDataService.class.getDeclaredMethod("watchConfigKeyValues",
                String.class, BiConsumer.class, Consumer.class);
        watchConfigKeyValues.setAccessible(true);

        final Field consul = ConsulSyncDataService.class.getDeclaredField("consulClient");
        consul.setAccessible(true);
        final ConsulClient consulClient = mock(ConsulClient.class);
        consul.set(consulSyncDataService, consulClient);

        final Field declaredField = ConsulSyncDataService.class.getDeclaredField("consulConfig");
        declaredField.setAccessible(true);
        final ConsulConfig consulConfig = mock(ConsulConfig.class);
        declaredField.set(consulSyncDataService, consulConfig);

        final Field executorField = ConsulSyncDataService.class.getDeclaredField("executor");
        executorField.setAccessible(true);
        executorField.set(consulSyncDataService, mock(ScheduledThreadPoolExecutor.class));

        final Field shenyuConfigField = ConsulSyncDataService.class.getDeclaredField("shenyuConfig");
        shenyuConfigField.setAccessible(true);
        final ShenyuConfig shenyuConfig = mock(ShenyuConfig.class);
        shenyuConfigField.set(consulSyncDataService, shenyuConfig);

        final Response<List<GetValue>> response = mock(Response.class);

        when(consulClient.getKVValues(any(), any(), any())).thenReturn(response);

        BiConsumer<String, String> updateHandler = (changeData, decodedValue) -> {

        };
        Consumer<String> deleteHandler = removeKey -> {

        };
        String watchPathRoot = "/" + Constants.SYS_DEFAULT_NAMESPACE_ID + "/shenyu";
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService, watchPathRoot, updateHandler, deleteHandler));

        List<GetValue> getValues = new ArrayList<>(1);
        getValues.add(mock(GetValue.class));
        when(response.getValue()).thenReturn(getValues);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService, watchPathRoot, updateHandler, deleteHandler));

        when(response.getConsulIndex()).thenReturn(2L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService, watchPathRoot, updateHandler, deleteHandler));

        when(response.getConsulIndex()).thenReturn(null);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService, watchPathRoot, updateHandler, deleteHandler));

        when(response.getConsulIndex()).thenReturn(0L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService, watchPathRoot, updateHandler, deleteHandler));

        final Field consulIndexes = ConsulSyncDataService.class.getDeclaredField("consulIndexes");
        consulIndexes.setAccessible(true);
        final Map<String, Long> consulIndexesSource = (Map<String, Long>) consulIndexes.get(consulSyncDataService);
        consulIndexesSource.remove(watchPathRoot);
        when(response.getConsulIndex()).thenReturn(2L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService, watchPathRoot, updateHandler, deleteHandler));
    }

    @Test
    public void testWatcherStateUsesConcurrentMaps() throws NoSuchFieldException, IllegalAccessException {
        Field consulIndexesField = ConsulSyncDataService.class.getDeclaredField("consulIndexes");
        consulIndexesField.setAccessible(true);
        Field cacheDataField = ConsulSyncDataService.class.getDeclaredField("cacheConsulDataKeyMap");
        cacheDataField.setAccessible(true);

        Assertions.assertTrue(consulIndexesField.get(consulSyncDataService) instanceof ConcurrentMap);
        Assertions.assertTrue(cacheDataField.get(consulSyncDataService) instanceof ConcurrentMap);
    }

    @Test
    public void watchConfigKeyValuesShouldDispatchWhenAnotherRootSharesTheNewIndex() throws Exception {
        final Method watchConfigKeyValues = ConsulSyncDataService.class.getDeclaredMethod("watchConfigKeyValues",
                String.class, BiConsumer.class, Consumer.class);
        watchConfigKeyValues.setAccessible(true);

        final GetValue changed = new GetValue();
        changed.setKey("/shenyu/selector/sel-1");
        changed.setValue(Base64.getEncoder().encodeToString("changed".getBytes(StandardCharsets.UTF_8)));
        changed.setModifyIndex(20L);
        changed.setCreateIndex(10L);

        final ConsulClient consulClientMock = mock(ConsulClient.class);
        when(consulClientMock.getKVValues(anyString(), any(), any()))
                .thenReturn(new Response<>(Collections.singletonList(changed), 20L, true, 1L));
        final Field consulField = ConsulSyncDataService.class.getDeclaredField("consulClient");
        consulField.setAccessible(true);
        consulField.set(consulSyncDataService, consulClientMock);

        final ConsulConfig consulConfigMock = mock(ConsulConfig.class);
        when(consulConfigMock.getWatchDelay()).thenReturn(1000);
        when(consulConfigMock.getWaitTime()).thenReturn(1000);
        final Field configField = ConsulSyncDataService.class.getDeclaredField("consulConfig");
        configField.setAccessible(true);
        configField.set(consulSyncDataService, consulConfigMock);

        final Field executorField = ConsulSyncDataService.class.getDeclaredField("executor");
        executorField.setAccessible(true);
        executorField.set(consulSyncDataService, mock(ScheduledThreadPoolExecutor.class));

        @SuppressWarnings("unchecked")
        final Map<String, Long> consulIndexes = (Map<String, Long>) getConsulIndexesField().get(consulSyncDataService);
        consulIndexes.put("/shenyu/plugin", 20L);
        consulIndexes.put("/shenyu/selector", 10L);

        final List<String> dispatched = new CopyOnWriteArrayList<>();
        final BiConsumer<String, String> updateHandler = (key, value) -> dispatched.add(key);
        final Consumer<String> deleteHandler = key -> dispatched.add("deleted:" + key);
        watchConfigKeyValues.invoke(consulSyncDataService, "/shenyu/selector", updateHandler, deleteHandler);
        assertFalse(dispatched.isEmpty(), "a root whose own index advanced must dispatch even when another root already stored that index");
    }

    private Field getConsulIndexesField() throws NoSuchFieldException {
        final Field field = ConsulSyncDataService.class.getDeclaredField("consulIndexes");
        field.setAccessible(true);
        return field;
    }

}
