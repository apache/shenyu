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
import org.apache.shenyu.common.constant.ConsulConstants;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * test case for {@link ConsulSyncDataService}.
 */
public final class ConsulSyncDataServiceTest {

    private ConsulClient consulClient;

    private ConsulSyncDataService consulSyncDataService;

    @Mock
    private Response<List<GetValue>> response;

    @Mock
    private Map<String, Long> consulIndexes;

    private AutoCloseable autoCloseable;

    @Before
    public void setup() {
        consulClient = spy(ConsulClient.class);
        ConsulConfig consulConfig = new ConsulConfig();
        consulConfig.setWaitTime(1000);
        consulConfig.setWatchDelay(1000);
        consulSyncDataService = new ConsulSyncDataService(consulClient, consulConfig, null,
                Collections.emptyList(), Collections.emptyList());
        AtomicBoolean running = (AtomicBoolean) ReflectUtils.getFieldValue(consulSyncDataService, "running");
        assertTrue(Objects.requireNonNull(running).get());
        autoCloseable = MockitoAnnotations.openMocks(this);
    }

    @After
    public void after() throws Exception {
        consulSyncDataService.close();
        AtomicBoolean running = (AtomicBoolean) ReflectUtils.getFieldValue(consulSyncDataService, "running");
        assertFalse(Objects.requireNonNull(running).get());
        autoCloseable.close();
    }

    @Test
    public void testWatch() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long index = 1L;
        final List<GetValue> list = new ArrayList<>();
        GetValue getValue = spy(GetValue.class);
        doReturn(index).when(getValue).getModifyIndex();
        doReturn(ConsulConstants.PLUGIN_DATA).when(getValue).getKey();
        doReturn("{}").when(getValue).getDecodedValue();
        list.add(getValue);

        doReturn(response).when(consulClient).getKVValues(any(), any(), any());
        when(response.getValue()).thenReturn(list);
        when(response.getConsulIndex()).thenReturn(1L);

        Method watchConfigKeyValues = consulSyncDataService.getClass().getDeclaredMethod("watchConfigKeyValues");
        watchConfigKeyValues.setAccessible(true);
        watchConfigKeyValues.invoke(consulSyncDataService);

        Object target = ReflectUtils.getFieldValue(consulSyncDataService, "consulIndexes");

        consulIndexes = typeConversionMap(target);

        Assert.assertEquals(index, consulIndexes.get(ConsulConstants.SYNC_PRE_FIX));
    }

    private Map<String, Long> typeConversionMap(final Object obj) {
        Map<String, Long> result = new HashMap<>();
        if (obj instanceof Map<?, ?>) {
            Map<?, ?> map = (Map<?, ?>) obj;
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                result.put((String) entry.getKey(), (Long) entry.getValue());
            }
        }
        return result;
    }
}
