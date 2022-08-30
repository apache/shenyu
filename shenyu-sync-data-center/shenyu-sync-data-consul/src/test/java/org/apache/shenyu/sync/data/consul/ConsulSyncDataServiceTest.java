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
import com.ecwid.consul.v1.QueryParams;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.shenyu.common.constant.ConsulConstants;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
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

    private ConsulSyncDataService consulSyncDataService;

    @Mock
    private GetValue getValue;

    @Mock
    private Response response;

    @BeforeEach
    public void setup() {
        when(consulConfig.getWaitTime()).thenReturn(WAIT_TIME);
        when(consulConfig.getWatchDelay()).thenReturn(WATCH_DELAY);
        final List<GetValue> list = new ArrayList<>(1);
        when(getValue.getModifyIndex()).thenReturn(INDEX);
        when(getValue.getKey()).thenReturn(ConsulConstants.PLUGIN_DATA);
        when(getValue.getDecodedValue()).thenReturn("{}");
        list.add(getValue);
        when(consulClient.getKVValues(any(), any(), any()))
                .thenReturn(response);
        when(response.getValue()).thenReturn(list);
        when(response.getConsulIndex()).thenReturn(INDEX);
        consulSyncDataService = new ConsulSyncDataService(consulClient, consulConfig, null,
                Collections.emptyList(), Collections.emptyList());
    }

    @Test
    public void testStart() throws Exception {
        TimeUnit.SECONDS.sleep(2);
        verify(consulClient, atLeast(1)).getKVValues(eq(ConsulConstants.SYNC_PRE_FIX),
                eq(null), any(QueryParams.class));
        verify(getValue, atLeast(1)).getModifyIndex();
        verify(getValue, atLeast(1)).getDecodedValue();
        verify(response, atLeast(3)).getValue();
    }

    @AfterEach
    public void testStop() {
        consulSyncDataService.close();
    }

    @Test
    public void testWatchConfigKeyValues() throws NoSuchMethodException, IllegalAccessException, NoSuchFieldException {
        final Method watchConfigKeyValues = ConsulSyncDataService.class.getDeclaredMethod("watchConfigKeyValues");
        watchConfigKeyValues.setAccessible(true);

        final Field consul = ConsulSyncDataService.class.getDeclaredField("consulClient");
        consul.setAccessible(true);
        final ConsulClient consulClient = mock(ConsulClient.class);
        consul.set(consulSyncDataService, consulClient);
        final Response<List<GetValue>> response = mock(Response.class);

        when(consulClient.getKVValues(any(), any(), any())).thenReturn(response);

        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService));

        List<GetValue> getValues = new ArrayList<>(1);
        getValues.add(mock(GetValue.class));
        when(response.getValue()).thenReturn(getValues);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService));

        when(response.getConsulIndex()).thenReturn(2L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService));

        when(response.getConsulIndex()).thenReturn(null);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService));

        when(response.getConsulIndex()).thenReturn(0L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService));

        final Field consulIndexes = ConsulSyncDataService.class.getDeclaredField("consulIndexes");
        consulIndexes.setAccessible(true);
        final Map<String, Long> consulIndexesSource = (Map<String, Long>) consulIndexes.get(consulSyncDataService);
        consulIndexesSource.put("/null", null);
        when(response.getConsulIndex()).thenReturn(2L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(consulSyncDataService));
    }
}
