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
import lombok.SneakyThrows;
import org.apache.shenyu.common.constant.ConsulConstants;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link ConsulSyncDataService}.
 */
public class ConsulSyncDataServiceTest {

    private ConsulClient consulClient;

    private ConsulSyncDataService consulSyncDataService;

    @Before
    public void setup() {
        consulClient = mock(ConsulClient.class);
        ConsulConfig consulConfig = new ConsulConfig();
        consulConfig.setWaitTime(1000);
        consulConfig.setWatchDelay(1000);
        consulSyncDataService = new ConsulSyncDataService(consulClient, consulConfig, null,
                Collections.emptyList(), Collections.emptyList());
    }

    @Test
    public void testStart() {
        consulSyncDataService.start();
    }

    @Test
    public void testClose() {
        consulSyncDataService.close();
    }

    @Test
    @SneakyThrows
    public void testWatch() {
        Long index = 1L;
        final List<GetValue> list = new ArrayList<>();
        GetValue getValue = mock(GetValue.class);
        when(getValue.getModifyIndex()).thenReturn(index);
        when(getValue.getKey()).thenReturn(ConsulConstants.PLUGIN_DATA);
        when(getValue.getDecodedValue()).thenReturn("{}");
        list.add(getValue);
        Response response = mock(Response.class);
        when(consulClient.getKVValues(any(), any(), any()))
                .thenReturn(response);
        when(response.getValue()).thenReturn(list);
        when(response.getConsulIndex()).thenReturn(1L);

        Method watchConfigKeyValues = consulSyncDataService.getClass().getDeclaredMethod("watchConfigKeyValues");
        watchConfigKeyValues.setAccessible(true);
        watchConfigKeyValues.invoke(consulSyncDataService);

        Map<String, Long> consulIndexes = (Map<String, Long>) ReflectUtils.getFieldValue(consulSyncDataService, "consulIndexes");
        Assert.assertEquals(index, consulIndexes.get(ConsulConstants.SYNC_PRE_FIX));
    }
}
