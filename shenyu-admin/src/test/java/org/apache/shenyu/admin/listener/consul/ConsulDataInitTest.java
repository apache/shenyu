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

package org.apache.shenyu.admin.listener.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ConsulDataInit}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ConsulDataInitTest {

    @Mock
    private ConsulClient consulClient;

    @Mock
    private SyncDataService syncDataService;

    @Test
    public void testRun() {
        ConsulDataInit consulDataInit = new ConsulDataInit(consulClient, syncDataService);

        Response<GetValue> response = mock(Response.class);
        when(consulClient.getKVValue(anyString())).thenReturn(response);
        when(response.getValue()).thenReturn(null);
        consulDataInit.run();
        when(response.getValue()).thenReturn(new GetValue());
        consulDataInit.run();

        verify(syncDataService, times(1)).syncAll(any(DataEventTypeEnum.class));
    }
}
