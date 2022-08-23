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
import org.apache.shenyu.common.constant.ConsulConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ConsulDataChangedInit}.
 */
@ExtendWith(MockitoExtension.class)
public class ConsulDataChangedInitTest {

    @Mock
    private ConsulClient consulClient;

    @Test
    public void testNotExist() throws Exception {
        ConsulDataChangedInit consulDataChangedInit = new ConsulDataChangedInit(consulClient);
        assertNotNull(consulDataChangedInit);

        Response<GetValue> pluginResponse = mock(Response.class);
        when(consulClient.getKVValue(ConsulConstants.PLUGIN_DATA)).thenReturn(pluginResponse);
        when(pluginResponse.getValue()).thenReturn(new GetValue());
        boolean pluginExist = consulDataChangedInit.notExist();
        assertFalse(pluginExist, "plugin exist.");
        when(pluginResponse.getValue()).thenReturn(null);

        Response<GetValue> appAuthResponse = mock(Response.class);
        when(consulClient.getKVValue(ConsulConstants.AUTH_DATA)).thenReturn(appAuthResponse);
        when(appAuthResponse.getValue()).thenReturn(new GetValue());
        boolean appAuthExist = consulDataChangedInit.notExist();
        assertFalse(appAuthExist, "app auth exist.");
        when(appAuthResponse.getValue()).thenReturn(null);

        Response<GetValue> mataDataResponse = mock(Response.class);
        when(consulClient.getKVValue(ConsulConstants.META_DATA)).thenReturn(mataDataResponse);
        when(mataDataResponse.getValue()).thenReturn(new GetValue());
        boolean metaDataExist = consulDataChangedInit.notExist();
        assertFalse(metaDataExist, "metadata exist.");
        when(mataDataResponse.getValue()).thenReturn(null);
        boolean metaDataNotExist = consulDataChangedInit.notExist();
        assertTrue(metaDataNotExist, "metadata not exist.");
    }
}
