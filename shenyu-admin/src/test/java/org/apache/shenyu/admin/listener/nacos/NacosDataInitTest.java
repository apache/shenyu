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

package org.apache.shenyu.admin.listener.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import lombok.SneakyThrows;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link NacosDataInit}.
 */
@RunWith(MockitoJUnitRunner.class)
public class NacosDataInitTest {

    private static final String PLUGIN_DATA_ID = "PLUGIN_DATA_ID";

    private static final String AUTH_DATA_ID = "AUTH_DATA_ID";

    private static final String META_DATA_ID = "META_DATA_ID";

    @Mock
    private ConfigService configService;

    @Mock
    private SyncDataService syncDataService;

    @Test
    @SneakyThrows
    public void testRun() {
        String group = NacosPathConstants.GROUP;
        long timeout = NacosPathConstants.DEFAULT_TIME_OUT;
        NacosDataInit nacosDataInit = new NacosDataInit(configService, syncDataService);

        String pluginDataId = NacosPathConstants.PLUGIN_DATA_ID;
        when(configService.getConfig(pluginDataId, group, timeout)).thenReturn(null);
        nacosDataInit.run();
        when(configService.getConfig(pluginDataId, group, timeout)).thenReturn(PLUGIN_DATA_ID);
        nacosDataInit.run();

        reset(configService);
        String authDataId = NacosPathConstants.AUTH_DATA_ID;
        when(configService.getConfig(authDataId, group, timeout)).thenReturn(null);
        nacosDataInit.run();
        when(configService.getConfig(authDataId, group, timeout)).thenReturn(AUTH_DATA_ID);
        nacosDataInit.run();

        reset(configService);
        String metaDataId = NacosPathConstants.META_DATA_ID;
        when(configService.getConfig(metaDataId, group, timeout)).thenReturn(null);
        nacosDataInit.run();
        when(configService.getConfig(metaDataId, group, timeout)).thenReturn(META_DATA_ID);
        nacosDataInit.run();

        verify(syncDataService, times(3)).syncAll(any(DataEventTypeEnum.class));
    }
}
