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
import com.alibaba.nacos.api.exception.NacosException;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.apache.shenyu.common.constant.NacosPathConstants.AUTH_DATA_ID;
import static org.apache.shenyu.common.constant.NacosPathConstants.META_DATA_ID;
import static org.apache.shenyu.common.constant.NacosPathConstants.PLUGIN_DATA_ID;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link NacosDataChangedInit}.
 */
@ExtendWith(MockitoExtension.class)
public class NacosDataChangedInitTest {

    @Mock
    private ConfigService configService;

    @Test
    public void testNotExist() throws NacosException {
        String group = NacosPathConstants.GROUP;
        long timeout = NacosPathConstants.DEFAULT_TIME_OUT;
        NacosDataChangedInit nacosDataChangedInit = new NacosDataChangedInit(configService);

        when(configService.getConfig(PLUGIN_DATA_ID, group, timeout)).thenReturn(PLUGIN_DATA_ID);
        boolean pluginExist = nacosDataChangedInit.notExist();
        assertFalse(pluginExist, "plugin exist.");
        when(configService.getConfig(PLUGIN_DATA_ID, group, timeout)).thenReturn(null);

        when(configService.getConfig(AUTH_DATA_ID, group, timeout)).thenReturn(AUTH_DATA_ID);
        boolean authExist = nacosDataChangedInit.notExist();
        assertFalse(authExist, "auth exist.");
        when(configService.getConfig(AUTH_DATA_ID, group, timeout)).thenReturn(null);

        when(configService.getConfig(META_DATA_ID, group, timeout)).thenReturn(META_DATA_ID);
        boolean metaDataExist = nacosDataChangedInit.notExist();
        assertFalse(metaDataExist, "metadata exist.");
        when(configService.getConfig(META_DATA_ID, group, timeout)).thenReturn(null);
        boolean metaDataNotExist = nacosDataChangedInit.notExist();
        assertTrue(metaDataNotExist, "metadata not exist.");
    }
}
