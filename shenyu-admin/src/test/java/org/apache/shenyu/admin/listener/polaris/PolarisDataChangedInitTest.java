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

package org.apache.shenyu.admin.listener.polaris;

import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.client.internal.DefaultConfigFile;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.apache.shenyu.common.constant.PolarisPathConstants.PLUGIN_DATA_FILE_NAME;
import static org.apache.shenyu.common.constant.PolarisPathConstants.PROXY_SELECTOR_FILE_NAME;
import static org.apache.shenyu.common.constant.PolarisPathConstants.AUTH_DATA_ID_FILE_NAME;
import static org.apache.shenyu.common.constant.PolarisPathConstants.META_DATA_FILE_NAME;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PolarisDataChangedInit}.
 */
@ExtendWith(MockitoExtension.class)
public class PolarisDataChangedInitTest {
    @Mock
    private ConfigFileService polarisConfigFileService;

    @Test
    public void testNotExist() throws PolarisException {
//        ConfigFile configFile = new DefaultConfigFile("namespace", "fileGroup", "fileName", "configFileRepo", "configFileConfig");
//        PolarisDataChangedInit polarisDataChangedInit = new PolarisDataChangedInit(polarisConfigFileService);
//
//        when(polarisConfigFileService.getConfigFile(PolarisPathConstants.NAMESPACE, PolarisPathConstants.FILE_GROUP, PLUGIN_DATA_FILE_NAME)).thenReturn(configFile);
//        boolean pluginExist = polarisDataChangedInit.notExist();
//        assertFalse(pluginExist, "plugin exist.");
//
//        when(polarisConfigFileService.getConfigFile(PolarisPathConstants.NAMESPACE, PolarisPathConstants.FILE_GROUP, AUTH_DATA_ID_FILE_NAME)).thenReturn(configFile);
//        boolean authExist = polarisDataChangedInit.notExist();
//        assertFalse(authExist, "auth exist.");

//        when(polarisConfigFileService.getConfigFile(PolarisPathConstants.NAMESPACE, PolarisPathConstants.FILE_GROUP, META_DATA_FILE_NAME)
//                .hasContent()).thenReturn(Boolean.TRUE);
//        boolean metaExist = polarisDataChangedInit.notExist();
//        assertFalse(metaExist, "meta exist.");
//
//        when(polarisConfigFileService.getConfigFile(PolarisPathConstants.NAMESPACE, PolarisPathConstants.FILE_GROUP, PROXY_SELECTOR_FILE_NAME)
//                .hasContent()).thenReturn(Boolean.TRUE);
//        boolean proxyExist = polarisDataChangedInit.notExist();
//        assertFalse(proxyExist, "proxy exist.");
//
//        when(polarisConfigFileService.getConfigFile(PolarisPathConstants.NAMESPACE, PolarisPathConstants.FILE_GROUP, PROXY_SELECTOR_FILE_NAME)
//                .hasContent()).thenReturn(Boolean.FALSE);
//        boolean proxyNotExist = polarisDataChangedInit.notExist();
//        assertTrue(proxyNotExist, "proxy not exist.");
    }
}
