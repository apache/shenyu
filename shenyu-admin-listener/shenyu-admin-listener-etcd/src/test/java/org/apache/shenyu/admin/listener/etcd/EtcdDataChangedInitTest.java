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

package org.apache.shenyu.admin.listener.etcd;

import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link EtcdDataChangedInit}.
 */
@ExtendWith(MockitoExtension.class)
public class EtcdDataChangedInitTest {

    @Mock
    private EtcdClient etcdClient;

    @Test
    public void testNotExist() throws Exception {
        EtcdDataChangedInit etcdDataChangedInit = new EtcdDataChangedInit(etcdClient);
        assertNotNull(etcdDataChangedInit);

        when(etcdClient.exists(DefaultPathConstants.PLUGIN_PARENT)).thenReturn(true);
        boolean pluginExist = etcdDataChangedInit.notExist();
        assertFalse(pluginExist, "plugin exist.");
        when(etcdClient.exists(DefaultPathConstants.PLUGIN_PARENT)).thenReturn(false);

        when(etcdClient.exists(DefaultPathConstants.APP_AUTH_PARENT)).thenReturn(true);
        boolean appAuthExist = etcdDataChangedInit.notExist();
        assertFalse(appAuthExist, "app auth exist.");
        when(etcdClient.exists(DefaultPathConstants.APP_AUTH_PARENT)).thenReturn(false);

        when(etcdClient.exists(DefaultPathConstants.META_DATA)).thenReturn(true);
        boolean metaDataExist = etcdDataChangedInit.notExist();
        assertFalse(metaDataExist, "metadata exist.");
        when(etcdClient.exists(DefaultPathConstants.META_DATA)).thenReturn(false);
        boolean metaDataNotExist = etcdDataChangedInit.notExist();
        assertTrue(metaDataNotExist, "metadata not exist.");
    }
}
