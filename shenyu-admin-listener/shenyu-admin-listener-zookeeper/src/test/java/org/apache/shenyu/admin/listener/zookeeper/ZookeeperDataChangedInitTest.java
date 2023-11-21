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

package org.apache.shenyu.admin.listener.zookeeper;

import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ZookeeperDataChangedInit}.
 */
@ExtendWith(MockitoExtension.class)
public class ZookeeperDataChangedInitTest {

    @Mock
    private ZookeeperClient zkClient;

    @Test
    public void testNotExist() {
        ZookeeperDataChangedInit zookeeperDataChangedInit = new ZookeeperDataChangedInit(zkClient);

        when(zkClient.isExist(DefaultPathConstants.PLUGIN_PARENT)).thenReturn(true);
        boolean pluginExist = zookeeperDataChangedInit.notExist();
        assertFalse(pluginExist, "plugin exist.");
        when(zkClient.isExist(DefaultPathConstants.PLUGIN_PARENT)).thenReturn(false);

        when(zkClient.isExist(DefaultPathConstants.APP_AUTH_PARENT)).thenReturn(true);
        boolean appAuthExist = zookeeperDataChangedInit.notExist();
        assertFalse(appAuthExist, "app auth exist.");
        when(zkClient.isExist(DefaultPathConstants.APP_AUTH_PARENT)).thenReturn(false);

        when(zkClient.isExist(DefaultPathConstants.META_DATA)).thenReturn(true);
        boolean metaDataExist = zookeeperDataChangedInit.notExist();
        assertFalse(metaDataExist, "metadata exist.");
        when(zkClient.isExist(DefaultPathConstants.META_DATA)).thenReturn(false);
        boolean metaDataNotExist = zookeeperDataChangedInit.notExist();
        assertTrue(metaDataNotExist, "metadata not exist.");
    }
}
