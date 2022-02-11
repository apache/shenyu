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

import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.boot.CommandLineRunner;

/**
 * The type Zookeeper data init.
 */
public class ZookeeperDataInit implements CommandLineRunner {

    private final ZkClient zkClient;

    private final SyncDataService syncDataService;

    /**
     * Instantiates a new Zookeeper data init.
     *
     * @param zkClient        the zk client
     * @param syncDataService the sync data service
     */
    public ZookeeperDataInit(final ZkClient zkClient, final SyncDataService syncDataService) {
        this.zkClient = zkClient;
        this.syncDataService = syncDataService;
    }

    @Override
    public void run(final String... args) {
        String pluginPath = DefaultPathConstants.PLUGIN_PARENT;
        String authPath = DefaultPathConstants.APP_AUTH_PARENT;
        String metaDataPath = DefaultPathConstants.META_DATA;

        boolean zkPathNotExist = !zkClient.exists(pluginPath)
                && !zkClient.exists(authPath)
                && !zkClient.exists(metaDataPath);

        if (zkPathNotExist) {
            syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        }
    }
}
