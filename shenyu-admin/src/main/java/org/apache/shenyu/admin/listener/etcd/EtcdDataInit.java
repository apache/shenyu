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

import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;

/**
 * EtcdDataInit.
 */
public class EtcdDataInit implements CommandLineRunner {

    private static final Logger LOG = LoggerFactory.getLogger(EtcdDataInit.class);

    private final EtcdClient etcdClient;

    private final SyncDataService syncDataService;

    public EtcdDataInit(final EtcdClient client, final SyncDataService syncDataService) {
        this.etcdClient = client;
        this.syncDataService = syncDataService;
    }

    @Override
    public void run(final String... args) throws Exception {
        final String pluginPath = DefaultPathConstants.PLUGIN_PARENT;
        final String authPath = DefaultPathConstants.APP_AUTH_PARENT;
        final String metaDataPath = DefaultPathConstants.META_DATA;
        if (!etcdClient.exists(pluginPath) && !etcdClient.exists(authPath) && !etcdClient.exists(metaDataPath)) {
            LOG.info("Init all data from database");
            syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        }
    }
}
