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
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;

/**
 * The type Nacos data init.
 */
public class NacosDataInit implements CommandLineRunner {

    private static final Logger LOG = LoggerFactory.getLogger(NacosDataInit.class);

    private final ConfigService configService;

    private final SyncDataService syncDataService;

    /**
     * Instantiates a new Nacos data init.
     * @param configService the nacos config service
     * @param syncDataService the sync data service
     */
    public NacosDataInit(final ConfigService configService, final SyncDataService syncDataService) {
        this.configService = configService;
        this.syncDataService = syncDataService;
    }

    @Override
    public void run(final String... args) {
        String pluginDataId = NacosPathConstants.PLUGIN_DATA_ID;
        String authDataId = NacosPathConstants.AUTH_DATA_ID;
        String metaDataId = NacosPathConstants.META_DATA_ID;
        if (dataIdNotExist(pluginDataId) && dataIdNotExist(authDataId) && dataIdNotExist(metaDataId)) {
            syncDataService.syncAll(DataEventTypeEnum.REFRESH);
        }
    }

    private boolean dataIdNotExist(final String pluginDataId) {
        try {
            String group = NacosPathConstants.GROUP;
            long timeout = NacosPathConstants.DEFAULT_TIME_OUT;
            return configService.getConfig(pluginDataId, group, timeout) == null;
        } catch (NacosException e) {
            LOG.error("Get data from nacos error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}
