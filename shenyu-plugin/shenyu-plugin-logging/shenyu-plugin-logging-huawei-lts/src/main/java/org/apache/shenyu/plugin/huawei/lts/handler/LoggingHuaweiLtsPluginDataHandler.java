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

package org.apache.shenyu.plugin.huawei.lts.handler;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClient;
import org.apache.shenyu.plugin.huawei.lts.collector.HuaweiLtsLogCollector;
import org.apache.shenyu.plugin.huawei.lts.config.HuaweiLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;

/**
 * LoggingHuaweiLtsPluginDataHandler Huawei lts plugin data handler.
 */
public class LoggingHuaweiLtsPluginDataHandler extends AbstractLogPluginDataHandler<HuaweiLogCollectConfig.HuaweiLtsLogConfig, GenericApiConfig> {

    private static final HuaweiLtsLogCollectClient HUAWEI_LTS_LOG_COLLECT_CLIENT = new HuaweiLtsLogCollectClient();

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_HUAWEI_LTS.getName();
    }

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return HuaweiLtsLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final HuaweiLogCollectConfig.HuaweiLtsLogConfig globalLogConfig) {
        HuaweiLogCollectConfig.INSTANCE.setHuaweiLtsLogConfig(globalLogConfig);
        HUAWEI_LTS_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    /**
     * get Huawei log collect client.
     * @return Huawei lts log collect client.
     */
    public static HuaweiLtsLogCollectClient getHuaweiLtsLogCollectClient() {
        return HUAWEI_LTS_LOG_COLLECT_CLIENT;
    }
}
