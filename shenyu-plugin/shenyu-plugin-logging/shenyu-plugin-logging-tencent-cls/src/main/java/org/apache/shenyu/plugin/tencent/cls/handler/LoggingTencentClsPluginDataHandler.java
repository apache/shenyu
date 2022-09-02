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

package org.apache.shenyu.plugin.tencent.cls.handler;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.tencent.cls.client.TencentClsLogCollectClient;
import org.apache.shenyu.plugin.tencent.cls.collector.TencentClsSlsLogCollector;
import org.apache.shenyu.plugin.tencent.cls.config.TencentLogCollectConfig;

/**
 * LoggingTencentClsPluginDataHandler Tencent cls plugin data handler.
 */
public class LoggingTencentClsPluginDataHandler extends AbstractLogPluginDataHandler<TencentLogCollectConfig.TencentClsLogConfig, GenericApiConfig> {

    private static final TencentClsLogCollectClient TENCENT_CLS_LOG_COLLECT_CLIENT = new TencentClsLogCollectClient();

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return TencentClsSlsLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final TencentLogCollectConfig.TencentClsLogConfig globalLogConfig) {
        TencentLogCollectConfig.INSTANCE.setTencentClsLogConfig(globalLogConfig);
        TENCENT_CLS_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_TENCENT_CLS.getName();
    }

    /**
     * get Tencent log collect client.
     * @return Tencent cls log collect client.
     */
    public static TencentClsLogCollectClient getTencentClsLogCollectClient() {
        return TENCENT_CLS_LOG_COLLECT_CLIENT;
    }
}
