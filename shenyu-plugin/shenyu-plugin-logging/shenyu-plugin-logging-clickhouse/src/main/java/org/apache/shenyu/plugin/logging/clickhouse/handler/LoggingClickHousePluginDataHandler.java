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

package org.apache.shenyu.plugin.logging.clickhouse.handler;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.clickhouse.client.ClickHouseLogCollectClient;
import org.apache.shenyu.plugin.logging.clickhouse.collector.ClickHouseLogCollector;
import org.apache.shenyu.plugin.logging.clickhouse.config.ClickHouseLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;

/**
 * The type logging pulsar plugin data handler.
 */
public class LoggingClickHousePluginDataHandler extends AbstractLogPluginDataHandler<ClickHouseLogCollectConfig.ClickHouseLogConfig, GenericApiConfig> {

    private static final ClickHouseLogCollectClient CLICK_HOUSE_LOG_COLLECT_CLIENT = new ClickHouseLogCollectClient();

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return ClickHouseLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final ClickHouseLogCollectConfig.ClickHouseLogConfig globalLogConfig) {
        ClickHouseLogCollectConfig.INSTANCE.setClickHouseLogConfig(globalLogConfig);
        CLICK_HOUSE_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_CLICK_HOUSE.getName();
    }

    /**
     * getClickHouseLogCollectClient.
     *
     * @return LogConsumeClient
     */
    public static ClickHouseLogCollectClient getClickHouseLogCollectClient() {
        return CLICK_HOUSE_LOG_COLLECT_CLIENT;
    }

}
