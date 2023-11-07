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

package org.apache.shenyu.plugin.logging.rabbitmq.handler;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.apache.shenyu.plugin.logging.rabbitmq.conllector.RabbitmqLogCollector;

/**
 * The type logging rabbitmq plugin data handler.
 */
public class LoggingRabbitmqPluginDataHandler extends AbstractLogPluginDataHandler<RabbitmqLogCollectConfig.RabbitmqLogConfig, RabbitmqLogCollectConfig.LogApiConfig> {

    private static final RabbitmqLogCollectClient RABBITMQ_LOG_COLLECT_CLIENT = new RabbitmqLogCollectClient();

    /**
     * get rabbitmq log collect client.
     *
     * @return rabbitmq log collect client
     */
    public static RabbitmqLogCollectClient getRabbitmqLogCollectClient() {
        return RABBITMQ_LOG_COLLECT_CLIENT;
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_RABBITMQ.getName();
    }

    /**
     * get RabbitmqLogCollector.
     *
     * @return RabbitmqLogCollector
     */
    @Override
    protected LogCollector logCollector() {
        return RabbitmqLogCollector.getInstance();
    }

    /**
     * refresh the config.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final RabbitmqLogCollectConfig.RabbitmqLogConfig globalLogConfig) {
        RabbitmqLogCollectConfig.INSTANCE.setRabbitmqLogConfig(globalLogConfig);
        RABBITMQ_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }
}
