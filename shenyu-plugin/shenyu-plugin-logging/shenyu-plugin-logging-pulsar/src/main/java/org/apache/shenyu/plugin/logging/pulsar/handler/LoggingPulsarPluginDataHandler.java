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

package org.apache.shenyu.plugin.logging.pulsar.handler;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.pulsar.client.PulsarLogCollectClient;
import org.apache.shenyu.plugin.logging.pulsar.collector.PulsarLogCollector;
import org.apache.shenyu.plugin.logging.pulsar.config.PulsarLogCollectConfig;


/**
 * The type logging pulsar plugin data handler.
 */
public class LoggingPulsarPluginDataHandler extends AbstractLogPluginDataHandler<PulsarLogCollectConfig.PulsarLogConfig, PulsarLogCollectConfig.LogApiConfig> {

    private static final PulsarLogCollectClient PULSAR_LOG_COLLECT_CLIENT = new PulsarLogCollectClient();

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return PulsarLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final PulsarLogCollectConfig.PulsarLogConfig globalLogConfig) {
        PulsarLogCollectConfig.INSTANCE.setPulsarLogConfig(globalLogConfig);
        PULSAR_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_PULSAR.getName();
    }

    /**
     * get pulsar log collect client.
     * @return pulsar log collect client
     */
    public static PulsarLogCollectClient getPulsarLogCollectClient() {
        return PULSAR_LOG_COLLECT_CLIENT;
    }
}
