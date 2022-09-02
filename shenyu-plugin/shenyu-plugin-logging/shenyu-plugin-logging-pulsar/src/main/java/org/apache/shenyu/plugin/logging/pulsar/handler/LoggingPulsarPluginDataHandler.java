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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.pulsar.client.PulsarLogCollectClient;
import org.apache.shenyu.plugin.logging.pulsar.collector.PulsarLogCollector;
import org.apache.shenyu.plugin.logging.pulsar.config.PulsarLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;


/**
 * The type logging pulsar plugin data handler.
 */
public class LoggingPulsarPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingPulsarPluginDataHandler.class);

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
    protected void doRefreshConfig(PulsarLogCollectConfig.PulsarLogConfig globalLogConfig) {
        PulsarLogCollectConfig.INSTANCE.setPulsarLogConfig(globalLogConfig);
        Properties properties = new Properties();
        properties.setProperty(GenericLoggingConstant.TOPIC, globalLogConfig.getTopic());
        properties.setProperty(GenericLoggingConstant.SERVICE_URL, globalLogConfig.getServiceUrl());
        PULSAR_LOG_COLLECT_CLIENT.initProducer(properties);
    }


    @Override
    public void handlerSelector(final SelectorData selectorData) {
        super.handlerSelector(selectorData);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        super.removeSelector(selectorData);
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
