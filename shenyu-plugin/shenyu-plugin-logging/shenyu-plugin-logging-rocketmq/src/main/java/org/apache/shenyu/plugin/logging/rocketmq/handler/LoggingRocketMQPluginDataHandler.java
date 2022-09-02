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

package org.apache.shenyu.plugin.logging.rocketmq.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.handler.AbstractPluginDataHandler;
import org.apache.shenyu.plugin.logging.rocketmq.client.RocketMQLogCollectClient;
import org.apache.shenyu.plugin.logging.rocketmq.collector.RocketMQLogCollector;
import org.apache.shenyu.plugin.logging.rocketmq.config.RocketMQLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type logging rocketmq plugin data handler.
 */
public class LoggingRocketMQPluginDataHandler extends AbstractLogPluginDataHandler<RocketMQLogCollectConfig.RocketMQLogConfig> {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingRocketMQPluginDataHandler.class);

    private static final RocketMQLogCollectClient ROCKET_MQ_LOG_COLLECT_CLIENT = new RocketMQLogCollectClient();

    public LoggingRocketMQPluginDataHandler() {
        super(RocketMQLogCollectConfig.LogApiConfig.class);
    }

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return RocketMQLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(RocketMQLogCollectConfig.RocketMQLogConfig globalLogConfig) {
        RocketMQLogCollectConfig.INSTANCE.setRocketMQLogConfig(globalLogConfig);
        Properties properties = new Properties();
        properties.setProperty(GenericLoggingConstant.TOPIC, globalLogConfig.getTopic());
        properties.setProperty(GenericLoggingConstant.NAMESERVER_ADDRESS, globalLogConfig.getNamesrvAddr());
        properties.setProperty(GenericLoggingConstant.PRODUCER_GROUP, globalLogConfig.getProducerGroup());
        ROCKET_MQ_LOG_COLLECT_CLIENT.initProducer(properties);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_ROCKETMQ.getName();
    }

    /**
     * get rocketmq log collect client.
     *
     * @return rocketmq log collect client.
     */
    public static RocketMQLogCollectClient getRocketMqLogCollectClient() {
        return ROCKET_MQ_LOG_COLLECT_CLIENT;
    }
}
