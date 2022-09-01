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

package org.apache.shenyu.plugin.logging.kafka.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.handler.AbstractPluginDataHandler;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.collector.KafkaLogCollector;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type logging kafka plugin data handler.
 */
public class LoggingKafkaPluginDataHandler extends AbstractPluginDataHandler<KafkaLogCollectConfig.LogApiConfig> {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingKafkaPluginDataHandler.class);

    private static final KafkaLogCollectClient KAFKA_LOG_COLLECT_CLIENT = new KafkaLogCollectClient();

    public LoggingKafkaPluginDataHandler() {
        super(KafkaLogCollectConfig.LogApiConfig.class);
    }

    /**
     * get kafka log collect client.
     *
     * @return kafka log collect client.
     */
    public static KafkaLogCollectClient getKafkaLogCollectClient() {
        return KAFKA_LOG_COLLECT_CLIENT;
    }

    /**
     * start or close kafka client.
     */
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        LOG.info("handler loggingKafka Plugin data:{}", GsonUtils.getGson().toJson(pluginData));
        if (pluginData.getEnabled()) {
            KafkaLogCollectConfig.KafkaLogConfig globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                KafkaLogCollectConfig.KafkaLogConfig.class);

            KafkaLogCollectConfig.INSTANCE.setKafkaLogConfig(globalLogConfig);
            // start kafka producer
            KAFKA_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
            KafkaLogCollector.getInstance().start();
        } else {
            try {
                KafkaLogCollector.getInstance().close();
            } catch (Exception e) {
                LOG.error("close log collector error", e);
            }
        }
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_KAFKA.getName();
    }
}
