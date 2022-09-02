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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.collector.KafkaLogCollector;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type logging kafka plugin data handler.
 */
public class LoggingKafkaPluginDataHandler extends AbstractLogPluginDataHandler<KafkaLogCollectConfig.KafkaLogConfig> {

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
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return KafkaLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(KafkaLogCollectConfig.KafkaLogConfig globalLogConfig) {
        KafkaLogCollectConfig.INSTANCE.setKafkaLogConfig(globalLogConfig);
        Properties properties = new Properties();
        properties.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        properties.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        properties.put("bootstrap.servers", globalLogConfig.getNamesrvAddr());
        KAFKA_LOG_COLLECT_CLIENT.initProducer(properties);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_KAFKA.getName();
    }
}
