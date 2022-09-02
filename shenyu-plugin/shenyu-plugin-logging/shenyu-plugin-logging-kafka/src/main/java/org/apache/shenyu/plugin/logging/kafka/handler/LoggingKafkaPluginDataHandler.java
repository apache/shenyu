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

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.collector.KafkaLogCollector;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;


/**
 * The type logging kafka plugin data handler.
 */
public class LoggingKafkaPluginDataHandler extends AbstractLogPluginDataHandler<KafkaLogCollectConfig.KafkaLogConfig, KafkaLogCollectConfig.LogApiConfig> {

    private static final KafkaLogCollectClient KAFKA_LOG_COLLECT_CLIENT = new KafkaLogCollectClient();

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
    protected void doRefreshConfig(final KafkaLogCollectConfig.KafkaLogConfig globalLogConfig) {
        KafkaLogCollectConfig.INSTANCE.setKafkaLogConfig(globalLogConfig);
        KAFKA_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    private Properties initProperties(final KafkaLogCollectConfig.KafkaLogConfig globalLogConfig) {
        //init kafka properties
        Properties properties = new Properties();
        properties.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        properties.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
        properties.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, globalLogConfig.getNamesrvAddr());
        properties.put(GenericLoggingConstant.TOPIC, globalLogConfig.getTopic());
        properties.put(GenericLoggingConstant.NAMESERVER_ADDRESS, globalLogConfig.getTopic());
        if (!StringUtils.isBlank(globalLogConfig.getSecurityProtocol()) && !StringUtils.isBlank(globalLogConfig.getSaslMechanism())) {
            properties.put(CommonClientConfigs.SECURITY_PROTOCOL_CONFIG, globalLogConfig.getSecurityProtocol());
            properties.put(SaslConfigs.SASL_MECHANISM, globalLogConfig.getSaslMechanism());
            properties.put(SaslConfigs.SASL_JAAS_CONFIG,
                    MessageFormat
                            .format("org.apache.kafka.common.security.scram.ScramLoginModule required username=\"{0}\" password=\"{1}\";",
                                    globalLogConfig.getUserName(), globalLogConfig.getPassWord()));
        }
        return properties;
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_KAFKA.getName();
    }

    /**
     * get kafka log collect client.
     *
     * @return kafka log collect client.
     */
    public static KafkaLogCollectClient getKafkaLogCollectClient() {
        return KAFKA_LOG_COLLECT_CLIENT;
    }
}
