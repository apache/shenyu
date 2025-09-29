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

package org.apache.shenyu.plugin.logging.kafka.collector;

import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.apache.shenyu.plugin.logging.kafka.cache.KafkaClientCache;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;
import org.apache.shenyu.plugin.logging.kafka.handler.LoggingKafkaPluginDataHandler;

import java.util.Objects;

/**
 * kafka log collector，depend a LogConsumeClient for consume logs.
 */
public class KafkaLogCollector extends AbstractLogCollector<KafkaLogCollectClient, ShenyuRequestLog, KafkaLogCollectConfig.KafkaLogConfig> {

    private static final LogCollector<ShenyuRequestLog> INSTANCE = new KafkaLogCollector();

    /**
     * get LogCollector instance.
     *
     * @return LogCollector instance
     */
    public static LogCollector<ShenyuRequestLog> getInstance() {
        return INSTANCE;
    }

    @Override
    protected KafkaLogCollectClient getLogConsumeClient() {
        return LoggingKafkaPluginDataHandler.getKafkaLogCollectClient();
    }

    @Override
    public KafkaLogCollectClient getLogConsumeClient(final String path) {
        KafkaLogCollectClient kafkaClient = KafkaClientCache.getInstance().getKafkaClient(path);
        if (Objects.isNull(kafkaClient)) {
            return getLogConsumeClient();
        }
        return kafkaClient;
    }

    @Override
    protected boolean getMultiClient() {
        return LoggingKafkaPluginDataHandler.getMultiClient();
    }

    @Override
    protected KafkaLogCollectConfig.KafkaLogConfig getLogCollectConfig() {
        return KafkaLogCollectConfig.INSTANCE.getKafkaLogConfig();
    }

    @Override
    protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
    }
}
