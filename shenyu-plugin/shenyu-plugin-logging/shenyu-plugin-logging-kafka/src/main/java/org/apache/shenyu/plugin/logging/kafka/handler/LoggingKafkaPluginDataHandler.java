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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.kafka.cache.KafkaClientCache;
import org.apache.shenyu.plugin.logging.kafka.client.KafkaLogCollectClient;
import org.apache.shenyu.plugin.logging.kafka.collector.KafkaLogCollector;
import org.apache.shenyu.plugin.logging.kafka.config.KafkaLogCollectConfig;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;


/**
 * The type logging kafka plugin data handler.
 */
public class LoggingKafkaPluginDataHandler extends AbstractLogPluginDataHandler<KafkaLogCollectConfig.KafkaLogConfig, KafkaLogCollectConfig.LogApiConfig> {

    private static final KafkaLogCollectClient KAFKA_LOG_COLLECT_CLIENT = new KafkaLogCollectClient();

    private static final AtomicBoolean MULTI_CLIENT = new AtomicBoolean(false);

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return KafkaLogCollector.getInstance();
    }

    /**
     * getMultiClient.
     *
     * @return multiClient
     */
    public static boolean getMultiClient() {
        return MULTI_CLIENT.get();
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

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        Map<String, Object> kafkaJsonMap = GsonUtils.getInstance().convertToMap(selectorData.getHandle());
        if (Objects.isNull(kafkaJsonMap)) {
            return;
        }
        Object hostObj = kafkaJsonMap.get("bootstrapServer");
        if (Objects.isNull(hostObj) || !(hostObj instanceof String) || ((String) hostObj).trim().isEmpty()) {
            KafkaClientCache.getInstance().invalidate(selectorData.getId());
            Optional.ofNullable(KafkaClientCache.getInstance().getClientCache())
                    .filter(Map::isEmpty)
                    .ifPresent(map -> MULTI_CLIENT.set(false));
            return;
        }
        KafkaLogCollectConfig.LogApiConfig nConfig = GsonUtils.getInstance().fromJson(selectorData.getHandle(), KafkaLogCollectConfig.LogApiConfig.class);
        KafkaLogCollectConfig.LogApiConfig oConfig = (KafkaLogCollectConfig.LogApiConfig) getSelectApiConfigMap().get(selectorData.getId());
        if (Objects.equals(nConfig, oConfig)) {
            return;
        }
        KafkaClientCache.getInstance().invalidate(selectorData.getId());
        KafkaClientCache.getInstance().initKafkaClient(selectorData.getId(), nConfig);
        MULTI_CLIENT.set(true);
        if (StringUtils.isNotEmpty(nConfig.getSampleRate())) {
            nConfig.setSampler(LogCollectConfigUtils.setSampler(nConfig.getSampleRate()));
        }
        getSelectApiConfigMap().put(selectorData.getId(), nConfig);
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        KafkaClientCache.getInstance().invalidateAll();
        super.removePlugin(pluginData);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        KafkaClientCache.getInstance().invalidate(selectorData.getId());
        super.removeSelector(selectorData);
    }
}
