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
public class LoggingKafkaPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingKafkaPluginDataHandler.class);

    private static final KafkaLogCollectClient KAFKA_LOG_COLLECT_CLIENT = new KafkaLogCollectClient();

    private static final String EMPTY_JSON = "{}";

    private static final Map<String, List<String>> SELECT_ID_URI_LIST_MAP = new ConcurrentHashMap<>();

    private static final Map<String, KafkaLogCollectConfig.LogApiConfig> SELECT_API_CONFIG_MAP = new ConcurrentHashMap<>();

    /**
     * get kafka log collect client.
     *
     * @return kafka log collect client.
     */
    public static KafkaLogCollectClient getKafkaLogCollectClient() {
        return KAFKA_LOG_COLLECT_CLIENT;
    }

    /**
     * get selectId uriList map.
     *
     * @return selectId uriList map
     */
    public static Map<String, List<String>> getSelectIdUriListMap() {
        return SELECT_ID_URI_LIST_MAP;
    }

    /**
     * get select api config map.
     *
     * @return select api config map
     */
    public static Map<String, KafkaLogCollectConfig.LogApiConfig> getSelectApiConfigMap() {
        return SELECT_API_CONFIG_MAP;
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
            Properties properties = new Properties();
            properties.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
            properties.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
            properties.put("bootstrap.servers", globalLogConfig.getNamesrvAddr());
            KAFKA_LOG_COLLECT_CLIENT.initProducer(properties);
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
    public void handlerSelector(final SelectorData selectorData) {
        LOG.info("handler loggingKafka selector data:{}", GsonUtils.getGson().toJson(selectorData));
        String handleJson = selectorData.getHandle();
        if (StringUtils.isEmpty(handleJson) || EMPTY_JSON.equals(handleJson.trim())) {
            return;
        }
        if (selectorData.getType() != SelectorTypeEnum.CUSTOM_FLOW.getCode()
            || CollectionUtils.isEmpty(selectorData.getConditionList())) {
            return;
        }
        KafkaLogCollectConfig.LogApiConfig logApiConfig = GsonUtils.getInstance().fromJson(handleJson,
            KafkaLogCollectConfig.LogApiConfig.class);
        if (StringUtils.isBlank(logApiConfig.getTopic()) || StringUtils.isBlank(logApiConfig.getSampleRate())) {
            return;
        }
        List<String> uriList = new ArrayList<>();
        for (ConditionData conditionData : selectorData.getConditionList()) {
            if ("uri".equals(conditionData.getParamType()) && StringUtils.isNotBlank(conditionData.getParamValue())
                && ("match".equals(conditionData.getOperator()) || "=".equals(conditionData.getOperator()))) {
                uriList.add(conditionData.getParamValue().trim());
            }
        }
        SELECT_ID_URI_LIST_MAP.put(selectorData.getId(), uriList);
        SELECT_API_CONFIG_MAP.put(selectorData.getId(), logApiConfig);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        LOG.info("handler remove loggingKafka selector data:{}", GsonUtils.getGson().toJson(selectorData));
        SELECT_ID_URI_LIST_MAP.remove(selectorData.getId());
        SELECT_API_CONFIG_MAP.remove(selectorData.getId());
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_KAFKA.getName();
    }
}
