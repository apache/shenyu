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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.rocketmq.client.RocketMQLogCollectClient;
import org.apache.shenyu.plugin.logging.rocketmq.collector.RocketMQLogCollector;
import org.apache.shenyu.plugin.logging.rocketmq.config.RocketMQLogCollectConfig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The type logging rocketmq plugin data handler.
 */
public class LoggingRocketMQPluginDataHandler extends AbstractLogPluginDataHandler<RocketMQLogCollectConfig.RocketMQLogConfig, GenericApiConfig> {

    private static final RocketMQLogCollectClient ROCKET_MQ_LOG_COLLECT_CLIENT = new RocketMQLogCollectClient();

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
    protected void doRefreshConfig(final RocketMQLogCollectConfig.RocketMQLogConfig globalLogConfig) {
        RocketMQLogCollectConfig.INSTANCE.setRocketMQLogConfig(globalLogConfig);
        ROCKET_MQ_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    @Override
    protected void doRefreshSelectorConfig() {
        Map<String, GenericApiConfig> selectApiConfigMap = LoggingRocketMQPluginDataHandler.getSelectApiConfigMap();
        Map<String, List<String>> selectIdUriListMap = LoggingRocketMQPluginDataHandler.getSelectIdUriListMap();
        Set<String> selectorIdSet = selectIdUriListMap.keySet();
        Map<String, String> apiTopicMap = new HashMap<>();
        for (String selectorId : selectorIdSet) {
            List<String> list = selectIdUriListMap.get(selectorId);
            for (String l : list) {
                if (StringUtils.isNotEmpty(selectApiConfigMap.get(selectorId).getTopic())) {
                    apiTopicMap.put(l, selectApiConfigMap.get(selectorId).getTopic());
                }
            }
        }
        RocketMQLogCollectClient.setTopic(apiTopicMap);
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
