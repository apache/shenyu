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

package org.apache.shenyu.plugin.logging.rabbitmq.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.rabbitmq.cache.RabbitmqClientCache;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.apache.shenyu.plugin.logging.rabbitmq.conllector.RabbitmqLogCollector;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type logging rabbitmq plugin data handler.
 */
public class LoggingRabbitmqPluginDataHandler extends AbstractLogPluginDataHandler<RabbitmqLogCollectConfig.RabbitmqLogConfig, RabbitmqLogCollectConfig.LogApiConfig> {

    private static final RabbitmqLogCollectClient RABBITMQ_LOG_COLLECT_CLIENT = new RabbitmqLogCollectClient();

    private static final AtomicBoolean MULTI_CLIENT = new AtomicBoolean(false);

    /**
     * get rabbitmq log collect client.
     *
     * @return rabbitmq log collect client
     */
    public static RabbitmqLogCollectClient getRabbitmqLogCollectClient() {
        return RABBITMQ_LOG_COLLECT_CLIENT;
    }

    /**
     * getMultiClient.
     *
     * @return multiClient
     */
    public static boolean getMultiClient() {
        return MULTI_CLIENT.get();
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_RABBITMQ.getName();
    }

    /**
     * get RabbitmqLogCollector.
     *
     * @return RabbitmqLogCollector
     */
    @Override
    protected LogCollector logCollector() {
        return RabbitmqLogCollector.getInstance();
    }

    /**
     * refresh the config.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final RabbitmqLogCollectConfig.RabbitmqLogConfig globalLogConfig) {
        RabbitmqLogCollectConfig.INSTANCE.setRabbitmqLogConfig(globalLogConfig);
        RABBITMQ_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        Map<String, Object> rabbitmqJsonMap = GsonUtils.getInstance().convertToMap(selectorData.getHandle());
        Object hostObj = rabbitmqJsonMap.get("host");
        if (Objects.isNull(hostObj) || !(hostObj instanceof String) || ((String) hostObj).trim().isEmpty()) {
            RabbitmqClientCache.getInstance().invalidate(selectorData.getId());
            Optional.ofNullable(RabbitmqClientCache.getInstance().getClientCache())
                    .filter(Map::isEmpty)
                    .ifPresent(map -> MULTI_CLIENT.set(false));
            return;
        }
        RabbitmqLogCollectConfig.LogApiConfig nConfig = GsonUtils.getInstance().fromJson(selectorData.getHandle(), RabbitmqLogCollectConfig.LogApiConfig.class);
        RabbitmqLogCollectConfig.LogApiConfig oConfig = (RabbitmqLogCollectConfig.LogApiConfig) getSelectApiConfigMap().get(selectorData.getId());
        if (Objects.equals(nConfig, oConfig)) {
            return;
        }
        RabbitmqClientCache.getInstance().invalidate(selectorData.getId());
        if (Objects.isNull(nConfig)) {
            return;
        }
        RabbitmqClientCache.getInstance().initRabbitmqClient(selectorData.getId(), nConfig);
        MULTI_CLIENT.set(true);
        if (StringUtils.isNotEmpty(nConfig.getSampleRate())) {
            nConfig.setSampler(LogCollectConfigUtils.setSampler(nConfig.getSampleRate()));
        }
        getSelectApiConfigMap().put(selectorData.getId(), nConfig);
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        RabbitmqClientCache.getInstance().invalidateAll();
        super.removePlugin(pluginData);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        RabbitmqClientCache.getInstance().invalidate(selectorData.getId());
        super.removeSelector(selectorData);
    }
}
