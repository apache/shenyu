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

package org.apache.shenyu.plugin.tencent.cls.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.tencent.cls.client.TencentClsLogCollectClient;
import org.apache.shenyu.plugin.tencent.cls.collector.TencentClsSlsLogCollector;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.tencent.cls.config.TencentLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Properties;

/**
 * LoggingTencentClsPluginDataHandler Tencent cls plugin data handler.
 */
public class LoggingTencentClsPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingTencentClsPluginDataHandler.class);

    private static final TencentClsLogCollectClient TENCENT_CLS_LOG_COLLECT_CLIENT = new TencentClsLogCollectClient();

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        LOG.info("Tencent cls plugin data: {}", GsonUtils.getGson().toJson(pluginData));
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            TencentLogCollectConfig.TencentClsLogConfig globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                    TencentLogCollectConfig.TencentClsLogConfig.class);
            TencentLogCollectConfig.TencentClsLogConfig exist = Singleton.INST.get(TencentLogCollectConfig.TencentClsLogConfig.class);
            if (Objects.isNull(globalLogConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !globalLogConfig.equals(exist)) {
                // no data, init client
                TencentLogCollectConfig.INSTANCE.setTencentClsLogConfig(globalLogConfig);
                Properties properties = new Properties();
                properties.setProperty(GenericLoggingConstant.SECRET_ID, globalLogConfig.getSecretId().trim());
                properties.setProperty(GenericLoggingConstant.SECRET_KEY, globalLogConfig.getSecretKey().trim());
                properties.setProperty(GenericLoggingConstant.ENDPOINT, globalLogConfig.getEndpoint().trim());
                properties.setProperty(GenericLoggingConstant.TOPIC, globalLogConfig.getTopic().trim());
                properties.setProperty(GenericLoggingConstant.SEND_THREAD_COUNT, String.valueOf(globalLogConfig.getSendThreadCount()));

                // Optional parameters
                properties.setProperty(GenericLoggingConstant.TOTAL_SIZE_IN_BYTES, globalLogConfig.getTotalSizeInBytes());
                properties.setProperty(GenericLoggingConstant.MAX_SEND_THREAD_COUNT, globalLogConfig.getMaxSendThreadCount());
                properties.setProperty(GenericLoggingConstant.MAX_BLOCK_SEC, globalLogConfig.getMaxBlockSec());
                properties.setProperty(GenericLoggingConstant.MAX_BATCH_SIZE, globalLogConfig.getMaxBatchSize());
                properties.setProperty(GenericLoggingConstant.MAX_BATCH_COUNT, globalLogConfig.getMaxBatchCount());
                properties.setProperty(GenericLoggingConstant.LINGER_MS, globalLogConfig.getLingerMs());
                properties.setProperty(GenericLoggingConstant.RETRIES, globalLogConfig.getRetries());
                properties.setProperty(GenericLoggingConstant.MAX_RESERVED_ATTEMPTS, globalLogConfig.getMaxReservedAttempts());
                properties.setProperty(GenericLoggingConstant.BASE_RETRY_BACKOFF_MS, globalLogConfig.getBaseRetryBackoffMs());
                properties.setProperty(GenericLoggingConstant.MAX_RETRY_BACKOFF_MS, globalLogConfig.getMaxRetryBackoffMs());
                // init tencent cls client
                TENCENT_CLS_LOG_COLLECT_CLIENT.initClient(properties);
                TencentClsSlsLogCollector.getInstance().start();
                Singleton.INST.single(TencentLogCollectConfig.TencentClsLogConfig.class, globalLogConfig);
            }
        } else {
            try {
                TencentClsSlsLogCollector.getInstance().close();
            } catch (Exception e) {
                LOG.error("close log collector error", e);
            }
        }
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_TENCENT_CLS.getName();
    }

    /**
     * get Tencent log collect client.
     * @return Tencent cls log collect client.
     */
    public static TencentClsLogCollectClient getTencentClsLogCollectClient() {
        return TENCENT_CLS_LOG_COLLECT_CLIENT;
    }
}
