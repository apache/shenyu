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

package org.apache.shenyu.plugin.aliyun.sls.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.aliyun.sls.client.AliyunSlsLogCollectClient;
import org.apache.shenyu.plugin.aliyun.sls.collector.AliyunSlsLogCollector;
import org.apache.shenyu.plugin.aliyun.sls.config.AliyunLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;

import java.util.Objects;

/**
 * LoggingAliYunSlsPluginDataHandler aliyun sls plugin data handler.
 */
public class LoggingAliyunSlsPluginDataHandler extends AbstractLogPluginDataHandler<AliyunLogCollectConfig.AliyunSlsLogConfig, GenericApiConfig> {

    private static final AliyunSlsLogCollectClient ALIYUN_SLS_LOG_COLLECT_CLIENT = new AliyunSlsLogCollectClient();

    /**
     * logCollector.
     */
    @Override
    protected LogCollector logCollector() {
        return AliyunSlsLogCollector.getInstance();
    }

    /**
     * doRefreshConfig.
     *
     * @param globalLogConfig globalLogConfig
     */
    @Override
    protected void doRefreshConfig(final AliyunLogCollectConfig.AliyunSlsLogConfig globalLogConfig) {
        AliyunLogCollectConfig.INSTANCE.setAliyunSlsLogConfig(globalLogConfig);
        if (Objects.isNull(globalLogConfig)
                || StringUtils.isBlank(globalLogConfig.getHost())
                || StringUtils.isBlank(globalLogConfig.getAccessId())
                || StringUtils.isBlank(globalLogConfig.getAccessKey())) {
            LOG.error("aliyun sls props is empty. failed init aliyun sls producer");
            return;
        }
        ALIYUN_SLS_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_ALIYUN_SLS.getName();
    }

    /**
     * get elasticsearch log collect client.
     * @return aliyun sls log collect client.
     */
    public static AliyunSlsLogCollectClient getAliyunSlsLogCollectClient() {
        return ALIYUN_SLS_LOG_COLLECT_CLIENT;
    }
}
