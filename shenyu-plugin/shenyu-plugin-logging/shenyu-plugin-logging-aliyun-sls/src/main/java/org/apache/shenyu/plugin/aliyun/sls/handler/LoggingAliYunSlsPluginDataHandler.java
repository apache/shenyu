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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.plugin.MotanRegisterConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.aliyun.sls.DefaultLogCollector;
import org.apache.shenyu.plugin.aliyun.sls.aliyunsls.AliyunSlsLogCollectClient;
import org.apache.shenyu.plugin.aliyun.sls.config.LogCollectConfig;
import org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant;
import org.apache.shenyu.plugin.aliyun.sls.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Properties;

/**
 * LoggingAliYunSlsPluginDataHandler aliyun sls plugin data handler.
 */
public class LoggingAliYunSlsPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingAliYunSlsPluginDataHandler.class);

    private static final AliyunSlsLogCollectClient ALIYUN_SLS_LOG_COLLECT_CLIENT = new AliyunSlsLogCollectClient();

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        LOG.info("AliYun sls plugin data: {}", GsonUtils.getGson().toJson(pluginData));
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            LogCollectConfig.GlobalLogConfig globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                    LogCollectConfig.GlobalLogConfig.class);
            LogCollectConfig.GlobalLogConfig exist = Singleton.INST.get(LogCollectConfig.GlobalLogConfig.class);
            if (Objects.isNull(globalLogConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !globalLogConfig.equals(exist)) {
                // no data, init client
                LogCollectConfigUtils.setGlobalConfig(globalLogConfig);
                Properties properties = new Properties();
                properties.setProperty(LoggingConstant.ACCESS_ID, globalLogConfig.getAccessId().trim());
                properties.setProperty(LoggingConstant.ACCESS_KEY, globalLogConfig.getAccessKey().trim());
                properties.setProperty(LoggingConstant.HOST, globalLogConfig.getHost().trim());
                properties.setProperty(LoggingConstant.PROJECT_NAME, globalLogConfig.getProjectName().trim());
                properties.setProperty(LoggingConstant.LOG_STORE, globalLogConfig.getLogStoreName().trim());
                properties.setProperty(LoggingConstant.TTL_IN_DAY, String.valueOf(globalLogConfig.getTtlInDay()));
                properties.setProperty(LoggingConstant.SHARD_COUNT, String.valueOf(globalLogConfig.getShardCount()));
                properties.setProperty(LoggingConstant.TOPIC, globalLogConfig.getTopic().trim());
                properties.setProperty(LoggingConstant.SEND_THREAD_COUNT, String.valueOf(globalLogConfig.getSendThreadCount()));
                properties.setProperty(LoggingConstant.IO_THREAD_COUNT, String.valueOf(globalLogConfig.getIoThreadCount()));

                // init aliyun sls client
                ALIYUN_SLS_LOG_COLLECT_CLIENT.initClient(properties);
                DefaultLogCollector.getInstance().start();
            }
            Singleton.INST.single(MotanRegisterConfig.class, globalLogConfig);
        } else {
            try {
                DefaultLogCollector.getInstance().close();
            } catch (Exception e) {
                LOG.error("close log collector error", e);
            }
        }
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
