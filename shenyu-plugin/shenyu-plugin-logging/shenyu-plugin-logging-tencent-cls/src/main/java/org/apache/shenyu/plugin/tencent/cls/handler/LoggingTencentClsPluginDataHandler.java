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
import org.apache.shenyu.plugin.tencent.cls.client.TencentClsLogCollectClient;
import org.apache.shenyu.plugin.tencent.cls.collector.TencentClsSlsLogCollector;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.tencent.cls.config.TencentLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

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
                // init tencent cls client
                TENCENT_CLS_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
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
