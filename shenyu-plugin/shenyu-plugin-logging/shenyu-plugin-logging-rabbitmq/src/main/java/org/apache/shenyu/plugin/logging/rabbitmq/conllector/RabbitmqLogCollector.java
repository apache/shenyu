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

package org.apache.shenyu.plugin.logging.rabbitmq.conllector;

import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.apache.shenyu.plugin.logging.rabbitmq.client.RabbitmqLogCollectClient;
import org.apache.shenyu.plugin.logging.rabbitmq.config.RabbitmqLogCollectConfig;
import org.apache.shenyu.plugin.logging.rabbitmq.handler.LoggingRabbitmqPluginDataHandler;

/**
 * rabbitmq log collector，depend a LogConsumeClient for consume logs.
 */
public class RabbitmqLogCollector extends AbstractLogCollector<RabbitmqLogCollectClient, ShenyuRequestLog, RabbitmqLogCollectConfig.RabbitmqLogConfig> {

    private static final LogCollector<ShenyuRequestLog> INSTANCE = new RabbitmqLogCollector();

    /**
     * get LogCollector Instance.
     *
     * @return logCollector Instance
     */
    public static LogCollector<ShenyuRequestLog> getInstance() {
        return INSTANCE;
    }

    /**
     * get LogCollector Instance.
     *
     * @return LogCollector Instance
     */
    @Override
    public RabbitmqLogCollectClient getLogConsumeClient() {
        return LoggingRabbitmqPluginDataHandler.getRabbitmqLogCollectClient();
    }

    @Override
    protected RabbitmqLogCollectConfig.RabbitmqLogConfig getLogCollectConfig() {
        return RabbitmqLogCollectConfig.INSTANCE.getRabbitmqLogConfig();
    }

    @Override
    protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
    }
}
