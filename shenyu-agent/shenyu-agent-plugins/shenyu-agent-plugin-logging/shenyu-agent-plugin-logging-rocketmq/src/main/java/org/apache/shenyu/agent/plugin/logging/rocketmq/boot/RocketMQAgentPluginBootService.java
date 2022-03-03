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

package org.apache.shenyu.agent.plugin.logging.rocketmq.boot;

import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.spi.AgentPluginBootService;
import org.apache.shenyu.agent.plugin.logging.rocketmq.RocketMQLogCollectClient;
import org.apache.shenyu.agent.plugin.logging.rocketmq.RocketMQLogCollector;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Log Collection.
 */
@Join
public class RocketMQAgentPluginBootService implements AgentPluginBootService {

    private static final Logger LOG = LoggerFactory.getLogger(RocketMQAgentPluginBootService.class);

    /**
     * start logging plugin service.
     */
    @Override
    public void start(final AgentPluginConfig agentPluginConfig) {
        LOG.info("start RocketMQAgentPluginBootService, config:{}", JsonUtils.toJson(agentPluginConfig));
        RocketMQLogCollectClient client = new RocketMQLogCollectClient(agentPluginConfig);
        new RocketMQLogCollector(client);
    }

    /**
     * close collector.
     */
    @Override
    public void close() {
        try {
            RocketMQLogCollector.getInstance().close();
        } catch (Exception e) {
            LOG.error("close RocketMQLogCollector error", e);
        }
    }
}
