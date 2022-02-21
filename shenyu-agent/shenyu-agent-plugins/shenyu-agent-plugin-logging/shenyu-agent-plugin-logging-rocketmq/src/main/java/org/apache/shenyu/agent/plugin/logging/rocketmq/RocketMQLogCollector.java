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

package org.apache.shenyu.agent.plugin.logging.rocketmq;

import org.apache.shenyu.agent.plugin.logging.LogCollector;
import org.apache.shenyu.agent.plugin.logging.common.AbstractLogCollector;
import org.apache.shenyu.agent.plugin.logging.spi.LogCollectClient;

import java.util.Objects;

/**
 * queue-based logging collector.
 */
public class RocketMQLogCollector extends AbstractLogCollector {

    private static RocketMQLogCollector instance;

    public RocketMQLogCollector(final LogCollectClient logCollectClient) {
        super(logCollectClient);
        instance = this;
    }

    /**
     * get RocketMQLogCollector instance.
     *
     * @return RocketMQLogCollector instance
     */
    public static LogCollector getInstance() {
        return Objects.requireNonNull(instance);
    }

}
