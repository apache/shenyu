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

package org.apache.shenyu.agent.plugin.logging.common.definition;

import org.apache.shenyu.agent.api.entity.PointCutConfig;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint.JoinPointBuilder;
import org.apache.shenyu.agent.api.spi.AbstractAgentPluginDefinition;
import org.apache.shenyu.agent.core.builder.JoinPointBuilderFactory;
import org.apache.shenyu.agent.core.locator.ShenyuAgentLocator;
import org.apache.shenyu.agent.core.yaml.ShenyuYamlEngine;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Collection;

/**
 * define logging collect plugin.
 */
@Join
public class LoggingAgentPluginDefinition extends AbstractAgentPluginDefinition {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingAgentPluginDefinition.class);

    /**
     * get Collection of JoinPointBuilder.
     *
     * @return Collection of JoinPointBuilder
     */
    @Override
    protected Collection<JoinPointBuilder> joinPointBuilder() {
        PointCutConfig config = null;
        try {
            config = ShenyuYamlEngine.unmarshal(ShenyuAgentLocator.locatorConf("logging-point.yaml"),
                    PointCutConfig.class);
        } catch (IOException e) {
            LOG.error("Exception loader logging point config", e);
        }
        return JoinPointBuilderFactory.create(config);
    }
}
