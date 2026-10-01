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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import org.apache.shenyu.plugin.agent.gateway.AgentGatewayPlugin;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentGatewayPluginConfigurationTest {

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner()
            .withUserConfiguration(AgentGatewayPluginConfiguration.class);

    @Test
    void shouldKeepPluginUnassembledByDefault() {
        contextRunner.run(context -> {
            assertFalse(context.containsBean("agentGatewayPlugin"));
            assertFalse(context.containsBean("agentGatewayPluginDataHandler"));
        });
    }

    @Test
    void shouldAssemblePluginWhenEnabled() {
        contextRunner.withPropertyValues("shenyu.plugins.agent.gateway.enabled=true")
                .run(context -> {
                    assertTrue(context.getBean("agentGatewayPlugin") instanceof AgentGatewayPlugin);
                    assertTrue(context.getBean(ShenyuPlugin.class) instanceof AgentGatewayPlugin);
                    assertTrue(context.getBean("agentGatewayPluginDataHandler") instanceof PluginDataHandler);
                });
    }
}
