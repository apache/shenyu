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
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.beans.factory.ObjectProvider;
import reactor.core.publisher.Mono;

/**
 * Agent gateway plugin auto configuration.
 */
@Configuration
@ConditionalOnProperty(value = "shenyu.plugins.agent.gateway.enabled", havingValue = "true", matchIfMissing = false)
public class AgentGatewayPluginConfiguration {

    /**
     * Create the agent gateway plugin.
     *
     * @param providers explicitly registered server-side tools
     * @param resolvers trusted security adapter, missing adapters deny access
     * @return the plugin
     */
    @Bean
    public ShenyuPlugin agentGatewayPlugin(final ObjectProvider<AgentToolProvider> providers, final ObjectProvider<AgentMcpSecurityResolver> resolvers) {
        AgentToolRegistry registry = new AgentToolRegistry(providers.orderedStream().toList());
        AgentMcpSecurityResolver resolver = resolvers.getIfAvailable(() -> exchange -> Mono.empty());
        return new AgentGatewayPlugin(new AgentMcpHttpHandler(new AgentMcpDispatcher(registry, "shenyu-agent-gateway", "2.7.2-SNAPSHOT"), resolver));
    }

    /**
     * Create the agent gateway plugin data handler.
     *
     * @return the data handler
     */
    @Bean
    public PluginDataHandler agentGatewayPluginDataHandler() {
        return new AgentGatewayPluginDataHandler();
    }
}
