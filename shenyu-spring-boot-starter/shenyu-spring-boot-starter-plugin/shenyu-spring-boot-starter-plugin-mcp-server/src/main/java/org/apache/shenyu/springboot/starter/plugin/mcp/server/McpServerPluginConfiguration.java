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

package org.apache.shenyu.springboot.starter.plugin.mcp.server;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.McpServerPlugin;
import org.apache.shenyu.plugin.mcp.server.handler.McpServerPluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.http.codec.ServerCodecConfigurer;

/**
 * The type Mock plugin configuration.
 */
@ConditionalOnProperty(prefix = "shenyu.plugins.mcp.server", name = "enabled", havingValue = "true", matchIfMissing = true)
public class McpServerPluginConfiguration {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(McpServerPluginConfiguration.class);
    
//    /**
//     * Health filter.
//     *
//     * @param dispatcherHandler   the dispatcher handler
//     * @param shenyuMcpServerManager the shenyu mcp server manager
//     * @return the web filter
//     */
//    @Bean
//    @Order(-99)
//    public WebFilter mcpFilter(final DispatcherHandler dispatcherHandler,
//                               final ShenyuMcpServerManager shenyuMcpServerManager) {
//        return new McpServerFilter(dispatcherHandler, shenyuMcpServerManager);
//    }
//
//    @Bean
//    public RouterFunction<ServerResponse> dynamicRouter(final ShenyuMcpServerManager shenyuMcpServerManager) {
//        return RouterFunctions.route(RequestPredicates.all(), shenyuMcpServerManager::dispatch);
//    }
    
    @Bean
    public ShenyuMcpServerManager shenyuMcpServerManager() {
        return new ShenyuMcpServerManager();
    }

    /**
     * Mcp Server plugin data handler.
     *
     * @param shenyuMcpServerManager the shenyu mcp server manager
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler mcpServerPluginDataHandler(
            final ShenyuMcpServerManager shenyuMcpServerManager) {
        return new McpServerPluginDataHandler(shenyuMcpServerManager);
    }
    
    /**
     * Mcp Server plugin.
     *
     * @param shenyuMcpServerManager the shenyu mcp server manager
     * @param configurer the server codec configurer
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin mcpServerPlugin(final ShenyuMcpServerManager shenyuMcpServerManager,
                                        final ServerCodecConfigurer configurer) {
        return new McpServerPlugin(shenyuMcpServerManager, configurer.getReaders());
    }
}
