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

import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.filter.McpServerFilter;
import org.apache.shenyu.plugin.mcp.server.handler.McpServerPluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.core.annotation.Order;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.reactive.function.server.RequestPredicates;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.server.WebFilter;

/**
 * The type Mock plugin configuration.
 */
@ConditionalOnProperty(prefix = "shenyu.plugins.mcp.server", name = "enabled", havingValue = "true", matchIfMissing = true)
public class McpServerPluginConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(McpServerPluginConfiguration.class);


//    @Bean
//    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "SYNC", matchIfMissing = true)
//    public List<SyncToolSpecification> syncTools(final ObjectProvider<List<ToolCallback>> toolCalls,
//            final List<ToolCallback> toolCallbacksList, final McpServerProperties serverProperties) {
//
//        List<ToolCallback> tools = new ArrayList<>(toolCalls.stream().flatMap(List::stream).toList());
//
//        if (!CollectionUtils.isEmpty(toolCallbacksList)) {
//            tools.addAll(toolCallbacksList);
//        }
//
//        return this.toSyncToolSpecifications(tools, serverProperties);
//    }

//    private List<McpServerFeatures.SyncToolSpecification> toSyncToolSpecifications(final List<ToolCallback> tools,
//            final McpServerProperties serverProperties) {
//
//        // De-duplicate tools by their name, keeping the first occurrence of each tool
//        // name
//        return tools.stream()
//                // Key:
//                .collect(Collectors.toMap(tool -> tool.getToolDefinition().name(),
//                        // tool
//                        // name
//                        // Value: the tool itself
//                        tool -> tool,
//                        // On duplicate key, keep the
//                        (existing, replacement) -> existing))
//                // existing tool
//                .values().stream().map(tool -> {
//                    String toolName = tool.getToolDefinition().name();
//                    MimeType mimeType = (serverProperties.getToolResponseMimeType().containsKey(toolName))
//                            ? MimeType.valueOf(serverProperties.getToolResponseMimeType().get(toolName))
//                            : null;
//                    return McpToolUtils.toSyncToolSpecification(tool, mimeType);
//                }).toList();
//    }

//    @Bean
//    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "SYNC", matchIfMissing = true)
//    public McpSyncServer mcpSyncServer(final McpServerTransportProvider transportProvider,
//            final McpSchema.ServerCapabilities.Builder capabilitiesBuilder,
//            final McpServerProperties serverProperties,
//            final ObjectProvider<List<SyncToolSpecification>> tools,
//            final ObjectProvider<List<SyncResourceSpecification>> resources,
//            final ObjectProvider<List<SyncPromptSpecification>> prompts,
//            // ObjectProvider<List<SyncCompletionSpecification>> completions,
//            final ObjectProvider<BiConsumer<McpSyncServerExchange, List<Root>>> rootsChangeConsumers,
//            final List<ToolCallbackProvider> toolCallbackProvider) {
//
//        McpSchema.Implementation serverInfo = new Implementation(serverProperties.getName(),
//                serverProperties.getVersion());
//
//        // Create the server with both tool and resource capabilities
//        SyncSpecification serverBuilder = McpServer.sync(transportProvider).serverInfo(serverInfo);
//
//        List<SyncToolSpecification> toolSpecifications = new ArrayList<>(tools.stream().flatMap(List::stream).toList());
//
//        List<ToolCallback> providerToolCallbacks = toolCallbackProvider.stream()
//                .map(pr -> List.of(pr.getToolCallbacks())).flatMap(List::stream).filter(Objects::nonNull).toList();
//
//        toolSpecifications.addAll(this.toSyncToolSpecifications(providerToolCallbacks, serverProperties));
//
//        if (!CollectionUtils.isEmpty(toolSpecifications)) {
//            serverBuilder.tools(toolSpecifications);
//            capabilitiesBuilder.tools(serverProperties.isToolChangeNotification());
//            LOGGER.info("Registered tools: " + toolSpecifications.size() + ", notification: "
//                    + serverProperties.isToolChangeNotification());
//        }
//
//        List<SyncResourceSpecification> resourceSpecifications = resources.stream().flatMap(List::stream).toList();
//        if (!CollectionUtils.isEmpty(resourceSpecifications)) {
//            serverBuilder.resources(resourceSpecifications);
//            capabilitiesBuilder.resources(false, serverProperties.isResourceChangeNotification());
//            LOGGER.info("Registered resources: " + resourceSpecifications.size() + ", notification: "
//                    + serverProperties.isResourceChangeNotification());
//        }
//
//        List<SyncPromptSpecification> promptSpecifications = prompts.stream().flatMap(List::stream).toList();
//        if (!CollectionUtils.isEmpty(promptSpecifications)) {
//            serverBuilder.prompts(promptSpecifications);
//            capabilitiesBuilder.prompts(serverProperties.isPromptChangeNotification());
//            LOGGER.info("Registered prompts: " + promptSpecifications.size() + ", notification: "
//                    + serverProperties.isPromptChangeNotification());
//        }
//
//        rootsChangeConsumers.ifAvailable(consumer -> {
//            serverBuilder.rootsChangeHandler(consumer);
//            LOGGER.info("Registered roots change consumer");
//        });
//
//        serverBuilder.capabilities(capabilitiesBuilder.build());
//
//        serverBuilder.instructions(serverProperties.getInstructions());
//
//        return serverBuilder.build();
//    }

//    @Bean
//    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "ASYNC")
//    public List<McpServerFeatures.AsyncToolSpecification> asyncTools(final ObjectProvider<List<ToolCallback>> toolCalls,
//            final List<ToolCallback> toolCallbackList,
//            final McpServerProperties serverProperties) {
//
//        List<ToolCallback> tools = new ArrayList<>(toolCalls.stream().flatMap(List::stream).toList());
//        if (!CollectionUtils.isEmpty(toolCallbackList)) {
//            tools.addAll(toolCallbackList);
//        }
//
//        return this.toAsyncToolSpecification(tools, serverProperties);
//    }

//    private List<McpServerFeatures.AsyncToolSpecification> toAsyncToolSpecification(final List<ToolCallback> tools,
//            final McpServerProperties serverProperties) {
//        // De-duplicate tools by their name, keeping the first occurrence of each tool
//        // name
//        // Key:
//        return tools.stream().collect(Collectors.toMap(tool -> tool.getToolDefinition().name(),
//                // tool
//                // name
//                // Value: the tool itself
//                tool -> tool,
//                // On duplicate key, keep the
//                (existing, replacement) -> existing))
//                // existing tool
//                .values().stream().map(tool -> {
//                    String toolName = tool.getToolDefinition().name();
//                    MimeType mimeType = (serverProperties.getToolResponseMimeType().containsKey(toolName))
//                            ? MimeType.valueOf(serverProperties.getToolResponseMimeType().get(toolName))
//                            : null;
//                    return McpToolUtils.toAsyncToolSpecification(tool, mimeType);
//                }).toList();
//    }

//    @Bean
//    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "ASYNC")
//    public McpAsyncServer mcpAsyncServer(final McpServerTransportProvider transportProvider,
//            final McpSchema.ServerCapabilities.Builder capabilitiesBuilder,
//            final McpServerProperties serverProperties,
//            final ObjectProvider<List<AsyncToolSpecification>> tools,
//            final ObjectProvider<List<AsyncResourceSpecification>> resources,
//            final ObjectProvider<List<AsyncPromptSpecification>> prompts,
//            // ObjectProvider<List<AsyncCompletionSpecification>> completions,
//            final ObjectProvider<BiConsumer<McpAsyncServerExchange, List<McpSchema.Root>>> rootsChangeConsumer,
//            final List<ToolCallbackProvider> toolCallbackProvider) {
//
//        McpSchema.Implementation serverInfo = new Implementation(serverProperties.getName(),
//                serverProperties.getVersion());
//
//        // Create the server with both tool and resource capabilities
//        AsyncSpecification serverBuilder = McpServer.async(transportProvider).serverInfo(serverInfo);
//
//        List<AsyncToolSpecification> toolSpecifications = new ArrayList<>(
//                tools.stream().flatMap(List::stream).toList());
//        List<ToolCallback> providerToolCallbacks = toolCallbackProvider.stream()
//                .map(pr -> List.of(pr.getToolCallbacks())).flatMap(List::stream).filter(Objects::nonNull).toList();
//
//        toolSpecifications.addAll(this.toAsyncToolSpecification(providerToolCallbacks, serverProperties));
//
//        if (!CollectionUtils.isEmpty(toolSpecifications)) {
//            serverBuilder.tools(toolSpecifications);
//            capabilitiesBuilder.tools(serverProperties.isToolChangeNotification());
//            LOGGER.info("Registered tools: " + toolSpecifications.size() + ", notification: "
//                    + serverProperties.isToolChangeNotification());
//        }
//
//        List<AsyncResourceSpecification> resourceSpecifications = resources.stream().flatMap(List::stream).toList();
//        if (!CollectionUtils.isEmpty(resourceSpecifications)) {
//            serverBuilder.resources(resourceSpecifications);
//            capabilitiesBuilder.resources(false, serverProperties.isResourceChangeNotification());
//            LOGGER.info("Registered resources: " + resourceSpecifications.size() + ", notification: "
//                    + serverProperties.isResourceChangeNotification());
//        }
//
//        List<AsyncPromptSpecification> promptSpecifications = prompts.stream().flatMap(List::stream).toList();
//        if (!CollectionUtils.isEmpty(promptSpecifications)) {
//            serverBuilder.prompts(promptSpecifications);
//            capabilitiesBuilder.prompts(serverProperties.isPromptChangeNotification());
//            LOGGER.info("Registered prompts: " + promptSpecifications.size() + ", notification: "
//                    + serverProperties.isPromptChangeNotification());
//        }
//
//        rootsChangeConsumer.ifAvailable(consumer -> {
//            BiFunction<McpAsyncServerExchange, List<Root>, Mono<Void>> asyncConsumer = (exchange, roots) -> {
//                consumer.accept(exchange, roots);
//                return Mono.empty();
//            };
//            serverBuilder.rootsChangeHandler(asyncConsumer);
//            LOGGER.info("Registered roots change consumer");
//        });
//
//        serverBuilder.capabilities(capabilitiesBuilder.build());
//
//        serverBuilder.instructions(serverProperties.getInstructions());
//
//        return serverBuilder.build();
//    }


    /**
     * Health filter.
     *
     * @param dispatcherHandler   the dispatcher handler
     * @param shenyuMcpServerManager the shenyu mcp server manager
     * @return the web filter
     */
    @Bean
    @Order(-99)
    @ConditionalOnProperty(name = "shenyu.mcp.server.enabled", havingValue = "true", matchIfMissing = true)
    public WebFilter mcpFilter(final DispatcherHandler dispatcherHandler,
                               final ShenyuMcpServerManager shenyuMcpServerManager) {
        return new McpServerFilter(dispatcherHandler, shenyuMcpServerManager);
    }
    
    @Bean
    public RouterFunction<ServerResponse> dynamicRouter(final ShenyuMcpServerManager shenyuMcpServerManager) {
        return RouterFunctions.route(RequestPredicates.all(), shenyuMcpServerManager::dispatch);
    }
    
    @Bean
    public ShenyuMcpServerManager shenyuMcpServerManager() {
        return new ShenyuMcpServerManager();
    }

    @Bean
    public PluginDataHandler mcpServerPluginDataHandler(
            final ShenyuMcpServerManager shenyuMcpServerManager) {
        return new McpServerPluginDataHandler(shenyuMcpServerManager);
    }

}
