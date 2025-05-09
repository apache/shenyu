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

import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.server.McpAsyncServer;
import io.modelcontextprotocol.server.McpAsyncServerExchange;
import io.modelcontextprotocol.server.McpServer;
import io.modelcontextprotocol.server.McpServer.AsyncSpecification;
import io.modelcontextprotocol.server.McpServer.SyncSpecification;
import io.modelcontextprotocol.server.McpServerFeatures;
import io.modelcontextprotocol.server.McpServerFeatures.AsyncPromptSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.AsyncResourceSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.AsyncToolSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.SyncPromptSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.SyncResourceSpecification;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.server.McpSyncServer;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.server.transport.WebFluxSseServerTransportProvider;
import io.modelcontextprotocol.spec.McpSchema;
import io.modelcontextprotocol.spec.McpSchema.Implementation;
import io.modelcontextprotocol.spec.McpSchema.Root;
import io.modelcontextprotocol.spec.McpServerTransportProvider;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.McpServerPlugin;
import org.apache.shenyu.plugin.mcp.server.filter.McpServerFilter;
import org.apache.shenyu.plugin.mcp.server.handler.McpServerPluginDataHandler;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpAsyncToolsManager;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpSyncToolsManager;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpToolsManager;
import org.apache.shenyu.plugin.mcp.server.tools.ShenyuDefaultTools;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.mcp.McpToolUtils;
import org.springframework.ai.mcp.server.autoconfigure.McpServerAutoConfiguration;
import org.springframework.ai.mcp.server.autoconfigure.McpServerProperties;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.ToolCallbackProvider;
import org.springframework.ai.tool.method.MethodToolCallbackProvider;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.core.annotation.Order;
import org.springframework.util.CollectionUtils;
import org.springframework.util.MimeType;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.server.WebFilter;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * The type Mock plugin configuration.
 */
@EnableConfigurationProperties({McpServerProperties.class })
@AutoConfiguration(after = McpServerAutoConfiguration.class)
@ConditionalOnProperty(prefix = "shenyu.plugins.mcp.server" , name = "enabled",havingValue = "true", matchIfMissing = true)
public class McpServerPluginConfiguration {
    
    private static final Logger logger = LoggerFactory.getLogger(McpServerPluginConfiguration.class);
    
    @Bean
    @ConditionalOnMissingBean
    public WebFluxSseServerTransportProvider webFluxTransport(ObjectProvider<ObjectMapper> objectMapperProvider,
                                                              McpServerProperties serverProperties) {
        ObjectMapper objectMapper = objectMapperProvider.getIfAvailable(ObjectMapper::new);
        return new WebFluxSseServerTransportProvider(objectMapper, serverProperties.getSseMessageEndpoint(),
                serverProperties.getSseEndpoint());
    }
    
    // Router function for SSE transport used by Spring WebFlux to start an HTTP server.
    @Bean
    public RouterFunction<?> webfluxMcpRouterFunction(WebFluxSseServerTransportProvider webFluxProvider) {
        return webFluxProvider.getRouterFunction();
    }
    
    
    @Bean
    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "SYNC",
            matchIfMissing = true)
    public List<SyncToolSpecification> syncTools(ObjectProvider<List<ToolCallback>> toolCalls,
                                                 List<ToolCallback> toolCallbacksList, McpServerProperties serverProperties) {
        
        List<ToolCallback> tools = new ArrayList<>(toolCalls.stream().flatMap(List::stream).toList());
        
        if (!CollectionUtils.isEmpty(toolCallbacksList)) {
            tools.addAll(toolCallbacksList);
        }
        
        return this.toSyncToolSpecifications(tools, serverProperties);
    }
    
    private List<McpServerFeatures.SyncToolSpecification> toSyncToolSpecifications(List<ToolCallback> tools,
                                                                                   McpServerProperties serverProperties) {
        
        // De-duplicate tools by their name, keeping the first occurrence of each tool
        // name
        return tools.stream()
                .collect(Collectors.toMap(tool -> tool.getToolDefinition().name(), // Key:
                        // tool
                        // name
                        tool -> tool, // Value: the tool itself
                        (existing, replacement) -> existing)) // On duplicate key, keep the
                // existing tool
                .values()
                .stream()
                .map(tool -> {
                    String toolName = tool.getToolDefinition().name();
                    MimeType mimeType = (serverProperties.getToolResponseMimeType().containsKey(toolName))
                            ? MimeType.valueOf(serverProperties.getToolResponseMimeType().get(toolName)) : null;
                    return McpToolUtils.toSyncToolSpecification(tool, mimeType);
                })
                .toList();
    }
    
    @Bean
    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "SYNC",
            matchIfMissing = true)
    public McpSyncServer mcpSyncServer(McpServerTransportProvider transportProvider,
                                       McpSchema.ServerCapabilities.Builder capabilitiesBuilder, McpServerProperties serverProperties,
                                       ObjectProvider<List<SyncToolSpecification>> tools,
                                       ObjectProvider<List<SyncResourceSpecification>> resources,
                                       ObjectProvider<List<SyncPromptSpecification>> prompts,
                                       // ObjectProvider<List<SyncCompletionSpecification>> completions,
                                       ObjectProvider<BiConsumer<McpSyncServerExchange, List<Root>>> rootsChangeConsumers,
                                       List<ToolCallbackProvider> toolCallbackProvider) {
        
        McpSchema.Implementation serverInfo = new Implementation(serverProperties.getName(),
                serverProperties.getVersion());
        
        // Create the server with both tool and resource capabilities
        SyncSpecification serverBuilder = McpServer.sync(transportProvider).serverInfo(serverInfo);
        
        List<SyncToolSpecification> toolSpecifications = new ArrayList<>(tools.stream().flatMap(List::stream).toList());
        
        List<ToolCallback> providerToolCallbacks = toolCallbackProvider.stream()
                .map(pr -> List.of(pr.getToolCallbacks()))
                .flatMap(List::stream)
                .filter(Objects::nonNull)
                .toList();
        
        toolSpecifications.addAll(this.toSyncToolSpecifications(providerToolCallbacks, serverProperties));
        
        if (!CollectionUtils.isEmpty(toolSpecifications)) {
            serverBuilder.tools(toolSpecifications);
            capabilitiesBuilder.tools(serverProperties.isToolChangeNotification());
            logger.info("Registered tools: " + toolSpecifications.size() + ", notification: "
                    + serverProperties.isToolChangeNotification());
        }
        
        List<SyncResourceSpecification> resourceSpecifications = resources.stream().flatMap(List::stream).toList();
        if (!CollectionUtils.isEmpty(resourceSpecifications)) {
            serverBuilder.resources(resourceSpecifications);
            capabilitiesBuilder.resources(false, serverProperties.isResourceChangeNotification());
            logger.info("Registered resources: " + resourceSpecifications.size() + ", notification: "
                    + serverProperties.isResourceChangeNotification());
        }
        
        List<SyncPromptSpecification> promptSpecifications = prompts.stream().flatMap(List::stream).toList();
        if (!CollectionUtils.isEmpty(promptSpecifications)) {
            serverBuilder.prompts(promptSpecifications);
            capabilitiesBuilder.prompts(serverProperties.isPromptChangeNotification());
            logger.info("Registered prompts: " + promptSpecifications.size() + ", notification: "
                    + serverProperties.isPromptChangeNotification());
        }
        
        rootsChangeConsumers.ifAvailable(consumer -> {
            serverBuilder.rootsChangeHandler(consumer);
            logger.info("Registered roots change consumer");
        });
        
        serverBuilder.capabilities(capabilitiesBuilder.build());
        
        serverBuilder.instructions(serverProperties.getInstructions());
        
        return serverBuilder.build();
    }
    
    @Bean
    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "ASYNC")
    public List<McpServerFeatures.AsyncToolSpecification> asyncTools(ObjectProvider<List<ToolCallback>> toolCalls,
                                                                     List<ToolCallback> toolCallbackList, McpServerProperties serverProperties) {
        
        List<ToolCallback> tools = new ArrayList<>(toolCalls.stream().flatMap(List::stream).toList());
        if (!CollectionUtils.isEmpty(toolCallbackList)) {
            tools.addAll(toolCallbackList);
        }
        
        return this.toAsyncToolSpecification(tools, serverProperties);
    }
    
    private List<McpServerFeatures.AsyncToolSpecification> toAsyncToolSpecification(List<ToolCallback> tools,
                                                                                    McpServerProperties serverProperties) {
        // De-duplicate tools by their name, keeping the first occurrence of each tool
        // name
        return tools.stream()
                .collect(Collectors.toMap(tool -> tool.getToolDefinition().name(), // Key:
                        // tool
                        // name
                        tool -> tool, // Value: the tool itself
                        (existing, replacement) -> existing)) // On duplicate key, keep the
                // existing tool
                .values()
                .stream()
                .map(tool -> {
                    String toolName = tool.getToolDefinition().name();
                    MimeType mimeType = (serverProperties.getToolResponseMimeType().containsKey(toolName))
                            ? MimeType.valueOf(serverProperties.getToolResponseMimeType().get(toolName)) : null;
                    return McpToolUtils.toAsyncToolSpecification(tool, mimeType);
                })
                .toList();
    }
    
    @Bean
    @ConditionalOnProperty(prefix = McpServerProperties.CONFIG_PREFIX, name = "type", havingValue = "ASYNC")
    public McpAsyncServer mcpAsyncServer(McpServerTransportProvider transportProvider,
                                         McpSchema.ServerCapabilities.Builder capabilitiesBuilder, McpServerProperties serverProperties,
                                         ObjectProvider<List<AsyncToolSpecification>> tools,
                                         ObjectProvider<List<AsyncResourceSpecification>> resources,
                                         ObjectProvider<List<AsyncPromptSpecification>> prompts,
                                         // ObjectProvider<List<AsyncCompletionSpecification>> completions,
                                         ObjectProvider<BiConsumer<McpAsyncServerExchange, List<McpSchema.Root>>> rootsChangeConsumer,
                                         List<ToolCallbackProvider> toolCallbackProvider) {
        
        McpSchema.Implementation serverInfo = new Implementation(serverProperties.getName(),
                serverProperties.getVersion());
        
        // Create the server with both tool and resource capabilities
        AsyncSpecification serverBuilder = McpServer.async(transportProvider).serverInfo(serverInfo);
        
        List<AsyncToolSpecification> toolSpecifications = new ArrayList<>(
                tools.stream().flatMap(List::stream).toList());
        List<ToolCallback> providerToolCallbacks = toolCallbackProvider.stream()
                .map(pr -> List.of(pr.getToolCallbacks()))
                .flatMap(List::stream)
                .filter(Objects::nonNull)
                .toList();
        
        toolSpecifications.addAll(this.toAsyncToolSpecification(providerToolCallbacks, serverProperties));
        
        if (!CollectionUtils.isEmpty(toolSpecifications)) {
            serverBuilder.tools(toolSpecifications);
            capabilitiesBuilder.tools(serverProperties.isToolChangeNotification());
            logger.info("Registered tools: " + toolSpecifications.size() + ", notification: "
                    + serverProperties.isToolChangeNotification());
        }
        
        List<AsyncResourceSpecification> resourceSpecifications = resources.stream().flatMap(List::stream).toList();
        if (!CollectionUtils.isEmpty(resourceSpecifications)) {
            serverBuilder.resources(resourceSpecifications);
            capabilitiesBuilder.resources(false, serverProperties.isResourceChangeNotification());
            logger.info("Registered resources: " + resourceSpecifications.size() + ", notification: "
                    + serverProperties.isResourceChangeNotification());
        }
        
        List<AsyncPromptSpecification> promptSpecifications = prompts.stream().flatMap(List::stream).toList();
        if (!CollectionUtils.isEmpty(promptSpecifications)) {
            serverBuilder.prompts(promptSpecifications);
            capabilitiesBuilder.prompts(serverProperties.isPromptChangeNotification());
            logger.info("Registered prompts: " + promptSpecifications.size() + ", notification: "
                    + serverProperties.isPromptChangeNotification());
        }
        
        rootsChangeConsumer.ifAvailable(consumer -> {
            BiFunction<McpAsyncServerExchange, List<Root>, Mono<Void>> asyncConsumer = (exchange, roots) -> {
                consumer.accept(exchange, roots);
                return Mono.empty();
            };
            serverBuilder.rootsChangeHandler(asyncConsumer);
            logger.info("Registered roots change consumer");
        });
        
        serverBuilder.capabilities(capabilitiesBuilder.build());
        
        serverBuilder.instructions(serverProperties.getInstructions());
        
        return serverBuilder.build();
    }
    
    @Bean
    @ConditionalOnBean(McpAsyncServer.class)
    @ConditionalOnMissingBean(ShenyuMcpToolsManager.class)
    public ShenyuMcpToolsManager shenyuMcpAsyncToolsManager(final McpAsyncServer mcpAsyncServer) {
        return new ShenyuMcpAsyncToolsManager(mcpAsyncServer);
    }
    
    @Bean
    @ConditionalOnBean(McpSyncServer.class)
    @ConditionalOnMissingBean(ShenyuMcpToolsManager.class)
    public ShenyuMcpToolsManager shenyuMcpSyncToolsManager(final McpSyncServer mcpSyncServer) {
        return new ShenyuMcpSyncToolsManager(mcpSyncServer);
    }
    
    /**
     * Health filter.
     *
     * @param dispatcherHandler   the dispatcher handler
     * @param mcpServerProperties the mcp server properties
     * @return the web filter
     */
    @Bean
    @Order(-99)
    @ConditionalOnProperty(name = "shenyu.mcp.server.enabled", havingValue = "true", matchIfMissing = true)
    public WebFilter mcpFilter(final DispatcherHandler dispatcherHandler,
                               final McpServerProperties mcpServerProperties) {
        return new McpServerFilter(dispatcherHandler, mcpServerProperties.getSseMessageEndpoint());
    }
    
    @Bean
    public ToolCallbackProvider shenyuTools() {
        return MethodToolCallbackProvider.builder().toolObjects(new ShenyuDefaultTools()).build();
    }

    /**
     * Mcp Server plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin mcpServerPlugin() {
        return new McpServerPlugin();
    }

    /**
     * Mcp Server plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler mcpServerPluginDataHandler(final ShenyuMcpToolsManager shenyuMcpToolsManager) {
        return new McpServerPluginDataHandler(shenyuMcpToolsManager);
    }
    
}
