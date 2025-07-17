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

package org.apache.shenyu.plugin.mcp.server.callback;

import com.google.common.collect.Maps;
import com.google.gson.JsonObject;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.request.BodyWriterExchange;
import org.apache.shenyu.plugin.mcp.server.request.RequestConfig;
import org.apache.shenyu.plugin.mcp.server.request.RequestConfigHelper;
import org.apache.shenyu.plugin.mcp.server.response.ShenyuMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.response.NonCommittingMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.session.McpSessionHelper;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Implementation of Spring AI's ToolCallback interface for Shenyu Gateway integration.
 * <p>
 * This callback handles tool invocations within the MCP (Model Context Protocol) framework,
 * allowing AI models to interact with Shenyu Gateway services through defined tools.
 * </p>
 * <p>
 * Key responsibilities:
 * <ul>
 *   <li>Execute tool calls by routing them through the Shenyu plugin chain</li>
 *   <li>Manage MCP session context and exchange correlation</li>
 *   <li>Handle protocol-specific response processing (SSE vs Streamable HTTP)</li>
 *   <li>Provide proper error handling and timeout management</li>
 * </ul>
 * </p>
 *
 * @see org.springframework.ai.tool.ToolCallback
 * @see org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition
 * @since 1.0.0
 */
public class ShenyuToolCallback implements ToolCallback {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuToolCallback.class);

    /**
     * Default timeout for tool execution in seconds.
     */
    private static final int DEFAULT_TIMEOUT_SECONDS = 30;

    /**
     * Reduced timeout for testing and quick operations.
     */
    private static final int QUICK_TIMEOUT_SECONDS = 2;

    /**
     * MCP tool call attribute marker to prevent infinite loops.
     */
    private static final String MCP_TOOL_CALL_ATTR = "MCP_TOOL_CALL";

    /**
     * MCP session ID attribute key.
     */
    private static final String MCP_SESSION_ID_ATTR = "MCP_TOOL_SESSION_ID";

    /**
     * Streamable HTTP protocol path indicator.
     */
    private static final String STREAMABLE_HTTP_PATH = "/streamablehttp";

    private final ToolDefinition toolDefinition;

    /**
     * Constructs a new Shenyu tool callback with the specified tool definition.
     *
     * @param toolDefinition the tool definition that this callback will handle
     * @throws NullPointerException if toolDefinition is null
     */
    public ShenyuToolCallback(final ToolDefinition toolDefinition) {
        this.toolDefinition = Objects.requireNonNull(toolDefinition, "ToolDefinition cannot be null");
    }

    @NonNull
    @Override
    public ToolDefinition getToolDefinition() {
        return this.toolDefinition;
    }

    @NonNull
    @Override
    public String call(@NonNull final String input) {
        return call(input, new ToolContext(Maps.newHashMap()));
    }

    @NonNull
    @Override
    public String call(@NonNull final String input, final ToolContext toolContext) {
        Objects.requireNonNull(input, "Input cannot be null");
        Objects.requireNonNull(toolContext, "ToolContext cannot be null");

        LOG.debug("Executing tool call with input length: {} chars", input.length());

        try {
            // Extract MCP session context with enhanced handling
            final McpSyncServerExchange mcpExchange = extractMcpExchange(toolContext);
            final String sessionId = extractSessionIdWithFallback(mcpExchange, toolContext);

            // Validate and extract tool configuration
            final ShenyuToolDefinition shenyuTool = validateToolDefinition();
            final String configStr = extractRequestConfig(shenyuTool);

            // Get or create exchange and plugin chain with enhanced handling
            final ServerWebExchange originExchange = getOrCreateOriginExchange(sessionId, toolContext);
            final ShenyuPluginChain chain = getPluginChain(originExchange);

            // Execute the tool call through the plugin chain
            final String result = executeToolCall(originExchange, chain, sessionId, configStr, input);

            // Clean up temporary session from tool context if needed
            cleanupTemporarySessionFromContext(sessionId, toolContext);

            return result;

        } catch (Exception e) {
            LOG.error("Failed to process tool call: {}", e.getMessage(), e);

            // Try to clean up any temporary session on error
            try {
                final String tempSessionId = (String) toolContext.getContext().get("TEMP_SESSION_ID");
                if (tempSessionId != null) {
                    cleanupTemporarySessionFromContext(tempSessionId, toolContext);
                }
            } catch (Exception cleanupException) {
                LOG.warn("Failed to cleanup temporary session on error: {}", cleanupException.getMessage());
            }

            throw new RuntimeException("Tool execution failed: " + e.getMessage(), e);
        }
    }

    /**
     * Enhanced session ID extraction with fallback mechanism.
     * <p>
     * This method attempts to extract session ID with the following fallback strategy:
     * <ol>
     *   <li>Try to extract from MCP exchange normally</li>
     *   <li>If extraction fails and protocol is Streamable HTTP, create a temporary session</li>
     *   <li>If extraction fails and protocol is SSE, throw exception (SSE requires real sessions)</li>
     * </ol>
     * </p>
     *
     * @param mcpExchange the MCP sync server exchange
     * @param toolContext the tool context for session creation
     * @return the session ID (existing, restored, or temporary for Streamable HTTP only)
     * @throws IllegalStateException if session ID cannot be extracted for SSE protocol
     */
    private String extractSessionIdWithFallback(final McpSyncServerExchange mcpExchange,
                                                final ToolContext toolContext) {
        try {
            // Attempt normal session ID extraction
            final String sessionId = McpSessionHelper.getSessionId(mcpExchange);
            if (StringUtils.hasText(sessionId)) {
                LOG.debug("Extracted session ID: {}", sessionId);
                return sessionId;
            } else {
                // Check if we can create temporary session based on protocol
                return handleMissingSessionId(toolContext, "Session ID is empty");
            }
        } catch (NoSuchFieldException | IllegalAccessException e) {
            LOG.warn("Failed to extract session ID from MCP exchange: {}", e.getMessage());
            return handleMissingSessionId(toolContext, "Failed to extract session ID: " + e.getMessage());
        } catch (Exception e) {
            LOG.warn("Unexpected error extracting session ID: {}", e.getMessage());
            return handleMissingSessionId(toolContext, "Unexpected error: " + e.getMessage());
        }
    }

    /**
     * Handles missing session ID based on the detected protocol.
     * <p>
     * For Streamable HTTP: Creates temporary session (supports stateless operations)
     * For SSE: Throws exception (requires real persistent session)
     * </p>
     *
     * @param toolContext the tool context
     * @param reason the reason for missing session ID
     * @return temporary session ID for Streamable HTTP protocol
     * @throws IllegalStateException for SSE protocol or when protocol cannot be determined
     */
    private String handleMissingSessionId(final ToolContext toolContext, final String reason) {
        // Try to determine protocol from context or create a test exchange to check
        final boolean isStreamableHttp = isStreamableHttpProtocolContext(toolContext);

        if (isStreamableHttp) {
            LOG.info("Creating temporary session for Streamable HTTP tool call - reason: {}", reason);
            return createTemporarySessionForToolCall(toolContext);
        } else {
            // SSE protocol requires real session - don't create temporary session
            final String errorMsg = String.format(
                    "SSE protocol requires a valid session ID for tool calls. %s. " +
                            "Please ensure the SSE connection is properly established before calling tools.",
                    reason);
            LOG.error(errorMsg);
            throw new IllegalStateException(errorMsg);
        }
    }

    /**
     * Determines if the current context is for Streamable HTTP protocol.
     * <p>
     * This method attempts to detect the protocol by checking available context information.
     * Since we may not have a complete exchange at this point, we use heuristics.
     * </p>
     *
     * @param toolContext the tool context
     * @return true if Streamable HTTP protocol is detected, false if SSE or unknown
     */
    private boolean isStreamableHttpProtocolContext(final ToolContext toolContext) {
        try {
            // Try to get any protocol hints from tool context
            final Object protocolHint = toolContext.getContext().get("PROTOCOL_TYPE");
            if (protocolHint != null) {
                final String protocol = protocolHint.toString();
                LOG.debug("Found protocol hint in context: {}", protocol);
                return "STREAMABLE_HTTP".equals(protocol) || "HTTP_STREAM".equals(protocol);
            }

            // Check if there's any indication of Streamable HTTP in the context
            final Object pathHint = toolContext.getContext().get("REQUEST_PATH");
            if (pathHint != null) {
                final String path = pathHint.toString();
                final boolean isStreamable = path.contains(STREAMABLE_HTTP_PATH) || path.endsWith(STREAMABLE_HTTP_PATH);
                LOG.debug("Checking path hint for protocol: {} -> isStreamable: {}", path, isStreamable);
                return isStreamable;
            }

            // Try to extract protocol from MCP exchange if available
            try {
                final McpSyncServerExchange mcpExchange = McpSessionHelper.getMcpSyncServerExchange(toolContext);
                if (mcpExchange != null) {
                    final boolean protocolFromExchange = extractProtocolFromMcpExchange(mcpExchange);
                    if (protocolFromExchange) {
                        LOG.debug("Detected Streamable HTTP from MCP exchange analysis");
                        return true;
                    }
                }
            } catch (Exception e) {
                LOG.debug("Could not extract protocol from MCP exchange: {}", e.getMessage());
            }

            // If no clear indicators, default to false (SSE) for safety
            // This prevents creating temporary sessions for SSE protocol
            LOG.debug("No protocol hints found, defaulting to SSE (no temporary session)");
            return false;

        } catch (Exception e) {
            LOG.warn("Error detecting protocol context: {}, defaulting to SSE", e.getMessage());
            return false;
        }
    }

    /**
     * Attempts to extract protocol information from the MCP exchange.
     * <p>
     * This method uses reflection and heuristics to determine if the underlying
     * exchange indicates Streamable HTTP protocol.
     * </p>
     *
     * @param mcpExchange the MCP sync server exchange
     * @return true if Streamable HTTP is detected, false otherwise
     */
    private boolean extractProtocolFromMcpExchange(final McpSyncServerExchange mcpExchange) {
        try {
            // Try to get session ID first - if we can get it, check associated exchange
            final String sessionId = McpSessionHelper.getSessionId(mcpExchange);
            if (StringUtils.hasText(sessionId)) {
                final ServerWebExchange associatedExchange = ShenyuMcpExchangeHolder.get(sessionId);
                if (associatedExchange != null) {
                    final boolean isStreamable = isStreamableHttpProtocol(associatedExchange);
                    LOG.debug("Protocol detection from associated exchange for session {}: {}", sessionId, isStreamable);
                    return isStreamable;
                }
            }

            // If no associated exchange found, we cannot determine protocol reliably
            return false;

        } catch (Exception e) {
            LOG.debug("Error extracting protocol from MCP exchange: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Gets or creates the origin ServerWebExchange with enhanced fallback handling.
     * <p>
     * This method handles the following scenarios:
     * <ol>
     *   <li>Session exists with valid exchange - return existing exchange</li>
     *   <li>Session exists but no exchange found - create new session and exchange (Streamable HTTP only)</li>
     *   <li>Temporary session - create minimal exchange context (Streamable HTTP only)</li>
     * </ol>
     * </p>
     *
     * @param sessionId   the session identifier
     * @param toolContext the tool context for exchange creation
     * @return the origin ServerWebExchange
     * @throws IllegalStateException if exchange cannot be created for SSE protocol
     */
    private ServerWebExchange getOrCreateOriginExchange(final String sessionId, final ToolContext toolContext) {
        // Try to get existing exchange
        ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);

        if (exchange != null) {
            LOG.debug("Found existing exchange for session: {}", sessionId);
            return exchange;
        }

        // Check if this is a temporary session (only allowed for Streamable HTTP)
        if (sessionId.startsWith("temp_")) {
            LOG.info("Creating minimal exchange context for temporary session: {}", sessionId);
            return createMinimalExchangeContext(sessionId, toolContext);
        }

        // Session ID exists but no exchange found - check protocol before restoring
        if (isStreamableHttpProtocolContext(toolContext)) {
            LOG.info("Session {} found but no exchange available, restoring session context for Streamable HTTP", sessionId);
            return restoreSessionContext(sessionId, toolContext);
        } else {
            // SSE protocol - cannot restore session without proper SSE connection
            final String errorMsg = String.format(
                    "SSE protocol session '%s' has no associated exchange. " +
                            "This typically means the SSE connection was lost. " +
                            "Please re-establish the SSE connection before calling tools.",
                    sessionId);
            LOG.error(errorMsg);
            throw new IllegalStateException(errorMsg);
        }
    }

    /**
     * Creates a temporary session for tool calls when no session ID is available.
     * <p>
     * Temporary sessions are created with the prefix "temp_" and are designed
     * for stateless tool operations that don't require persistent session state.
     * </p>
     *
     * @param toolContext the tool context for session creation
     * @return the temporary session ID
     */
    private String createTemporarySessionForToolCall(final ToolContext toolContext) {
        final String tempSessionId = "temp_" + java.util.UUID.randomUUID().toString();
        LOG.info("Created temporary session for tool call: {}", tempSessionId);

        // Store the temporary session ID in the tool context for later cleanup
        toolContext.getContext().put("TEMP_SESSION_ID", tempSessionId);

        return tempSessionId;
    }

    /**
     * Creates a minimal exchange context for temporary sessions.
     * <p>
     * This method creates a basic ServerWebExchange with minimal required attributes
     * for tool execution. It's used when no existing exchange is available.
     * </p>
     *
     * @param sessionId   the session identifier
     * @param toolContext the tool context
     * @return a minimal ServerWebExchange instance
     */
    private ServerWebExchange createMinimalExchangeContext(final String sessionId, final ToolContext toolContext) {
        try {
            // Get or create a basic exchange from Spring context
            final ServerWebExchange minimalExchange = createBasicExchange();

            // Configure basic attributes for tool execution
            configureMinimalExchangeAttributes(minimalExchange, sessionId);

            // Store the exchange for potential reuse during this tool call
            ShenyuMcpExchangeHolder.put(sessionId, minimalExchange);

            LOG.debug("Created minimal exchange context for session: {}", sessionId);
            return minimalExchange;

        } catch (Exception e) {
            LOG.error("Failed to create minimal exchange context for session {}: {}", sessionId, e.getMessage(), e);
            throw new RuntimeException("Unable to create exchange context for tool execution: " + e.getMessage(), e);
        }
    }

    /**
     * Restores session context when session ID exists but no exchange is found.
     * <p>
     * This handles scenarios where a session was previously created but the exchange
     * was lost (e.g., server restart, session timeout). A new exchange is created
     * and associated with the existing session ID.
     * </p>
     *
     * @param sessionId   the session identifier to restore
     * @param toolContext the tool context
     * @return the restored ServerWebExchange
     */
    private ServerWebExchange restoreSessionContext(final String sessionId, final ToolContext toolContext) {
        try {
            // Create new exchange for the existing session
            final ServerWebExchange restoredExchange = createBasicExchange();

            // Configure attributes for restored session
            configureRestoredExchangeAttributes(restoredExchange, sessionId);

            // Re-associate the exchange with the session
            ShenyuMcpExchangeHolder.put(sessionId, restoredExchange);

            LOG.info("Restored session context for session: {}", sessionId);
            return restoredExchange;

        } catch (Exception e) {
            LOG.error("Failed to restore session context for session {}: {}", sessionId, e.getMessage(), e);
            throw new RuntimeException("Unable to restore session context: " + e.getMessage(), e);
        }
    }

    /**
     * Creates a basic ServerWebExchange instance for tool execution.
     * <p>
     * This method uses Spring's infrastructure to create a minimal but functional
     * ServerWebExchange that can be used for plugin chain execution.
     * </p>
     *
     * @return a basic ServerWebExchange instance
     */
    private ServerWebExchange createBasicExchange() {
        try {
            // Try to get an existing exchange from current context if available
            final ServerWebExchange currentExchange = getCurrentExchangeFromContext();
            if (currentExchange != null) {
                return currentExchange;
            }

            // Create a new basic exchange using Spring Boot's infrastructure
            return createNewBasicExchange();

        } catch (Exception e) {
            LOG.warn("Could not create basic exchange through normal means, using fallback: {}", e.getMessage());
            return createFallbackExchange();
        }
    }

    /**
     * Attempts to get the current exchange from the reactive context.
     * <p>
     * Since getting exchange from reactive context is complex and error-prone,
     * this method now returns null and lets other methods handle exchange creation.
     * </p>
     *
     * @return null (simplified implementation)
     */
    private ServerWebExchange getCurrentExchangeFromContext() {
        // Simplified implementation - return null to let other methods handle exchange creation
        // This avoids complex reactive context operations that may not work in all scenarios
        LOG.debug("Skipping reactive context exchange lookup, will create new exchange");
        return null;
    }

    /**
     * Creates a new basic exchange using Spring infrastructure.
     *
     * @return a new ServerWebExchange instance
     */
    private ServerWebExchange createNewBasicExchange() {
        // For tool calls, we create a minimal HTTP exchange using Mock objects
        try {
            final org.springframework.mock.http.server.reactive.MockServerHttpRequest request =
                    org.springframework.mock.http.server.reactive.MockServerHttpRequest
                            .post("http://localhost/tool-call")
                            .header("Content-Type", "application/json")
                            .build();

            // Create exchange from mock request - this provides a working ServerWebExchange
            return org.springframework.mock.web.server.MockServerWebExchange.from(request);

        } catch (Exception e) {
            LOG.warn("Failed to create new basic exchange: {}", e.getMessage());
            throw new RuntimeException("Unable to create basic exchange: " + e.getMessage(), e);
        }
    }

    /**
     * Creates a fallback exchange when normal creation methods fail.
     *
     * @return a fallback ServerWebExchange instance
     */
    private ServerWebExchange createFallbackExchange() {
        // This is a last resort - create a minimal mock exchange using Spring Mock
        try {
            final org.springframework.mock.http.server.reactive.MockServerHttpRequest request =
                    org.springframework.mock.http.server.reactive.MockServerHttpRequest
                            .post("/tool-call")
                            .header("Content-Type", "application/json")
                            .build();

            final org.springframework.mock.http.server.reactive.MockServerHttpResponse response =
                    new org.springframework.mock.http.server.reactive.MockServerHttpResponse();

            // Create the exchange using Mock objects
            return org.springframework.mock.web.server.MockServerWebExchange.from(request);

        } catch (Exception e) {
            LOG.error("All exchange creation methods failed: {}", e.getMessage());
            throw new RuntimeException("Cannot create any form of ServerWebExchange for tool execution", e);
        }
    }

    /**
     * Configures minimal attributes for a temporary session exchange.
     *
     * @param exchange  the exchange to configure
     * @param sessionId the session identifier
     */
    private void configureMinimalExchangeAttributes(final ServerWebExchange exchange, final String sessionId) {
        // Create basic Shenyu context
        final ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(RpcTypeEnum.HTTP.getName());
        shenyuContext.setPath("/tool-call");
        shenyuContext.setRealUrl("/tool-call");

        // Set required attributes
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        exchange.getAttributes().put(MCP_TOOL_CALL_ATTR, true);
        exchange.getAttributes().put(MCP_SESSION_ID_ATTR, sessionId);

        LOG.debug("Configured minimal attributes for temporary session: {}", sessionId);
    }

    /**
     * Configures attributes for a restored session exchange.
     *
     * @param exchange  the exchange to configure
     * @param sessionId the session identifier
     */
    private void configureRestoredExchangeAttributes(final ServerWebExchange exchange, final String sessionId) {
        // Create restored Shenyu context
        final ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(RpcTypeEnum.HTTP.getName());
        shenyuContext.setPath("/tool-call-restored");
        shenyuContext.setRealUrl("/tool-call-restored");

        // Set attributes for restored session
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        exchange.getAttributes().put(MCP_TOOL_CALL_ATTR, true);
        exchange.getAttributes().put(MCP_SESSION_ID_ATTR, sessionId);

        LOG.debug("Configured restored attributes for session: {}", sessionId);
    }

    /**
     * Extracts the MCP sync server exchange from the tool context.
     *
     * @param toolContext the tool context containing MCP session information
     * @return the MCP sync server exchange
     * @throws IllegalStateException if exchange cannot be retrieved
     */
    private McpSyncServerExchange extractMcpExchange(final ToolContext toolContext) {
        final McpSyncServerExchange exchange = McpSessionHelper.getMcpSyncServerExchange(toolContext);
        if (Objects.isNull(exchange)) {
            throw new IllegalStateException("Failed to retrieve MCP sync server exchange from context");
        }
        return exchange;
    }

    /**
     * Validates and casts the tool definition to Shenyu-specific type.
     *
     * @return the Shenyu tool definition
     * @throws IllegalStateException if tool definition is not of expected type
     */
    private ShenyuToolDefinition validateToolDefinition() {
        if (!(this.toolDefinition instanceof ShenyuToolDefinition)) {
            throw new IllegalStateException("Tool definition must be of type ShenyuToolDefinition, got: "
                    + this.toolDefinition.getClass().getSimpleName());
        }
        return (ShenyuToolDefinition) this.toolDefinition;
    }

    /**
     * Extracts and validates the request configuration from the tool definition.
     *
     * @param definition the Shenyu tool definition
     * @return the request configuration string
     * @throws IllegalStateException if configuration is missing or invalid
     */
    private String extractRequestConfig(final ShenyuToolDefinition definition) {
        final String config = definition.requestConfig();
        if (!StringUtils.hasText(config)) {
            throw new IllegalStateException("Request configuration cannot be empty");
        }
        LOG.debug("Using request configuration with length: {} chars", config.length());
        return config;
    }

    /**
     * Extracts the plugin chain from the exchange.
     *
     * @param exchange the server web exchange
     * @return the plugin chain
     * @throws IllegalStateException if chain is not found
     */
    private ShenyuPluginChain getPluginChain(final ServerWebExchange exchange) {
        final ShenyuPluginChain chain = exchange.getAttribute(Constants.CHAIN);
        Assert.notNull(chain, "ShenyuPluginChain cannot be null");
        return chain;
    }

    /**
     * Executes the tool call through the Shenyu plugin chain.
     *
     * @param originExchange the original server web exchange
     * @param chain          the plugin chain to execute
     * @param sessionId      the MCP session identifier
     * @param configStr      the request configuration
     * @param input          the tool input parameters
     * @return the execution result
     */
    private String executeToolCall(final ServerWebExchange originExchange,
                                   final ShenyuPluginChain chain,
                                   final String sessionId,
                                   final String configStr,
                                   final String input) {

        final CompletableFuture<String> responseFuture = new CompletableFuture<>();
        final ServerWebExchange decoratedExchange = buildDecoratedExchange(
                originExchange, responseFuture, sessionId, configStr, input);

        LOG.debug("Executing plugin chain for session: {}", sessionId);

        // Check if this is a temporary session that needs cleanup
        final boolean isTemporarySession = sessionId.startsWith("temp_");

        // Execute the plugin chain asynchronously
        chain.execute(decoratedExchange)
                .doOnSubscribe(s -> LOG.debug("Plugin chain subscribed for session: {}", sessionId))
                .doOnError(e -> {
                    LOG.error("Plugin chain execution failed for session {}: {}", sessionId, e.getMessage(), e);
                    responseFuture.completeExceptionally(e);
                })
                .doOnSuccess(v -> LOG.debug("Plugin chain completed successfully for session: {}", sessionId))
                .doOnCancel(() -> {
                    LOG.warn("Plugin chain execution cancelled for session: {}", sessionId);
                    responseFuture.completeExceptionally(new RuntimeException("Execution was cancelled"));
                })
                .doFinally(signalType -> {
                    // Clean up temporary sessions after execution
                    if (isTemporarySession) {
                        LOG.debug("Cleaning up temporary session: {} (signal: {})", sessionId, signalType);
                        ShenyuMcpExchangeHolder.remove(sessionId);
                    }
                })
                .subscribe();

        // Wait for the response with timeout
        try {
            final String result = responseFuture.get(QUICK_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            LOG.debug("Tool call completed successfully for session: {}", sessionId);
            return result;
        } catch (Exception e) {
            LOG.error("Timeout or error waiting for response for session {}: {}", sessionId, e.getMessage(), e);

            // Ensure cleanup on error for temporary sessions
            if (isTemporarySession) {
                LOG.debug("Emergency cleanup of temporary session on error: {}", sessionId);
                ShenyuMcpExchangeHolder.remove(sessionId);
            }

            throw new RuntimeException("Tool execution timeout or error: " + e.getMessage(), e);
        }
    }

    /**
     * Builds a decorated ServerWebExchange for tool execution.
     * <p>
     * This method creates a new exchange with:
     * <ul>
     *   <li>Modified request (method, path, headers, body)</li>
     *   <li>Response decorator based on protocol type</li>
     *   <li>Updated Shenyu context and metadata</li>
     * </ul>
     * </p>
     *
     * @param originExchange the original exchange
     * @param responseFuture the future for capturing response
     * @param sessionId      the session identifier
     * @param configStr      the request configuration
     * @param input          the tool input parameters
     * @return the decorated exchange ready for execution
     */
    private ServerWebExchange buildDecoratedExchange(final ServerWebExchange originExchange,
                                                     final CompletableFuture<String> responseFuture,
                                                     final String sessionId,
                                                     final String configStr,
                                                     final String input) {

        // Parse input and configuration
        final JsonObject inputJson = parseInput(input);
        final RequestConfig requestConfig = buildRequestConfig(configStr, inputJson);

        // Build decorated request
        final ServerHttpRequest decoratedRequest = buildDecoratedRequest(
                originExchange, sessionId, requestConfig);

        // Build response decorator based on protocol
        final ServerHttpResponseDecorator responseDecorator = createResponseDecorator(
                originExchange, sessionId, responseFuture, configStr);

        // Create base decorated exchange
        final ServerWebExchange decoratedExchange = originExchange.mutate()
                .request(decoratedRequest)
                .response(responseDecorator)
                .build();

        // Handle request body if needed
        final ServerWebExchange finalExchange = handleRequestBody(decoratedExchange, requestConfig);

        // Configure Shenyu context and metadata
        configureShenyuContext(finalExchange, sessionId, requestConfig.getPath());

        return finalExchange;
    }

    /**
     * Parses the input JSON string into a JsonObject.
     *
     * @param input the input JSON string
     * @return the parsed JsonObject
     * @throws IllegalArgumentException if input is not valid JSON
     */
    private JsonObject parseInput(final String input) {
        final JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
        if (Objects.isNull(inputJson)) {
            throw new IllegalArgumentException("Invalid input JSON format");
        }
        return inputJson;
    }

    /**
     * Builds request configuration from configuration string and input parameters.
     *
     * @param configStr the configuration string
     * @param inputJson the input parameters
     * @return the built request configuration
     */
    private RequestConfig buildRequestConfig(final String configStr, final JsonObject inputJson) {
        final RequestConfigHelper configHelper = new RequestConfigHelper(configStr);
        final JsonObject requestTemplate = configHelper.getRequestTemplate();
        final JsonObject argsPosition = configHelper.getArgsPosition();
        final String urlTemplate = configHelper.getUrlTemplate();
        final String method = configHelper.getMethod();
        final boolean argsToJsonBody = configHelper.isArgsToJsonBody();

        final String path = RequestConfigHelper.buildPath(urlTemplate, argsPosition, inputJson);
        final JsonObject bodyJson = RequestConfigHelper.buildBodyJson(argsToJsonBody, argsPosition, inputJson);

        return new RequestConfig(method, path, bodyJson, requestTemplate, argsToJsonBody);
    }

    /**
     * Builds a decorated HTTP request with modified method, path, and headers.
     *
     * @param originExchange the original exchange
     * @param sessionId      the session identifier
     * @param requestConfig  the request configuration
     * @return the decorated HTTP request
     */
    private ServerHttpRequest buildDecoratedRequest(final ServerWebExchange originExchange,
                                                    final String sessionId,
                                                    final RequestConfig requestConfig) {

        final ServerHttpRequest.Builder requestBuilder = originExchange
                .getRequest()
                .mutate()
                .method(HttpMethod.valueOf(requestConfig.getMethod()))
                .header("sessionId", sessionId)
                .header("Accept", "application/json");

        // Add custom headers from template
        addCustomHeaders(requestBuilder, requestConfig);

        // Set content type for body methods
        configureContentType(requestBuilder, requestConfig.getMethod());

        // Set the target URI
        setTargetUri(requestBuilder, originExchange, requestConfig.getPath());

        return requestBuilder.build();
    }

    /**
     * Adds custom headers from the request template to the request builder.
     *
     * @param requestBuilder the request builder
     * @param requestConfig  the request configuration
     */
    private void addCustomHeaders(final ServerHttpRequest.Builder requestBuilder,
                                  final RequestConfig requestConfig) {
        if (requestConfig.getRequestTemplate().has("headers")) {
            for (final var headerElem : requestConfig.getRequestTemplate().getAsJsonArray("headers")) {
                final JsonObject headerObj = headerElem.getAsJsonObject();
                requestBuilder.header(
                        headerObj.get("key").getAsString(),
                        headerObj.get("value").getAsString()
                );
            }
        }
    }

    /**
     * Configures content type based on HTTP method.
     *
     * @param requestBuilder the request builder
     * @param method        the HTTP method
     */
    private void configureContentType(final ServerHttpRequest.Builder requestBuilder, final String method) {
        if (isRequestBodyMethod(method)) {
            requestBuilder.header("Content-Type", "application/json");
        } else {
            requestBuilder.headers(httpHeaders -> httpHeaders.remove("Content-Type"));
        }
    }

    /**
     * Sets the target URI for the request.
     *
     * @param requestBuilder the request builder
     * @param originExchange the original exchange
     * @param path          the target path
     */
    private void setTargetUri(final ServerHttpRequest.Builder requestBuilder,
                              final ServerWebExchange originExchange,
                              final String path) {
        try {
            final URI oldUri = originExchange.getRequest().getURI();
            final String newUriStr = oldUri.getScheme() + "://" + oldUri.getAuthority() + path;
            requestBuilder.uri(new URI(newUriStr));
        } catch (URISyntaxException e) {
            throw new RuntimeException("Invalid URI construction: " + e.getMessage(), e);
        }
    }

    /**
     * Creates appropriate response decorator based on protocol type.
     *
     * @param originExchange the original exchange
     * @param sessionId      the session identifier
     * @param responseFuture the response future
     * @param configStr      the configuration string (for response template)
     * @return the appropriate response decorator
     */
    private ServerHttpResponseDecorator createResponseDecorator(final ServerWebExchange originExchange,
                                                                final String sessionId,
                                                                final CompletableFuture<String> responseFuture,
                                                                final String configStr) {

        final RequestConfigHelper configHelper = new RequestConfigHelper(configStr);
        final JsonObject responseTemplate = configHelper.getResponseTemplate();

        if (isStreamableHttpProtocol(originExchange)) {
            LOG.debug("Using non-committing decorator for Streamable HTTP protocol, session: {}", sessionId);
            return new NonCommittingMcpResponseDecorator(
                    originExchange.getResponse(), sessionId, responseFuture, responseTemplate);
        } else {
            LOG.debug("Using standard decorator for SSE protocol, session: {}", sessionId);
            return new ShenyuMcpResponseDecorator(
                    originExchange.getResponse(), sessionId, responseFuture, responseTemplate);
        }
    }

    /**
     * Handles request body for methods that support it.
     *
     * @param decoratedExchange the decorated exchange
     * @param requestConfig     the request configuration
     * @return the exchange with body handling applied
     */
    private ServerWebExchange handleRequestBody(final ServerWebExchange decoratedExchange,
                                                final RequestConfig requestConfig) {
        if (isRequestBodyMethod(requestConfig.getMethod()) && requestConfig.getBodyJson().size() > 0) {
            return new BodyWriterExchange(decoratedExchange, requestConfig.getBodyJson().toString());
        }
        return decoratedExchange;
    }

    /**
     * Configures Shenyu context and metadata for the decorated exchange.
     *
     * @param decoratedExchange the decorated exchange
     * @param sessionId        the session identifier
     * @param decoratedPath    the decorated request path
     */
    private void configureShenyuContext(final ServerWebExchange decoratedExchange,
                                        final String sessionId,
                                        final String decoratedPath) {

        final ShenyuContext shenyuContext = decoratedExchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(shenyuContext)) {
            // Set metadata if available
            configureMetadata(decoratedExchange, decoratedPath, shenyuContext);

            // Configure RPC type and context
            shenyuContext.setRpcType(RpcTypeEnum.HTTP.getName());
            shenyuContext.setPath(decoratedPath);
            shenyuContext.setRealUrl(decoratedPath);

            LOG.debug("Configured RpcType to HTTP for tool call, session: {}", sessionId);

            decoratedExchange.getAttributes().put(Constants.CONTEXT, shenyuContext);

            // Add MCP tool call markers to prevent loops
            decoratedExchange.getAttributes().put(MCP_TOOL_CALL_ATTR, true);
            decoratedExchange.getAttributes().put(MCP_SESSION_ID_ATTR, sessionId);
        }
    }

    /**
     * Configures metadata for the request if available.
     *
     * @param decoratedExchange the decorated exchange
     * @param decoratedPath    the request path
     * @param shenyuContext    the Shenyu context
     */
    private void configureMetadata(final ServerWebExchange decoratedExchange,
                                   final String decoratedPath,
                                   final ShenyuContext shenyuContext) {

        final MetaData metaData = MetaDataCache.getInstance().obtain(decoratedPath);
        if (Objects.nonNull(metaData) && Boolean.TRUE.equals(metaData.getEnabled())) {
            decoratedExchange.getAttributes().put(Constants.META_DATA, metaData);
            // Set metadata RPC type first (will be overridden to HTTP)
            shenyuContext.setRpcType(metaData.getRpcType());
            LOG.debug("Applied metadata for path: {}", decoratedPath);
        }
    }

    /**
     * Checks if the protocol is Streamable HTTP based on URI path.
     *
     * @param exchange the server web exchange
     * @return true if Streamable HTTP protocol is detected
     */
    private boolean isStreamableHttpProtocol(final ServerWebExchange exchange) {
        final String uri = exchange.getRequest().getURI().getRawPath();
        final boolean isStreamable = uri.contains(STREAMABLE_HTTP_PATH) || uri.endsWith(STREAMABLE_HTTP_PATH);
        LOG.debug("Protocol detection - URI: {}, isStreamableHttp: {}", uri, isStreamable);
        return isStreamable;
    }

    /**
     * Checks if the HTTP method supports request body.
     *
     * @param method the HTTP method name
     * @return true if the method supports request body
     */
    private boolean isRequestBodyMethod(final String method) {
        return "POST".equalsIgnoreCase(method)
                || "PUT".equalsIgnoreCase(method)
                || "PATCH".equalsIgnoreCase(method);
    }

    /**
     * Cleans up temporary session information from the ToolContext.
     * <p>
     * This method removes temporary session data from the tool context to prevent
     * memory leaks and ensure proper cleanup of temporary resources.
     * </p>
     *
     * @param sessionId   the session identifier
     * @param toolContext the tool context to clean up
     */
    private void cleanupTemporarySessionFromContext(final String sessionId, final ToolContext toolContext) {
        if (sessionId != null && sessionId.startsWith("temp_")) {
            try {
                // Remove temporary session ID from context
                toolContext.getContext().remove("TEMP_SESSION_ID");
                LOG.debug("Cleaned up temporary session from tool context: {}", sessionId);
            } catch (Exception e) {
                LOG.warn("Failed to cleanup temporary session from context {}: {}", sessionId, e.getMessage());
            }
        }
    }
}
