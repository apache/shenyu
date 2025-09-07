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
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.request.BodyWriterExchange;
import org.apache.shenyu.plugin.mcp.server.request.ParameterFormatter;
import org.apache.shenyu.plugin.mcp.server.request.RequestConfig;
import org.apache.shenyu.plugin.mcp.server.request.RequestConfigHelper;
import org.apache.shenyu.plugin.mcp.server.response.ShenyuMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.response.NonCommittingMcpResponseDecorator;
import org.apache.shenyu.plugin.mcp.server.session.McpSessionHelper;
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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Tool Callback Implementation for Shenyu Gateway MCP Integration.
 * 
 * <p>Handles tool invocations within the MCP framework, enabling AI models to interact
 * with Shenyu Gateway services through defined tools. Requires pre-established session
 * context and ServerWebExchange correlation.</p>
 *
 * @see ToolCallback
 * @see ToolDefinition
 * @since 2.7.0.2
 */
public class ShenyuToolCallback implements ToolCallback {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuToolCallback.class);

    /**
     * Default timeout for tool execution in seconds.
     * Increased to handle multiple concurrent tool executions.
     */
    private static final int DEFAULT_TIMEOUT_SECONDS = 60;

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

        LOG.debug("Executing tool call for definition '{}' with input length: {} chars",
                toolDefinition.name(), input.length());

        try {
            // Extract MCP session context (must be pre-established)
            final McpSyncServerExchange mcpExchange = extractMcpExchange(toolContext);
            final String sessionId = extractSessionId(mcpExchange);

            // Validate and extract tool configuration
            final ShenyuToolDefinition shenyuTool = validateToolDefinition();
            final String configStr = extractRequestConfig(shenyuTool);

            // Get pre-stored exchange and plugin chain
            final ServerWebExchange originExchange = getOriginExchange(sessionId);
            final ShenyuPluginChain chain = getPluginChain(originExchange);

            // Execute the tool call through the plugin chain
            return executeToolCall(originExchange, chain, sessionId, configStr, input);

        } catch (Exception e) {
            LOG.error("Failed to process tool call for '{}': {}", toolDefinition.name(), e.getMessage(), e);

            throw new RuntimeException("Tool execution failed: " + e.getMessage(), e);
        }
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
                    if (!responseFuture.isDone()) {
                        responseFuture.completeExceptionally(e);
                    }
                })
                .doOnSuccess(v -> LOG.debug("Plugin chain completed successfully for session: {}", sessionId))
                .doOnCancel(() -> {
                    LOG.warn("Plugin chain execution cancelled for session: {}", sessionId);
                    if (!responseFuture.isDone()) {
                        responseFuture.completeExceptionally(new RuntimeException("Execution was cancelled"));
                    }
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
            final String result = responseFuture.get(DEFAULT_TIMEOUT_SECONDS, TimeUnit.SECONDS);
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
     * <p>Creates a new exchange with modified request (method, path, headers, body),
     * response decorator based on protocol type, and updated Shenyu context and metadata.</p>
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
        configureShenyuContext(finalExchange, sessionId, requestConfig.getPath(), configStr);
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
        try {
            final JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);
            if (Objects.isNull(inputJson)) {
                throw new IllegalArgumentException("Invalid input JSON format");
            }
            return inputJson;
        } catch (Exception e) {
            LOG.error("Failed to parse input JSON: {}", e.getMessage());
            throw new IllegalArgumentException("Invalid JSON format: " + e.getMessage(), e);
        }
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

        // Build path and query parameters (no formatting needed for URLs)
        final String path = RequestConfigHelper.buildPath(urlTemplate, argsPosition, inputJson);

        // Build body with parameter formatting (only format body parameters that need it)
        final JsonObject bodyJson = buildFormattedBodyJson(argsToJsonBody, argsPosition, inputJson);

        return new RequestConfig(method, path, bodyJson, requestTemplate, argsToJsonBody);
    }

    /**
     * Build body JSON with parameter formatting for specific types.
     * Only formats parameters that are mapped to body and need type conversion.
     *
     * @param argsToJsonBody whether to convert arguments to JSON body
     * @param argsPosition   the argument position mapping
     * @param inputJson      the input JSON object
     * @return the constructed body JSON object
     */
    private JsonObject buildFormattedBodyJson(final boolean argsToJsonBody, final JsonObject argsPosition, final JsonObject inputJson) {
        JsonObject bodyJson = new JsonObject();

        if (!argsToJsonBody) {
            return bodyJson;
        }

        for (String key : argsPosition.keySet()) {
            String position = argsPosition.get(key).getAsString();
            if (position.startsWith("body") && inputJson.has(key)) {
                JsonElement value = inputJson.get(key);

                // Format the value if it's a JSON string that should be parsed
                JsonElement formattedValue = formatBodyParameterValue(value, key);

                if ("body".equals(position)) {
                    bodyJson.add(key, formattedValue);
                } else if (position.startsWith("body.")) {
                    String[] pathParts = position.substring(5).split("\\.");
                    setNestedValue(bodyJson, pathParts, formattedValue);
                }
            }
        }

        return bodyJson;
    }

    /**
     * Format body parameter value. Only handles JSON string parsing for complex types.
     *
     * @param value the parameter value
     * @param paramName the parameter name for error messages
     * @return the formatted value
     */
    private JsonElement formatBodyParameterValue(final JsonElement value, final String paramName) {
        // If it's a string, try to parse it as JSON (for array/object types)
        if (value.isJsonPrimitive() && value.getAsJsonPrimitive().isString()) {
            String stringValue = value.getAsString();
            JsonElement parsed = ParameterFormatter.tryParseJsonString(stringValue);

            // If parsing succeeded (returned different object), log it
            if (!parsed.equals(value)) {
                LOG.debug("Parsed JSON string parameter '{}' into {}", paramName,
                        parsed.isJsonArray() ? "array" : "object");
            }

            return parsed;
        }

        // Return as-is for all other cases
        return value;
    }

    /**
     * Set nested value in JSON object.
     */
    private void setNestedValue(final JsonObject jsonObject, final String[] pathParts, final JsonElement value) {
        JsonObject current = jsonObject;

        for (int i = 0; i < pathParts.length - 1; i++) {
            String part = pathParts[i];
            if (!current.has(part)) {
                current.add(part, new JsonObject());
            }
            current = current.getAsJsonObject(part);
        }

        current.add(pathParts[pathParts.length - 1], value);
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
     * @param method         the HTTP method
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
     * @param path           the target path
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
     * @param sessionId         the session identifier
     * @param decoratedPath     the decorated request path
     * @param configStr  configStr.
     */
    private void configureShenyuContext(final ServerWebExchange decoratedExchange,
                                        final String sessionId,
                                        final String decoratedPath,
                                        final String configStr) {

        final ShenyuContext shenyuContext = decoratedExchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(shenyuContext)) {
            // Set metadata if available
            configureMetadata(decoratedExchange, decoratedPath, shenyuContext);

            final RequestConfigHelper configHelper = new RequestConfigHelper(configStr);
            MetaData metaData = MetaDataCache.getInstance().obtain(configHelper.getUrlTemplate());
            if (Objects.nonNull(metaData) && Boolean.TRUE.equals(metaData.getEnabled())) {
                decoratedExchange.getAttributes().put(Constants.META_DATA, metaData);
                shenyuContext.setRpcType(metaData.getRpcType());
            }
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
     * @param decoratedPath     the request path
     * @param shenyuContext     the Shenyu context
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
     * Extracts the session ID from the MCP sync server exchange.
     *
     * @param mcpExchange the MCP sync server exchange
     * @return the session ID
     * @throws IllegalStateException if session ID cannot be extracted
     */
    private String extractSessionId(final McpSyncServerExchange mcpExchange) {
        final String sessionId;
        try {
            sessionId = McpSessionHelper.getSessionId(mcpExchange);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        if (StringUtils.hasText(sessionId)) {
            LOG.debug("Extracted session ID: {}", sessionId);
            return sessionId;
        }
        throw new IllegalStateException("Session ID is empty – it should have been set earlier by handleMessageEndpoint");
    }

    /**
     * Gets the origin ServerWebExchange for the given session ID.
     *
     * @param sessionId the session ID
     * @return the origin ServerWebExchange
     * @throws IllegalStateException if exchange cannot be retrieved
     */
    private ServerWebExchange getOriginExchange(final String sessionId) {
        final ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);
        if (Objects.nonNull(exchange)) {
            LOG.debug("Found existing exchange for session: {}", sessionId);
            return exchange;
        }
        throw new IllegalStateException("No ServerWebExchange found for session '" + sessionId
                + "'. It should have been stored by handleMessageEndpoint before the tool was invoked.");
    }
}
