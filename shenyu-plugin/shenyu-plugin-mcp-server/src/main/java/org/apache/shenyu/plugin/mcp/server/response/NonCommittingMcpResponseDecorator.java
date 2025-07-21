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

package org.apache.shenyu.plugin.mcp.server.response;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.utils.GsonUtils;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;
import java.util.Objects;

/**
 * Non-committing MCP response decorator for Streamable HTTP protocol.
 * Intercepts response data without writing to the original response, avoiding premature
 * response commitment issues. Used for capturing response data for processing and transformation
 * before final output.
 *
 * @see org.apache.shenyu.plugin.mcp.server.response.ShenyuMcpResponseDecorator
 * @since 2.7.0.2
 */
public class NonCommittingMcpResponseDecorator extends ServerHttpResponseDecorator {

    private static final Logger LOG = LoggerFactory.getLogger(NonCommittingMcpResponseDecorator.class);

    /**
     * Template placeholder pattern for variable substitution.
     */
    private static final String PLACEHOLDER_PREFIX = "${";

    private static final String PLACEHOLDER_SUFFIX = "}";

    private final String sessionId;

    private final CompletableFuture<String> responseFuture;

    private final JsonObject responseTemplate;

    /**
     * Constructs a new non-committing MCP response decorator.
     *
     * @param delegate         the underlying HTTP response delegate
     * @param sessionId        the MCP session identifier for correlation
     * @param responseFuture   the future to complete with processed response data
     * @param responseTemplate the JSON template for response transformation (nullable)
     */
    public NonCommittingMcpResponseDecorator(final ServerHttpResponse delegate,
                                             final String sessionId,
                                             final CompletableFuture<String> responseFuture,
                                             final JsonObject responseTemplate) {
        super(delegate);
        this.sessionId = sessionId;
        this.responseFuture = responseFuture;
        this.responseTemplate = responseTemplate;
        LOG.debug("Created non-committing MCP response decorator for session: {}", sessionId);
    }

    @Override
    public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
        LOG.debug("Processing writeWith for session: {}", sessionId);

        return Flux.from(body)
                .collectList()
                .doOnNext(this::processResponseData)
                .then()
                .doOnSuccess(aVoid -> LOG.debug("Successfully completed writeWith for session: {}", sessionId))
                .doOnError(error -> handleProcessingError("writeWith", error));
    }

    @Override
    public Mono<Void> writeAndFlushWith(final Publisher<? extends Publisher<? extends DataBuffer>> body) {
        LOG.debug("Processing writeAndFlushWith for session: {}", sessionId);

        return Flux.from(body)
                .flatMap(Flux::from)
                .collectList()
                .doOnNext(this::processResponseData)
                .then()
                .doOnSuccess(aVoid -> LOG.debug("Successfully completed writeAndFlushWith for session: {}", sessionId))
                .doOnError(error -> handleProcessingError("writeAndFlushWith", error));
    }

    /**
     * Processes the collected response data buffers and completes the response future.
     *
     * <p>Aggregates all data buffers into a single response string, applies response template
     * transformations if configured, and completes the response future with the processed result.
     *
     * @param dataBuffers the collected response data buffers
     */
    private void processResponseData(final java.util.List<? extends DataBuffer> dataBuffers) {
        try {
            final String responseBody = aggregateDataBuffers(dataBuffers);
            LOG.debug("Captured response data for session {}, length: {} chars", sessionId, responseBody.length());

            final String processedResponse = processResponse(responseBody);
            LOG.debug("Processed response for session {}, final length: {} chars", sessionId, processedResponse.length());

            // Complete the future with processed response without writing to original response
            responseFuture.complete(processedResponse);

        } catch (Exception e) {
            LOG.error("Error processing response data for session {}: {}", sessionId, e.getMessage(), e);
            responseFuture.completeExceptionally(e);
        }
    }

    /**
     * Aggregates multiple data buffers into a single response string.
     *
     * @param dataBuffers the list of data buffers to aggregate
     * @return the aggregated response string
     */
    private String aggregateDataBuffers(final java.util.List<? extends DataBuffer> dataBuffers) {
        final StringBuilder responseBuilder = new StringBuilder();

        for (DataBuffer buffer : dataBuffers) {
            final byte[] bytes = new byte[buffer.readableByteCount()];
            buffer.read(bytes);
            responseBuilder.append(new String(bytes, StandardCharsets.UTF_8));
        }

        return responseBuilder.toString();
    }

    /**
     * Handles processing errors by logging and completing the future exceptionally.
     *
     * @param operation the operation that failed
     * @param error     the error that occurred
     */
    private void handleProcessingError(final String operation, final Throwable error) {
        LOG.error("Error in {} operation for session {}: {}", operation, sessionId, error.getMessage(), error);
        responseFuture.completeExceptionally(error);
    }

    /**
     * Processes response data by applying response template transformations.
     * If no response template is configured, returns the original response body unchanged.
     * If a template is provided, attempts to parse the response as JSON and apply template
     * transformations including placeholder substitution.
     *
     * @param responseBody the original response body to process
     * @return the processed response body
     */
    private String processResponse(final String responseBody) {
        try {
            if (Objects.isNull(responseTemplate) || responseTemplate.size() == 0) {
                return responseBody;
            }

            // Attempt to parse response as JSON for template processing
            final JsonObject responseJson = parseResponseAsJson(responseBody);
            if (Objects.isNull(responseJson)) {
                LOG.debug("Response is not valid JSON for session: {}, returning unchanged", sessionId);
                return responseBody;
            }

            // Apply response template transformations
            return applyResponseTemplate(responseJson);

        } catch (Exception e) {
            LOG.error("Error applying response template for session {}: {}", sessionId, e.getMessage());
            return responseBody;
        }
    }

    /**
     * Attempts to parse the response body as JSON.
     *
     * @param responseBody the response body to parse
     * @return the parsed JSON object, or null if parsing fails
     */
    private JsonObject parseResponseAsJson(final String responseBody) {
        try {
            return GsonUtils.getInstance().fromJson(responseBody, JsonObject.class);
        } catch (Exception e) {
            LOG.debug("Failed to parse response as JSON for session: {}", sessionId);
            return null;
        }
    }

    /**
     * Applies response template transformations to the parsed JSON response.
     *
     * @param responseJson the parsed JSON response data
     * @return the transformed response as JSON string
     */
    private String applyResponseTemplate(final JsonObject responseJson) {
        final JsonObject processedResponse = new JsonObject();

        if (responseTemplate.has("content")) {
            final JsonObject contentTemplate = responseTemplate.getAsJsonObject("content");
            final JsonObject content = buildContentFromTemplate(contentTemplate, responseJson);
            processedResponse.add("content", content);
        } else {
            // If no specific template structure, return original response
            return GsonUtils.getInstance().toJson(responseJson);
        }

        return GsonUtils.getInstance().toJson(processedResponse);
    }

    /**
     * Builds content object from template and response data.
     *
     * @param contentTemplate the content template configuration
     * @param responseData    the response data for placeholder substitution
     * @return the built content JSON object
     */
    private JsonObject buildContentFromTemplate(final JsonObject contentTemplate, final JsonObject responseData) {
        final JsonObject content = new JsonObject();

        if (contentTemplate.has("type")) {
            content.addProperty("type", contentTemplate.get("type").getAsString());
        }

        if (contentTemplate.has("text")) {
            final String textTemplate = contentTemplate.get("text").getAsString();
            final String processedText = applyPlaceholderSubstitution(textTemplate, responseData);
            content.addProperty("text", processedText);
        }

        return content;
    }

    /**
     * Applies placeholder substitution to a text template using response data.
     * Supports simple placeholder format: ${fieldName}
     *
     * @param template     the text template containing placeholders
     * @param responseData the JSON object containing substitution values
     * @return the text with placeholders replaced by actual values
     */
    private String applyPlaceholderSubstitution(final String template, final JsonObject responseData) {
        if (Objects.isNull(responseData)) {
            return template;
        }

        String result = template;

        for (String key : responseData.keySet()) {
            final String placeholder = PLACEHOLDER_PREFIX + key + PLACEHOLDER_SUFFIX;
            if (result.contains(placeholder)) {
                final String value = responseData.get(key).isJsonPrimitive()
                        ? responseData.get(key).getAsString()
                        : responseData.get(key).toString();
                result = result.replace(placeholder, value);
            }
        }

        return result;
    }
} 