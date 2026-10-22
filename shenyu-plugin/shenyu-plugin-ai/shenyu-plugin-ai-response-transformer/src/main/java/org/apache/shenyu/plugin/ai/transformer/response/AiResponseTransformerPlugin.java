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

package org.apache.shenyu.plugin.ai.transformer.response;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiResponseTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiResponseTransformerHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.transformer.response.handler.AiResponseTransformerPluginHandler;
import org.apache.shenyu.plugin.ai.transformer.response.template.AiResponseTransformerTemplate;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import org.reactivestreams.Publisher;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * this is ai response transformer plugin.
 * The complete response is buffered before transformation; streaming and SSE responses are not supported.
 */
public class AiResponseTransformerPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiResponseTransformerPlugin.class);

    private static final Pattern REQUEST_LINE_PATTERN = Pattern.compile("^(GET|POST|PUT|DELETE|PATCH|OPTIONS|HEAD)\\s.*\\sHTTP/1.1$");

    private static final Pattern STATUS_LINE_PATTERN = Pattern.compile("^HTTP/1\\.[01]\\s+(\\d{3})(?:\\s+.*)?$");

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private final List<HttpMessageReader<?>> messageReaders;

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    public AiResponseTransformerPlugin(final List<HttpMessageReader<?>> messageReaders, final AiModelFactoryRegistry aiModelFactoryRegistry) {
        this.messageReaders = messageReaders;
        this.aiModelFactoryRegistry = aiModelFactoryRegistry;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        AiResponseTransformerConfig aiResponseTransformerConfig = Singleton.INST.get(AiResponseTransformerConfig.class);
        if (Objects.isNull(aiResponseTransformerConfig)) {
            aiResponseTransformerConfig = new AiResponseTransformerConfig();
        }

        AiResponseTransformerHandle aiResponseTransformerHandle = AiResponseTransformerPluginHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        ChatClient client = ChatClientCache.getInstance().getClient("default");
        // Create final config with rule handle taking precedence
        if (Objects.nonNull(aiResponseTransformerHandle)) {
            aiResponseTransformerConfig = new AiResponseTransformerConfig();
            Optional.ofNullable(aiResponseTransformerHandle.getProvider()).ifPresent(aiResponseTransformerConfig::setProvider);
            Optional.ofNullable(aiResponseTransformerHandle.getBaseUrl()).ifPresent(aiResponseTransformerConfig::setBaseUrl);
            Optional.ofNullable(aiResponseTransformerHandle.getApiKey()).ifPresent(aiResponseTransformerConfig::setApiKey);
            Optional.ofNullable(aiResponseTransformerHandle.getModel()).ifPresent(aiResponseTransformerConfig::setModel);
            Optional.ofNullable(aiResponseTransformerHandle.getContent()).ifPresent(aiResponseTransformerConfig::setContent);
            client = ChatClientCache.getInstance().getClient(rule.getId());
        }

        String baseUrl = aiResponseTransformerConfig.getBaseUrl();
        String apiKey = aiResponseTransformerConfig.getApiKey();
        String provider = aiResponseTransformerConfig.getProvider();
        if (Stream.of(baseUrl, apiKey, provider).anyMatch(Objects::isNull)) {
            String missing = "";
            missing += Objects.isNull(baseUrl) ? "baseUrl, " : "";
            missing += Objects.isNull(apiKey) ? "apiKey, " : "";
            missing += Objects.isNull(provider) ? "provider, " : "";

            LOG.error("Missing configurations: {}", missing.substring(0, missing.length() - 2));
            return chain.execute(exchange);
        }

        if (Objects.isNull(client)) {
            ChatModel aiModel = aiModelFactoryRegistry
                    .getFactory(AiModelProviderEnum.getByName(provider))
                    .createAiModel(AiResponseTransformerPluginHandler.convertConfig(aiResponseTransformerConfig));
            client = ChatClientCache.getInstance().init(rule.getId(), aiModel);
        }

        ChatClient finalClient = client;
        AiResponseTransformerTemplate aiResponseTransformerTemplate = new AiResponseTransformerTemplate(aiResponseTransformerConfig.getContent(), exchange.getRequest());
        
        return chain.execute(exchange.mutate()
                .response(new AiResponseTransformerDecorator(exchange, aiResponseTransformerTemplate, finalClient))
                .build());
    }



    @Override
    public int getOrder() {
        return PluginEnum.AI_RESPONSE_TRANSFORMER.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_RESPONSE_TRANSFORMER.getName();
    }

    /**
     * For unit test.
     *
     * @param aiResponse the AI response
     * @return the extracted body
     */
    public static String extractBodyFromAiResponse(final String aiResponse) {
        return AiResponseParser.extractBodyFromAiResponse(aiResponse);
    }

    /**
     * For unit test.
     *
     * @param aiResponse the AI response
     * @return the extracted headers
     */
    public static HttpHeaders extractHeadersFromAiResponse(final String aiResponse) {
        return AiResponseParser.extractHeadersFromAiResponse(aiResponse);
    }

    /**
     * For unit test.
     *
     * @param aiResponse the AI response
     * @return the extracted status code, or {@code null} when the response has no usable status line
     */
    public static Integer extractStatusFromAiResponse(final String aiResponse) {
        return AiResponseParser.extractStatusFromAiResponse(aiResponse);
    }

    /**
     * Utility class for parsing AI responses.
     */
    private static class AiResponseParser {

        /**
         * Remove the surrounding markdown code block markers, if any.
         *
         * @param aiResponse the AI response
         * @return the response without code block markers
         */
        static String stripCodeBlockMarkers(final String aiResponse) {
            String cleanedResponse = aiResponse;
            if (cleanedResponse.contains("```")) {
                int startIndex = cleanedResponse.indexOf("```");
                int endIndex = cleanedResponse.lastIndexOf("```");

                if (startIndex != -1 && endIndex != -1 && endIndex > startIndex) {
                    cleanedResponse = cleanedResponse.substring(startIndex + 3, endIndex).trim();
                    LOG.debug("Removed code block markers, cleaned response: {}", cleanedResponse);
                }
            }
            return cleanedResponse;
        }

        /**
         * Extract body from AI response.
         *
         * @param aiResponse the AI response
         * @return the extracted body
         */
        static String extractBodyFromAiResponse(final String aiResponse) {
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                return null;
            }

            String cleanedResponse = stripCodeBlockMarkers(aiResponse);

            if (cleanedResponse.startsWith("{") && cleanedResponse.endsWith("}")
                    || cleanedResponse.startsWith("[") && cleanedResponse.endsWith("]")) {
                return cleanedResponse;
            }

            String[] lines = cleanedResponse.split("\\R");

            int emptyLineIndex = -1;
            for (int i = 0; i < lines.length; i++) {
                if (lines[i].trim().isEmpty()) {
                    emptyLineIndex = i;
                    break;
                }
            }

            if (emptyLineIndex == -1 || emptyLineIndex == lines.length - 1) {
                return null;
            }

            StringBuilder bodyBuilder = new StringBuilder();
            for (int i = emptyLineIndex + 1; i < lines.length; i++) {
                bodyBuilder.append(lines[i]);
                if (i < lines.length - 1) {
                    bodyBuilder.append("\n");
                }
            }

            String body = bodyBuilder.toString().trim();

            if (body.startsWith("{") && body.endsWith("}") || body.startsWith("[") && body.endsWith("]")) {
                try {
                    MAPPER.readTree(body);
                    return body;
                } catch (Exception e) {
                    LOG.warn("Body is not valid JSON: {}", body);
                    return null;
                }
            }

            return null;
        }

        /**
         * Extract headers from AI response.
         *
         * @param aiResponse the AI response
         * @return the extracted headers
         */
        static HttpHeaders extractHeadersFromAiResponse(final String aiResponse) {
            HttpHeaders headers = new HttpHeaders();
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                return headers;
            }

            String cleanedResponse = stripCodeBlockMarkers(aiResponse);

            try (BufferedReader reader = new BufferedReader(new StringReader(cleanedResponse))) {
                String line;
                boolean headerSectionStarted = false;
                while (Objects.nonNull(line = reader.readLine())) {
                    if (!headerSectionStarted) {
                        if (STATUS_LINE_PATTERN.matcher(line.trim()).matches() || REQUEST_LINE_PATTERN.matcher(line.trim()).matches()) {
                            headerSectionStarted = true;
                            continue;
                        }
                    } else {
                        if (line.trim().isEmpty()) {
                            break;
                        }
                        int colonIndex = line.indexOf(":");
                        if (colonIndex > 0) {
                            String name = line.substring(0, colonIndex).trim();
                            String value = line.substring(colonIndex + 1).trim();
                            headers.add(name, value);
                        }
                    }
                }
            } catch (IOException e) {
                LOG.error("AI response transformer plugin: extract headers from AiResponse fail");
            }
            return headers;
        }

        /**
         * Extract the status code from the status line of an AI response.
         * The status line is only accepted as the first non-empty line, so a status-looking
         * header value further down cannot change the response status.
         *
         * @param aiResponse the AI response
         * @return the extracted status code, or {@code null} when there is no valid status line
         */
        static Integer extractStatusFromAiResponse(final String aiResponse) {
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                return null;
            }

            String cleanedResponse = stripCodeBlockMarkers(aiResponse);
            try (BufferedReader reader = new BufferedReader(new StringReader(cleanedResponse))) {
                String line;
                while (Objects.nonNull(line = reader.readLine())) {
                    if (line.trim().isEmpty()) {
                        continue;
                    }
                    Matcher matcher = STATUS_LINE_PATTERN.matcher(line.trim());
                    if (!matcher.matches()) {
                        return null;
                    }
                    int statusCode = Integer.parseInt(matcher.group(1));
                    // HttpStatus resolves only codes defined by the HTTP specification.
                    if (Objects.isNull(HttpStatus.resolve(statusCode))) {
                        LOG.warn("AI response contains an invalid HTTP status code: {}", statusCode);
                        return null;
                    }
                    return statusCode;
                }
            } catch (IOException | NumberFormatException e) {
                LOG.warn("AI response transformer plugin: extract status from AiResponse fail");
            }
            return null;
        }
    }

    static class AiResponseTransformerDecorator extends ServerHttpResponseDecorator {

        private final ServerWebExchange exchange;

        private final AiResponseTransformerTemplate aiResponseTransformerTemplate;

        private final ChatClient chatClient;

        AiResponseTransformerDecorator(final ServerWebExchange exchange,
                                       final AiResponseTransformerTemplate aiResponseTransformerTemplate,
                                       final ChatClient chatClient) {
            super(exchange.getResponse());
            this.exchange = exchange;
            this.aiResponseTransformerTemplate = aiResponseTransformerTemplate;
            this.chatClient = chatClient;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            final Mono<DataBuffer> dataBufferMono = DataBufferUtils.join(body);
            return dataBufferMono.map(dataBuffer -> {
                byte[] bytes = new byte[dataBuffer.readableByteCount()];
                dataBuffer.read(bytes);
                DataBufferUtils.release(dataBuffer);
                String originalResponseBody = new String(bytes, StandardCharsets.UTF_8);

                String contentEncoding = exchange.getResponse().getHeaders().getFirst("Content-Encoding");
                if ("gzip".equalsIgnoreCase(contentEncoding)) {
                    LOG.debug("Detected gzip encoding, attempting to decompress");
                    try (GZIPInputStream gis = new GZIPInputStream(new ByteArrayInputStream(bytes));
                         ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
                        byte[] buffer = new byte[1024];
                        int len;
                        while ((len = gis.read(buffer)) > 0) {
                            bos.write(buffer, 0, len);
                        }
                        originalResponseBody = bos.toString(StandardCharsets.UTF_8.name());
                        LOG.debug("Decompressed response body: {}", originalResponseBody);
                    } catch (Exception e) {
                        LOG.error("Failed to decompress gzip response", e);
                    }
                }
                
                final String finalResponseBody = originalResponseBody;
                
                return aiResponseTransformerTemplate.assembleMessage(exchange)
                        .flatMap(message -> {

                            String messageWithResponseBody;
                            try {
                                JsonNode messageNode = MAPPER.readTree(message);

                                if (messageNode.has("response") && messageNode.get("response").isObject()) {
                                    ObjectNode responseNode = (ObjectNode) messageNode.get("response");
                                    responseNode.put("body", finalResponseBody);
                                }
                                
                                messageWithResponseBody = MAPPER.writeValueAsString(messageNode);
                            } catch (Exception e) {
                                LOG.error("Failed to update message with response body", e);
                                messageWithResponseBody = message.replace("\"body\":\"\"", "\"body\":\"" + finalResponseBody.replace("\"", "\\\"") + "\"");
                                LOG.info("Fallback message with response body: {}", messageWithResponseBody);
                            }
                            
                            final String finalMessage = messageWithResponseBody;
                            return chatClient.prompt().user(finalMessage).stream().content()
                                    .filter(Objects::nonNull)
                                    .collect(StringBuilder::new, StringBuilder::append)
                                    .map(StringBuilder::toString)
                                    .flatMap(aiResponse -> applyTransformedResponse(aiResponse, finalResponseBody))
                                    .onErrorResume(throwable -> {
                                        LOG.error("response transformation failed", throwable);
                                        LOG.info("Returning original response due to error: {}", finalResponseBody);
                                        return WebFluxResultUtils.result(this.exchange, finalResponseBody.getBytes(StandardCharsets.UTF_8));
                                    });
                        });
            }).defaultIfEmpty(Mono.defer(() -> super.writeWith(Flux.empty())))
                    .flatMap(response -> response);
        }

        @Override
        @NonNull
        public Mono<Void> writeAndFlushWith(@NonNull final Publisher<? extends Publisher<? extends DataBuffer>> body) {
            return writeWith(Flux.from(body).concatMap(Flux::from));
        }

        private Mono<Void> applyTransformedResponse(final String aiResponse, final String originalResponseBody) {
            HttpHeaders newHeaders = extractHeadersFromAiResponse(aiResponse);
            String newBody = extractBodyFromAiResponse(aiResponse);
            Integer newStatus = extractStatusFromAiResponse(aiResponse);

            if (isBodylessStatus(newStatus)) {
                // 204/304 must not carry a body, so the generated status and headers are applied as-is
                this.getHeaders().clear();
                this.getHeaders().putAll(newHeaders);
                this.getHeaders().remove(HttpHeaders.CONTENT_TYPE);
                this.getHeaders().remove(HttpHeaders.CONTENT_LENGTH);
                this.getHeaders().remove(HttpHeaders.TRANSFER_ENCODING);
                this.setStatusCode(HttpStatus.valueOf(newStatus));
                LOG.debug("Returning transformed bodyless response with status: {}", newStatus);
                return super.writeWith(Flux.empty());
            }
            if (Objects.nonNull(newBody) && !newBody.isEmpty()) {
                this.getHeaders().clear();
                this.getHeaders().putAll(newHeaders);
                if (Objects.nonNull(newStatus)) {
                    this.setStatusCode(HttpStatus.valueOf(newStatus));
                }
                LOG.debug("Returning transformed response body: {}", newBody);
                return WebFluxResultUtils.result(this.exchange, newBody.getBytes(StandardCharsets.UTF_8));
            }
            // the generated response has no usable body, so the original response (and its status) is kept
            LOG.warn("response body is empty or null, returning original response: {}", originalResponseBody);
            return WebFluxResultUtils.result(this.exchange, originalResponseBody.getBytes(StandardCharsets.UTF_8));
        }

        private String extractBodyFromAiResponse(final String aiResponse) {
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                LOG.warn("response is null or empty");
                return null;
            }

            String result = AiResponseParser.extractBodyFromAiResponse(aiResponse);
            if (Objects.isNull(result)) {
                LOG.warn("Could not extract body from AI response");
            } else {
                LOG.debug("Successfully extracted body from AI response: {}", result);
            }
            return result;
        }

        private static boolean isBodylessStatus(final Integer statusCode) {
            if (Objects.isNull(statusCode)) {
                return false;
            }
            HttpStatus status = HttpStatus.resolve(statusCode);
            return HttpStatus.NO_CONTENT.equals(status) || HttpStatus.NOT_MODIFIED.equals(status);
        }

        private HttpHeaders extractHeadersFromAiResponse(final String aiResponse) {
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                LOG.warn("AI response is null or empty for header extraction");
                return new HttpHeaders();
            }

            HttpHeaders headers = AiResponseParser.extractHeadersFromAiResponse(aiResponse);
            LOG.debug("Extracted {} headers: {}", headers.size(), headers);
            return headers;
        }
    }
} 
