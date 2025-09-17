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

package org.apache.shenyu.plugin.ai.transformer.request;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiRequestTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiRequestTransformerHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.request.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.transformer.request.handler.AiRequestTransformerPluginHandler;
import org.apache.shenyu.plugin.ai.transformer.request.template.AiRequestTransformerTemplate;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * this is ai request transformer plugin.
 */
public class AiRequestTransformerPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiRequestTransformerPlugin.class);

    private final List<HttpMessageReader<?>> messageReaders;

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    public AiRequestTransformerPlugin(final List<HttpMessageReader<?>> messageReaders, final AiModelFactoryRegistry aiModelFactoryRegistry) {
        this.messageReaders = messageReaders;
        this.aiModelFactoryRegistry = aiModelFactoryRegistry;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        AiRequestTransformerConfig aiRequestTransformerConfig = Singleton.INST.get(AiRequestTransformerConfig.class);
        if (Objects.isNull(aiRequestTransformerConfig)) {
            aiRequestTransformerConfig = new AiRequestTransformerConfig();
        }

        AiRequestTransformerHandle aiRequestTransformerHandle = AiRequestTransformerPluginHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        ChatClient client = ChatClientCache.getInstance().getClient("default");
        // Create final config with rule handle taking precedence
        if (Objects.nonNull(aiRequestTransformerHandle)) {
            aiRequestTransformerConfig = new AiRequestTransformerConfig();
            Optional.ofNullable(aiRequestTransformerHandle.getProvider()).ifPresent(aiRequestTransformerConfig::setProvider);
            Optional.ofNullable(aiRequestTransformerHandle.getBaseUrl()).ifPresent(aiRequestTransformerConfig::setBaseUrl);
            Optional.ofNullable(aiRequestTransformerHandle.getApiKey()).ifPresent(aiRequestTransformerConfig::setApiKey);
            Optional.ofNullable(aiRequestTransformerHandle.getModel()).ifPresent(aiRequestTransformerConfig::setModel);
            Optional.ofNullable(aiRequestTransformerHandle.getContent()).ifPresent(aiRequestTransformerConfig::setContent);
            client = ChatClientCache.getInstance().getClient(rule.getId());
        }

        String baseUrl = aiRequestTransformerConfig.getBaseUrl();
        String apiKey = aiRequestTransformerConfig.getApiKey();
        String provider = aiRequestTransformerConfig.getProvider();
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
                    .createAiModel(AiRequestTransformerPluginHandler.convertConfig(aiRequestTransformerConfig));
            client = ChatClientCache.getInstance().init(rule.getId(), aiModel);
        }

        AiRequestTransformerTemplate aiRequestTransformerTemplate = new AiRequestTransformerTemplate(aiRequestTransformerConfig.getContent(), exchange.getRequest());
        ChatClient finalClient = client;
        return aiRequestTransformerTemplate.assembleMessage()
                .flatMap(message -> Mono.fromCallable(() -> finalClient.prompt().user(message).call().content())
                        .subscribeOn(Schedulers.boundedElastic())
                        .flatMap(aiResponse -> convertHeader(exchange, aiResponse)
                                .flatMap(serverWebExchange -> convertBody(serverWebExchange, messageReaders, aiResponse))
                                    .flatMap(chain::execute)
                        )
                );

    }

    private static Mono<ServerWebExchange> convertBody(final ServerWebExchange exchange,
                                                       final List<HttpMessageReader<?>> readers,
                                                       final String aiResponse) {
        MediaType mediaType = exchange.getRequest().getHeaders().getContentType();
        if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
            return ServerWebExchangeUtils.rewriteRequestBody(exchange, readers, requestBodyString ->
                    Mono.just(convertBodyJson(aiResponse)));
        } else if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(mediaType)) {
            return ServerWebExchangeUtils.rewriteRequestBody(exchange, readers, requestBodyString ->
                    Mono.just(convertBodyFormData(aiResponse)));
        } else {
            return Mono.just(exchange);
        }

    }

    /**
     * For unit test.
     */
    static String convertBodyJson(final String aiResponse) {
        return extractJsonBodyFromHttpResponse(aiResponse);
    }

    /**
     * For unit test.
     */
    static String convertBodyFormData(final String aiResponse) {
        Map<String, Object> formDataMap = GsonUtils.getInstance().toObjectMap(extractJsonBodyFromHttpResponse(aiResponse));
        return mapToFormUrlEncoded(formDataMap);
    }

    public static String extractJsonBodyFromHttpResponse(final String aiResponse) {
        if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
            return null;
        }

        String[] lines = aiResponse.split("\\R");

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
            bodyBuilder.append("\n");
        }

        String body = bodyBuilder.toString().trim();

        if (body.startsWith("{") && body.endsWith("}") || body.startsWith("[") && body.endsWith("]")) {
            Map<String, Object> requestBodyMap = GsonUtils.getInstance().convertToMap(body);
            return GsonUtils.getInstance().toJson(requestBodyMap);
        }

        return null;
    }

    public static String mapToFormUrlEncoded(final Map<String, Object> map) {
        StringBuilder sb = new StringBuilder();
        try {
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                if (sb.length() > 0) {
                    sb.append("&");
                }
                sb.append(URLEncoder.encode(entry.getKey(), "UTF-8"));
                sb.append("=");
                sb.append(URLEncoder.encode(String.valueOf(entry.getValue()), "UTF-8"));
            }
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
        return sb.toString();
    }

    private static Mono<ServerWebExchange> convertHeader(final ServerWebExchange exchange, final String aiResponse) {
        HttpHeaders newHeaders = extractHeadersFromAiResponse(aiResponse);
        exchange.getRequest().mutate().headers(httpHeaders -> {
            httpHeaders.clear();
            httpHeaders.putAll(newHeaders);
        }).build();
        return Mono.just(exchange);
    }

    /**
     * For unit test.
     */
    static HttpHeaders extractHeadersFromAiResponse(final String aiResponse) {
        HttpHeaders headers = new HttpHeaders();
        if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
            return headers;
        }
        try (BufferedReader reader = new BufferedReader(new StringReader(aiResponse))) {
            String line;
            boolean headerSectionStarted = false;
            while (Objects.nonNull(line = reader.readLine())) {
                if (!headerSectionStarted) {
                    if (line.startsWith("HTTP/1.1") || line.matches("^(GET|POST|PUT|DELETE|PATCH|OPTIONS|HEAD)\\s.*\\sHTTP/1.1$")) {
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
            LOG.error("AI request transformer plugin: extract headers from AiResponse fail");
        }
        return headers;
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_REQUEST_TRANSFORMER.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_REQUEST_TRANSFORMER.getName();
    }

}

