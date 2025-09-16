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

package org.apache.shenyu.plugin.ai.common.strategy.openai;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.common.strategy.AiModel;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The OpenAI model.
 */
public class OpenAI implements AiModel {
    
    private static final Logger LOG = LoggerFactory.getLogger(OpenAI.class);
    
    @Override
    public Mono<Void> invoke(final AiCommonConfig aiCommonConfig, final ServerWebExchange exchange,
                             final ShenyuPluginChain chain,
                             final List<HttpMessageReader<?>> messageReaders) {
        ServerWebExchange modifiedExchange = exchange.mutate()
                .request(originalRequest -> originalRequest
                        .headers(httpHeaders -> convertHeader(httpHeaders, aiCommonConfig))
                        .method(exchange.getRequest().getMethod())
                )
                .build();
        return ServerWebExchangeUtils.rewriteRequestBody(modifiedExchange, messageReaders, originalBody ->
                        Mono.just(convertBody(originalBody, aiCommonConfig))
                ).flatMap(chain::execute)
                .onErrorResume(error -> {
                    if (error instanceof ResponsiveException) {
                        return WebFluxResultUtils.failedResult((ResponsiveException) error);
                    }
                    return Mono.error(error);
                });
    }

    @Override
    public Long getCompletionTokens(final String responseBody) {
        try {
            JsonNode root = JsonUtils.toJsonNode(responseBody);
            JsonNode usage = root.get(Constants.USAGE);
            if (Objects.nonNull(usage)) {
                JsonNode totalTokens = usage.get(Constants.COMPLETION_TOKENS);
                if (Objects.nonNull(totalTokens)) {
                    return totalTokens.asLong();
                }
            }
        } catch (Exception e) {
            // Handle parsing exceptions
            LOG.error("Failed to parse response body: {}", responseBody, e);
        }
        return 0L;
    }
    
    
    private static void convertHeader(final HttpHeaders httpHeaders, final AiCommonConfig aiCommonConfig) {
        if (!httpHeaders.containsKey("Authorization")) {
            httpHeaders.add("Authorization", "Bearer " + aiCommonConfig.getApiKey());
        }
        if (aiCommonConfig.getStream()) {
            httpHeaders.add(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_EVENT_STREAM_VALUE);
            httpHeaders.add(HttpHeaders.CACHE_CONTROL, "no-cache");
            httpHeaders.add(HttpHeaders.CONNECTION, "keep-alive");
        }
    }
    
    private String convertBody(final String originalBody, final AiCommonConfig aiCommonConfig) {
        Map<String, Object> requestBodyMap = GsonUtils.getInstance().convertToMap(originalBody);
        requestBodyMap.put(Constants.MODEL, aiCommonConfig.getModel());
        requestBodyMap.put(Constants.STREAM, aiCommonConfig.getStream());
        if (aiCommonConfig.getStream()) {
            Map<String, Object> streamOptions = new HashMap<>();
            streamOptions.put(Constants.INCLUDE_USAGE, true);
            requestBodyMap.put(Constants.STREAM_OPTIONS, streamOptions);
        }
        return GsonUtils.getInstance().toJson(requestBodyMap);
    }
    
}
