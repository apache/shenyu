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

import com.google.gson.JsonElement;
import org.apache.shenyu.common.constant.Constants;
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
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ResponseUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * this is ai request transformer plugin.
 */
public class AiRequestTransformerPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiRequestTransformerPlugin.class);

    private static final Pattern REQUEST_LINE_PATTERN = Pattern.compile("^(GET|POST|PUT|DELETE|PATCH|OPTIONS|HEAD)\\s.*\\sHTTP/1.1$");

    /**
     * Valid characters of a generated origin-form path component.
     */
    private static final Pattern REQUEST_PATH_PATTERN = Pattern.compile("^/[a-zA-Z0-9/_\\-]*$");

    /**
     * Valid characters of a generated query component: RFC 3986 query syntax (percent-encoded
     * sequences must be well formed so that the target is always a legal URI reference).
     */
    private static final Pattern QUERY_PATTERN = Pattern.compile("^(%[0-9a-fA-F]{2}|[a-zA-Z0-9\\-._~!$&'()*+,;=:@/?])*$");

    /**
     * Maximum number of request body bytes buffered for the model prompt and for replaying the
     * original body when the transformation does not apply. A larger body is forwarded untouched
     * without AI transformation, so the buffer stays bounded.
     */
    private static final int MAX_REQUEST_BODY_SIZE_BYTES = 5 * 1024 * 1024;

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    public AiRequestTransformerPlugin(final AiModelFactoryRegistry aiModelFactoryRegistry) {
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

        // The declared size is checked before the body is buffered, so an oversized request is
        // forwarded without transformation instead of being collected in memory.
        long declaredSize = exchange.getRequest().getHeaders().getContentLength();
        if (declaredSize > MAX_REQUEST_BODY_SIZE_BYTES) {
            LOG.warn("AI request transformer skipped: the request body of {} bytes exceeds the {} byte limit",
                    declaredSize, MAX_REQUEST_BODY_SIZE_BYTES);
            return chain.execute(exchange);
        }

        ChatClient finalClient = client;
        AiRequestTransformerConfig finalConfig = aiRequestTransformerConfig;
        // The inbound body is subscribed once. Its bytes are then replayed through a decorator, so
        // the model prompt and the fallback path never touch the one-shot network body again.
        return readRequestBody(exchange.getRequest().getBody())
                .flatMap(requestBody -> transform(exchange, chain, finalConfig, finalClient, requestBody));
    }

    private Mono<Void> transform(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                 final AiRequestTransformerConfig config, final ChatClient client,
                                 final byte[] requestBody) {
        ServerWebExchange replayExchange = replayBody(exchange, requestBody);
        AiRequestTransformerTemplate aiRequestTransformerTemplate =
                new AiRequestTransformerTemplate(config.getContent(), replayExchange.getRequest());
        String requestBodyString = new String(requestBody, StandardCharsets.UTF_8);
        // Recover model, preparation and transformation failures before entering the downstream chain:
        // re-entering the chain after it has started would resume a partially advanced cursor instead
        // of replaying the original request. The replay exchange carries the original body intact.
        return aiRequestTransformerTemplate.assembleMessage(requestBodyString)
                .flatMap(message -> client.prompt().user(message).stream().content()
                        .filter(Objects::nonNull)
                        .collect(StringBuilder::new, StringBuilder::append)
                        .map(StringBuilder::toString)
                        .flatMap(aiResponse -> {
                            LOG.debug("Request rewritten to: {}", aiResponse);
                            return convertHeader(replayExchange, aiResponse)
                                    .flatMap(serverWebExchange -> convertBody(serverWebExchange, aiResponse))
                                    .flatMap(serverWebExchange -> rewriteRequestPath(serverWebExchange, aiResponse));
                        })
                )
                .onErrorResume(throwable -> {
                    LOG.error("AI request transformation failed, proceeding without AI transformation", throwable);
                    return Mono.just(replayExchange);
                })
                .flatMap(chain::execute);
    }

    /**
     * Reads the inbound body once, bounded by {@link #MAX_REQUEST_BODY_SIZE_BYTES}. The joined
     * buffer is released before the bytes are cached, so no pooled buffer outlives this call.
     *
     * @param body the inbound body stream
     * @return the body bytes, empty when the request has no body
     */
    private static Mono<byte[]> readRequestBody(final Flux<DataBuffer> body) {
        return DataBufferUtils.join(body, MAX_REQUEST_BODY_SIZE_BYTES)
                .map(dataBuffer -> {
                    try {
                        byte[] bytes = new byte[dataBuffer.readableByteCount()];
                        dataBuffer.read(bytes);
                        return bytes;
                    } finally {
                        DataBufferUtils.release(dataBuffer);
                    }
                })
                .defaultIfEmpty(new byte[0]);
    }

    /**
     * Replays the original body from the cached bytes. The original headers are kept because the
     * body is forwarded unchanged, including its content length.
     *
     * @param exchange the exchange to decorate
     * @param body     the original body bytes
     * @return an exchange whose request replays the original body
     */
    private static ServerWebExchange replayBody(final ServerWebExchange exchange, final byte[] body) {
        return exchange.mutate().request(bodyRequest(exchange, body, false)).build();
    }

    private static Mono<ServerWebExchange> convertBody(final ServerWebExchange exchange, final String aiResponse) {
        MediaType mediaType = exchange.getRequest().getHeaders().getContentType();
        if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
            return replaceBody(exchange, convertBodyJson(aiResponse));
        } else if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(mediaType)) {
            return replaceBody(exchange, convertBodyFormData(aiResponse));
        } else {
            return Mono.just(exchange);
        }

    }

    /**
     * Builds the transformed body from the generated content, without reading the inbound body.
     *
     * @param exchange  the exchange to rewrite
     * @param body      the generated body, or null when the model answer has no usable body
     * @return an exchange carrying the generated body
     */
    private static Mono<ServerWebExchange> replaceBody(final ServerWebExchange exchange, final String body) {
        if (Objects.isNull(body)) {
            return Mono.error(new IllegalStateException("The model answer does not contain a transformable body"));
        }
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        // The generated body has a different length, so the original content length no longer applies.
        return Mono.just(exchange.mutate().request(bodyRequest(exchange, bytes, true)).build());
    }

    private static ServerHttpRequest bodyRequest(final ServerWebExchange exchange, final byte[] body, final boolean chunked) {
        ServerHttpRequest request = exchange.getRequest();
        DataBufferFactory bufferFactory = exchange.getResponse().bufferFactory();
        return new ServerHttpRequestDecorator(request) {
            @Override
            public Flux<DataBuffer> getBody() {
                return body.length == 0 ? Flux.empty() : Flux.defer(() -> Flux.just(bufferFactory.wrap(body)));
            }

            @Override
            public HttpHeaders getHeaders() {
                return chunked ? ResponseUtils.chunkedHeader(request.getHeaders()) : request.getHeaders();
            }
        };
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
            GsonUtils.getInstance().fromJson(body, JsonElement.class);
            return body;
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

    private static Mono<ServerWebExchange> rewriteRequestPath(final ServerWebExchange exchange, final String aiResponse) {
        String newTarget = extractRequestPathFromAiResponse(aiResponse);

        if (Objects.isNull(newTarget) || newTarget.isEmpty()) {
            return Mono.just(exchange);
        }

        if (newTarget.contains("..")) {
            LOG.warn("Detected potential path traversal attempt in extracted path: {} , Will continue to use the original path.", newTarget);
            return Mono.just(exchange);
        }

        // The model answers with an origin-form request target, so it may carry a query string.
        // Validate the path and the query separately, then apply both to the request URI.
        int queryIndex = newTarget.indexOf('?');
        String newPath = queryIndex < 0 ? newTarget : newTarget.substring(0, queryIndex);
        String newQuery = queryIndex < 0 ? null : newTarget.substring(queryIndex + 1);

        if (!newPath.startsWith("/")) {
            LOG.warn("Extracted path does not start with '/': {} , Will continue to use the original path.", newPath);
            return Mono.just(exchange);
        }

        if (!REQUEST_PATH_PATTERN.matcher(newPath).matches()) {
            LOG.warn("Extracted path contains invalid characters: {}, Will continue to use the original path.", newPath);
            return Mono.just(exchange);
        }

        if (Objects.nonNull(newQuery) && !QUERY_PATTERN.matcher(newQuery).matches()) {
            LOG.warn("Extracted query contains invalid characters: {}, Will continue to use the original path.", newQuery);
            return Mono.just(exchange);
        }

        String rewrittenTarget = Objects.isNull(newQuery) ? newPath : newPath + "?" + newQuery;

        LOG.debug("Request path after validation and rewriting: {}", rewrittenTarget);

        ServerHttpRequest originalRequest = exchange.getRequest();
        URI originalUri = originalRequest.getURI();

        URI newUri = originalUri.resolve(rewrittenTarget);

        ServerHttpRequest newRequest = originalRequest.mutate()
                .uri(newUri)
                .build();

        ServerWebExchange newExchange = exchange.mutate()
                .request(newRequest)
                .build();

        ShenyuContext context = newExchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(context)) {
            String rewrittenPath = newUri.getRawPath();
            // Preserve an already transformed forwarding path.
            if (Objects.equals(context.getRealUrl(), context.getPath())) {
                context.setRealUrl(rewrittenPath);
            }
            context.setPath(rewrittenPath);
        }

        return Mono.just(newExchange);
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
                    if (line.startsWith("HTTP/1.1") || REQUEST_LINE_PATTERN.matcher(line).matches()) {
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

    /**
     * For unit test.
     */
    static String extractRequestPathFromAiResponse(final String aiResponse) {
        if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
            return null;
        }
        String[] lines = aiResponse.split("\r?\n");
        if (lines.length == 0) {
            return null;
        }

        String startLine = lines[0].trim();
        String[] parts = startLine.split(" ");
        if (parts.length < 2) {
            return null;
        }

        return parts[1];
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
