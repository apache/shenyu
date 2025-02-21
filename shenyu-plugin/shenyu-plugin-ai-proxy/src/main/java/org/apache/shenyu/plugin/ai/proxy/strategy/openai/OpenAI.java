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

package org.apache.shenyu.plugin.ai.proxy.strategy.openai;

import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.BufferedSource;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.plugin.AiProxyConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.proxy.strategy.AiModel;
import org.jetbrains.annotations.NotNull;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * The OpenAI model.
 */
public class OpenAI implements AiModel {
    
    private final OkHttpClient client = new OkHttpClient.Builder()
            .connectTimeout(30, TimeUnit.SECONDS)
            .writeTimeout(5, TimeUnit.MINUTES)
            .readTimeout(5, TimeUnit.MINUTES)
            .build();
    
    @Override
    public Mono<Void> invoke(final AiProxyConfig aiProxyConfig, final ServerWebExchange exchange,
                             final String requestBody) {
        Map<String, Object> paramMap = GsonUtils.getInstance().convertToMap(requestBody);
        paramMap.put(Constants.MODEL, aiProxyConfig.getModel());
        paramMap.put(Constants.STREAM, aiProxyConfig.getStream());
        paramMap.put(Constants.PROMPT, aiProxyConfig.getPrompt());
        
        RequestBody body = RequestBody.create(MediaType.parse("application/json"),
                GsonUtils.getInstance().toJson(paramMap));
        Request request = buildRequest(aiProxyConfig, body);
        
        return aiProxyConfig.getStream()
                ? handleStreamResponse(exchange, request)
                : handleNormalResponse(exchange, request);
    }
    
    private Request buildRequest(final AiProxyConfig config, final RequestBody body) {
        return new Request.Builder()
                .url(config.getBaseUrl())
                .post(body)
                .addHeader("Content-Type", "application/json")
                .addHeader("Authorization", "Bearer " + config.getApiKey())
                .build();
    }
    
    private Mono<Void> handleStreamResponse(final ServerWebExchange exchange, final Request request) {
        ServerHttpResponse exchangeResponse = exchange.getResponse();
        exchangeResponse.getHeaders().set(HttpHeaders.CONTENT_TYPE,
                org.springframework.http.MediaType.TEXT_EVENT_STREAM_VALUE);
        exchangeResponse.getHeaders().set(HttpHeaders.CACHE_CONTROL, "no-cache");
        exchangeResponse.getHeaders().set(HttpHeaders.CONNECTION, "keep-alive");
        
        return exchangeResponse.writeAndFlushWith(
                Flux.<Publisher<DataBuffer>>create(
                        sink -> client.newCall(request).enqueue(new StreamResponseCallback(sink, exchangeResponse)),
                        FluxSink.OverflowStrategy.BUFFER));
    }
    
    private Mono<Void> handleNormalResponse(final ServerWebExchange exchange, final Request request) {
        return Mono.create(sink -> {
            try (Response response = client.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    sink.error(new IOException("Request failed: " + response));
                    return;
                }
                
                String responseBody = response.body().string();
                exchange.getResponse().getHeaders().setContentType(org.springframework.http.MediaType.APPLICATION_JSON);
                exchange.getResponse().writeWith(
                                Mono.just(exchange.getResponse().bufferFactory()
                                        .wrap(responseBody.getBytes(StandardCharsets.UTF_8))))
                        .subscribe(v -> sink.success(), sink::error);
            } catch (IOException e) {
                sink.error(e);
            }
        });
    }
    
    private record StreamResponseCallback(FluxSink<Publisher<DataBuffer>> sink, ServerHttpResponse response) implements Callback {
        
        @Override
        public void onFailure(@NotNull final Call call, @NotNull final IOException e) {
            sink.error(e);
        }
        
        @Override
        public void onResponse(@NotNull final Call call, @NotNull final Response response) {
            try (ResponseBody responseBody = response.body()) {
                if (!response.isSuccessful() || Objects.isNull(responseBody)) {
                    sink.error(new IOException("Request failed: " + response));
                    return;
                }
                
                processStreamResponse(responseBody);
                sink.complete();
            } catch (IOException e) {
                sink.error(e);
            }
        }
        
        private void processStreamResponse(final ResponseBody responseBody) throws IOException {
            BufferedSource source = responseBody.source();
            while (!source.exhausted()) {
                String line = source.readUtf8Line();
                if (Objects.isNull(line)) {
                    break;
                }
                
                if (line.startsWith("data: ")) {
                    String data = line.substring(6).trim();
                    if (!"[DONE]".equals(data)) {
                        DataBuffer buffer = response.bufferFactory()
                                .wrap((data + "\n").getBytes(StandardCharsets.UTF_8));
                        sink.next(Flux.just(buffer));
                    }
                }
            }
        }
    }
    
}
