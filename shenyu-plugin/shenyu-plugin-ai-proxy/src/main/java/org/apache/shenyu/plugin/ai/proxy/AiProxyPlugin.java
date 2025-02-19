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

package org.apache.shenyu.plugin.ai.proxy;

import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.BufferedSource;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiProxyConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.jetbrains.annotations.NotNull;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.FluxSink;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import reactor.core.publisher.Flux;

/**
 * this is ai proxy plugin.
 */
public class AiProxyPlugin extends AbstractShenyuPlugin {

    private final OkHttpClient client = new OkHttpClient.Builder()
            .connectTimeout(30, TimeUnit.SECONDS)
            .writeTimeout(5, TimeUnit.MINUTES)
            .readTimeout(5, TimeUnit.MINUTES)
            .build();

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
            final SelectorData selector, final RuleData rule) {
        AiProxyConfig aiProxyConfig = Singleton.INST.get(AiProxyConfig.class);
        if (Objects.isNull(aiProxyConfig)) {
            return chain.execute(exchange);
        }

        return DataBufferUtils.join(exchange.getRequest().getBody())
                .flatMap(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    DataBufferUtils.release(dataBuffer);
                    String requestBody = new String(bytes, StandardCharsets.UTF_8);

                    Gson gson = new Gson();
                    JsonObject jsonParams = new JsonParser().parse(requestBody).getAsJsonObject();
                    jsonParams.addProperty("model", aiProxyConfig.getModel());
                    jsonParams.addProperty("stream", aiProxyConfig.getStream());
                    jsonParams.addProperty("prompt", aiProxyConfig.getPrompt());

                    String jsonOutput = gson.toJson(jsonParams);
                    RequestBody body = RequestBody.create(MediaType.parse("application/json"), jsonOutput);
                    Request request = buildRequest(aiProxyConfig, body);

                    return aiProxyConfig.getStream()
                            ? handleStreamResponse(exchange, request)
                            : handleNormalResponse(exchange, request);
                });
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
                Flux.<Publisher<DataBuffer>>create(sink -> client.newCall(request).enqueue(new Callback() {
                    @Override
                    public void onFailure(@NotNull Call call, @NotNull IOException e) {
                        sink.error(e);
                    }

                    @Override
                    public void onResponse(@NotNull Call call, @NotNull Response response) {
                        try (ResponseBody responseBody = response.body()) {
                            if (!response.isSuccessful() || responseBody == null) {
                                sink.error(new IOException("Request failed: " + response));
                                return;
                            }

                            BufferedSource source = responseBody.source();
                            while (!source.exhausted()) {
                                String line = source.readUtf8Line();
                                if (line == null) {
                                    break;
                                }

                                if (line.startsWith("data: ")) {
                                    String data = line.substring(6).trim();
                                    if (!"[DONE]".equals(data)) {
                                        // Process the SSE data chunk
                                        DataBuffer buffer = exchangeResponse.bufferFactory()
                                                .wrap((data + "\n").getBytes(StandardCharsets.UTF_8));
                                        sink.next(Flux.just(buffer));
                                    }
                                }
                            }
                            sink.complete();
                        } catch (IOException e) {
                            sink.error(e);
                        }
                    }
                }), FluxSink.OverflowStrategy.BUFFER));
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

    @Override
    public int getOrder() {
        return PluginEnum.AI_PROXY.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROXY.getName();
    }
}
