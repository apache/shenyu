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

package org.apache.shenyu.plugin.mcp.server;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpMethod;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;

public class ShenyuToolCallback implements ToolCallback {

    private final ShenyuWebHandler shenyuWebHandler;

    private final ToolDefinition toolDefinition;

    public ShenyuToolCallback(final ShenyuWebHandler shenyuWebHandler, final ToolDefinition toolDefinition) {
        this.shenyuWebHandler = shenyuWebHandler;
        this.toolDefinition = toolDefinition;
    }

    @Override
    public ToolDefinition getToolDefinition() {
        return this.toolDefinition;
    }

    @Override
    public String call(@NonNull final String input) {
        return call(input, null);
    }

    @Override
    public String call(@NonNull final String input, final ToolContext toolContext) {
        JsonObject inputJson = GsonUtils.getInstance().fromJson(input, JsonObject.class);

        return McpServerFilter.getSessionId()
                .doOnNext(sessionId -> System.out.println("Got sessionId from Context: " + sessionId))
                .switchIfEmpty(Mono.defer(() -> {
                    System.out.println("No sessionId found in Context");
                    return Mono.error(new IllegalArgumentException("Session ID is required"));
                }))
                .flatMap(sessionId -> {
                    System.out.println("Using sessionId: " + sessionId);
                    ServerWebExchange exchange = createServerWebExchange(sessionId, inputJson);
                    return shenyuWebHandler.handle(exchange)
                            .then(Mono.just(exchange));
                })
                .map(ex -> {
                    String responseBody = "Response processed";
                    DataBuffer buffer = ex.getResponse().bufferFactory()
                            .wrap(responseBody.getBytes(StandardCharsets.UTF_8));
                    return buffer.toString(StandardCharsets.UTF_8);
                })
                .doOnError(ex -> {
                    ex.printStackTrace();
                    System.err.println("Error processing request: " + ex.getMessage());
                })
                .block();
    }

    private String getSessionId(final JsonObject inputJson) {
        if (inputJson.has("headers")) {
            JsonObject headers = inputJson.getAsJsonObject("headers");
            if (headers.has("sessionId")) {
                return headers.get("sessionId").getAsString();
            }
        }
        return null;
    }

    private ServerWebExchange createServerWebExchange(final String sessionId, final JsonObject inputJson) {
        ServerWebExchange exchange = ShenyuMcpExchangeHolder.get(sessionId);
        ShenyuToolDefinition shenyuToolDefinition = (ShenyuToolDefinition) this.toolDefinition;
        RuleData ruleData = shenyuToolDefinition.ruleData();
        String method = inputJson.has("method") ? inputJson.get("method").getAsString() : "GET";
        String path = inputJson.has("path") ? inputJson.get("path").getAsString() : this.toolDefinition.name();
        String body = inputJson.has("body") ? inputJson.get("body").getAsString() : "";
        return exchange.mutate().request(exchange.getRequest().mutate().method(HttpMethod.valueOf(method)).path(path)
                .header("sessionId", sessionId).build()).build();
        // MockServerHttpRequest request =
        // MockServerHttpRequest.method(HttpMethod.valueOf(method), path)
        // .remoteAddress(exchange.getRequest().getRemoteAddress())
        // .body(body);
        //
        // if (inputJson.has("headers")) {
        // JsonObject headers = inputJson.getAsJsonObject("headers");
        // headers.entrySet()
        // .forEach(entry -> request.getHeaders().add(entry.getKey(),
        // entry.getValue().getAsString()));
        // }
        //
        // if (inputJson.has("queryParams")) {
        // JsonObject params = inputJson.getAsJsonObject("queryParams");
        // params.entrySet()
        // .forEach(entry -> request.getQueryParams().add(entry.getKey(),
        // entry.getValue().getAsString()));
        // }
        // return MockServerWebExchange.from(request);
    }

}
