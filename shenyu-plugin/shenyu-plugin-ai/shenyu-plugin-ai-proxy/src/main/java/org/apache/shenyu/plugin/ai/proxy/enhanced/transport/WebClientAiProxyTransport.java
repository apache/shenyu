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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.springframework.http.HttpMethod;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * AI transport implemented with Spring WebClient.
 */
public final class WebClientAiProxyTransport implements AiProxyTransport {

    private final WebClient webClient;

    /**
     * Create a WebClient transport.
     *
     * @param webClient web client
     */
    public WebClientAiProxyTransport(final WebClient webClient) {
        this.webClient = webClient;
    }

    @Override
    public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
        final WebClient.RequestBodySpec bodySpec = webClient
                .method(HttpMethod.valueOf(request.getMethod()))
                .uri(request.getUri())
                .headers(headers -> {
                    request.getHeaders().forEach(headers::put);
                });
        WebClient.RequestHeadersSpec<?> headersSpec = bodySpec;
        if (Objects.nonNull(request.getBody()) && request.getBody().length > 0) {
            headersSpec = bodySpec.bodyValue(request.getBody());
        }

        return headersSpec
                .retrieve()
                .onRawStatus(status -> status >= 400, response -> Mono.empty())
                .toEntityFlux(byte[].class)
                .map(entity -> new AiUpstreamResponse(
                        entity.getStatusCode().value(),
                        entity.getHeaders(),
                        Objects.requireNonNull(entity.getBody())));
    }
}
