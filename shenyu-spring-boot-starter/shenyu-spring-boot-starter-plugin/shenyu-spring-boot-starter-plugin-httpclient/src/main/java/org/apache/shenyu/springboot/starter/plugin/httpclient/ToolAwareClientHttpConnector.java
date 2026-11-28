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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import org.apache.shenyu.plugin.api.utils.HttpToolDispatchGuard;
import org.springframework.http.HttpMethod;
import org.springframework.http.client.reactive.ClientHttpConnector;
import org.springframework.http.client.reactive.ClientHttpRequest;
import org.springframework.http.client.reactive.ClientHttpResponse;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.HttpClient;

import java.net.URI;
import java.util.function.Function;

/**
 * Keeps the configured pool and TLS settings while preventing transparent resends of delegated tool requests.
 */
final class ToolAwareClientHttpConnector implements ClientHttpConnector {

    private final ClientHttpConnector ordinary;

    private final ClientHttpConnector singleAttempt;

    ToolAwareClientHttpConnector(final HttpClient client) {
        ordinary = new ReactorClientHttpConnector(client);
        singleAttempt = new ReactorClientHttpConnector(client.disableRetry(true).followRedirect(false));
    }

    @Override
    public Mono<ClientHttpResponse> connect(final HttpMethod method, final URI uri,
                                            final Function<? super ClientHttpRequest, Mono<Void>> requestCallback) {
        return Mono.deferContextual(context -> (context.hasKey(HttpToolDispatchGuard.class) ? singleAttempt : ordinary).connect(method, uri, requestCallback));
    }
}
