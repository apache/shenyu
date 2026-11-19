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

package org.apache.shenyu.plugin.httpclient;

import io.netty.handler.codec.http.HttpMethod;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.UniqueHeaderEnum;
import org.apache.shenyu.plugin.api.utils.HttpRouteDeadline;
import org.apache.shenyu.plugin.api.utils.HttpRouteResponseHeaders;
import org.apache.shenyu.plugin.httpclient.config.UpstreamPoolGroupRegistry;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuUpstreamStatusException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.HttpClientResponse;

import java.net.URI;
import java.util.Objects;

/**
 * The type Netty http client plugin.
 */
public class NettyHttpClientPlugin extends AbstractHttpClientPlugin<HttpClientResponse> {

    private static final Logger LOG = LoggerFactory.getLogger(NettyHttpClientPlugin.class);

    private final HttpClient httpClient;

    private final UpstreamPoolGroupRegistry poolGroups;

    /**
     * Instantiates a new Netty http client plugin.
     *
     * @param httpClient the http client
     * @deprecated use {@link #NettyHttpClientPlugin(HttpClient, long)} to specify the replay cache cap
     */
    @Deprecated
    public NettyHttpClientPlugin(final HttpClient httpClient) {
        this(httpClient, Constants.BYTES_PER_MB);
    }

    /**
     * Instantiates a new Netty http client plugin.
     *
     * @param httpClient the http client
     * @param maxInMemorySize max request body size in bytes that may be cached for retry replay
     */
    public NettyHttpClientPlugin(final HttpClient httpClient, final long maxInMemorySize) {
        this(httpClient, maxInMemorySize, UpstreamPoolGroupRegistry.empty());
    }

    /**
     * Instantiates a new Netty http client plugin.
     *
     * @param httpClient the http client
     * @param maxInMemorySize max request body size in bytes that may be cached for retry replay
     * @param poolGroups the upstream pool group registry
     */
    public NettyHttpClientPlugin(final HttpClient httpClient, final long maxInMemorySize,
                                 final UpstreamPoolGroupRegistry poolGroups) {
        super(maxInMemorySize);
        this.httpClient = httpClient;
        this.poolGroups = poolGroups;
    }

    /**
     * Resolve the client for this request: the group client when the divide rule handle
     * selected a configured pool group, the shared default client otherwise.
     *
     * @param exchange the current exchange
     * @return the client to use
     */
    private HttpClient resolveClient(final ServerWebExchange exchange) {
        String group = exchange.getAttribute(Constants.HTTP_POOL_GROUP);
        if (StringUtils.isBlank(group)) {
            return httpClient;
        }
        return poolGroups.httpClientFor(group);
    }

    @Override
    protected Mono<HttpClientResponse> doRequest(final ServerWebExchange exchange, final String httpMethod,
                                                 final URI uri, final Flux<DataBuffer> body) {
        ServerHttpRequest request = exchange.getRequest();
        final HttpHeaders httpHeaders = new HttpHeaders(request.getHeaders());
        this.duplicateHeaders(exchange, httpHeaders, UniqueHeaderEnum.REQ_UNIQUE_HEADER);
        if (LOG.isDebugEnabled()) {
            LOG.debug("NettyHttpClient request: method={}, uri={}", httpMethod, uri);
        }
        final HttpClient client;
        try {
            client = resolveClient(exchange);
        } catch (IllegalStateException ex) {
            return Mono.error(new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, ex.getMessage(), ex));
        }
        HttpRouteDeadline attempt = exchange.getAttribute(HttpRouteDeadline.ATTEMPT);
        return Mono.from(client.headers(headers -> {
            httpHeaders.forEach(headers::set);
            headers.remove(HttpHeaders.HOST);
            Boolean preserveHost = exchange.getAttributeOrDefault(Constants.PRESERVE_HOST, Boolean.FALSE);
            if (preserveHost) {
                headers.add(HttpHeaders.HOST, request.getHeaders().getFirst(HttpHeaders.HOST));
            }
            // the headers of the client have been copied by now, so the trace context of the gateway
            // wins and the upstream continues the trace of the gateway span. The headers handed to a
            // propagator are the ones of the outgoing request, they are always writable.
            this.injectTraceContext(exchange, new NettyHttpHeadersTraceContextCarrier(headers));
        }).request(HttpMethod.valueOf(httpMethod)).uri(uri.toASCIIString())
                .send((req, nettyOutbound) -> {
                    // Do not send a request body for GET/HEAD. Otherwise Reactor Netty may add
                    // Transfer-Encoding: chunked and cause compatibility issues with some upstream servers.
                    if (isRequestBodyRequired(httpMethod)) {
                        return nettyOutbound.send(body.map(dataBuffer ->
                                ((NettyDataBuffer) dataBuffer).getNativeBuffer()));
                    } else {
                        return nettyOutbound;
                    }
                })
                .responseConnection((res, connection) -> {
                    if (LOG.isDebugEnabled()) {
                        LOG.debug("NettyHttpClient response: status={}", res.status().code());
                    }
                    if (Objects.nonNull(attempt)) {
                        attempt.onAbandon(connection::dispose);
                    }
                    if (shouldFailover(exchange, res.status().code())) {
                        return connection.inbound().receive().doOnComplete(() -> {
                            if (Objects.nonNull(attempt)) {
                                attempt.received();
                            }
                        }).then(Mono.error(new ShenyuUpstreamStatusException(res.status().code())));
                    }
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, res);
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_CONN_ATTR, connection);
                    final ServerHttpResponse response = exchange.getResponse();
                    HttpHeaders headers = new HttpHeaders();
                    res.responseHeaders().forEach(entry -> headers.add(entry.getKey(), entry.getValue()));
                    this.duplicateHeaders(exchange, headers, UniqueHeaderEnum.RESP_UNIQUE_HEADER);
                    String contentTypeValue = headers.getFirst(HttpHeaders.CONTENT_TYPE);
                    if (StringUtils.isNotBlank(contentTypeValue)) {
                        exchange.getAttributes().put(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR, contentTypeValue);
                    }
                    HttpRouteResponseHeaders.apply(exchange, () -> {
                        response.setStatusCode(HttpStatusCode.valueOf(res.status().code()));
                        try {
                            response.getHeaders().putAll(headers);
                        } catch (UnsupportedOperationException ex) {
                            LOG.warn("Failed to set response headers because they are read-only. "
                                    + "This may indicate unexpected response decorator usage. "
                                    + "responseClass={}, statusCode={}, message={}",
                                    response.getClass().getName(), res.status().code(), ex.getMessage());
                        }
                    });
                    return Mono.just(res);
                }));
    }

    @Override
    public int getOrder() {
        return PluginEnum.NETTY_HTTP_CLIENT.getCode();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExceptHttpLike(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.NETTY_HTTP_CLIENT.getName();
    }
}
