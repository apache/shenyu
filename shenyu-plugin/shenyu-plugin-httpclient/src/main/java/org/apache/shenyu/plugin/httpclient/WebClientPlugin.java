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

import io.netty.channel.ConnectTimeoutException;
import io.netty.handler.timeout.ReadTimeoutException;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestQueryCodecUtil;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.retry.Backoff;
import reactor.retry.Retry;

import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Web client plugin.
 */
public class WebClientPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(WebClientPlugin.class);

    private final WebClient webClient;

    /**
     * Instantiates a new Web client plugin.
     *
     * @param webClient the web client
     */
    public WebClientPlugin(final WebClient webClient) {
        this.webClient = webClient;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        URI uri = exchange.getAttribute(Constants.HTTP_URI);
        if (Objects.isNull(uri)) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_URL, null);
            return WebFluxResultUtils.result(exchange, error);
        }
        long timeout = (long) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_TIME_OUT)).orElse(3000L);
        int retryTimes = (int) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_RETRY)).orElse(0);
        final String retryStrategy = (String) Optional.ofNullable(exchange.getAttribute(Constants.RETRY_STRATEGY)).orElseGet(RetryEnum.CURRENT::getName);
        LOG.info("The request urlPath is {}, retryStrategy is{}, retryTimes is {}", uri.toASCIIString(), retryStrategy, retryTimes);
        final HttpMethod method = HttpMethod.valueOf(exchange.getRequest().getMethodValue());
        final Mono<ClientResponse> clientResponse = handleRequest(method, uri, exchange, timeout);
        if (RetryEnum.CURRENT.getName().equals(retryStrategy)) {
            //old version of DividePlugin and SpringCloudPlugin will run on this
            return clientResponse.retryWhen(Retry.anyOf(TimeoutException.class, ConnectTimeoutException.class, ReadTimeoutException.class)
                    .retryMax(retryTimes)
                    .backoff(Backoff.exponential(Duration.ofMillis(200), Duration.ofSeconds(20), 2, true)))
                    .flatMap(e -> doNext(e, exchange, chain));
        }
        return resend(clientResponse, method, uri, exchange, timeout, shenyuContext, retryTimes)
                .flatMap(e -> doNext(e, exchange, chain));
    }

    @Override
    public int getOrder() {
        return PluginEnum.WEB_CLIENT.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.WEB_CLIENT.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExceptHttpLike(exchange);
    }

    private Mono<ClientResponse> resend(final Mono<ClientResponse> clientResponse,
                                        final HttpMethod method,
                                        final URI uri,
                                        final ServerWebExchange exchange,
                                        final long timeout,
                                        final ShenyuContext shenyuContext,
                                        final int retryTimes) {
        Mono<ClientResponse> result = clientResponse;
        for (int i = 0; i < retryTimes; i++) {
            result = resend(result, method, uri, exchange, timeout, shenyuContext);
        }
        return result;
    }

    private Mono<ClientResponse> resend(final Mono<ClientResponse> clientResponse,
                                        final HttpMethod method,
                                        final URI uri,
                                        final ServerWebExchange exchange,
                                        final long timeout,
                                        final ShenyuContext shenyuContext) {
        //todo How to add backoff ?
        return clientResponse.onErrorResume(e -> {
            final String hostAndPort = uri.getHost() + ":" + uri.getPort();
            final String selectorId = exchange.getAttribute(Constants.DIVIDE_SELECTOR_ID);
            List<Upstream> upstreamList = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId)
                    .stream().filter(data -> !hostAndPort.equals(data.getUrl().trim())).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(upstreamList)) {
                LOG.error("upstream configuration error： {}", GsonUtils.getInstance().toJson(upstreamList));
                return Mono.error(new ShenyuException(ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL.getMsg()));
            }
            final String loadBalance = (String) Optional.ofNullable(exchange.getAttribute(Constants.LOAD_BALANCE)).orElseGet(LoadBalanceEnum.RANDOM::getName);
            String ip = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
            Upstream upstream = LoadBalancerFactory.selector(upstreamList, loadBalance, ip);
            if (Objects.isNull(upstream)) {
                LOG.error("has no upstream");
                return Mono.error(new ShenyuException(ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL.getMsg()));
            }
            final URI newUri = buildUri(upstream, exchange, shenyuContext);
            return handleRequest(method, newUri, exchange, timeout);
        });
    }

    private URI buildUri(final Upstream upstream,
                         final ServerWebExchange exchange,
                         final ShenyuContext shenyuContext) {
        String protocol = upstream.getProtocol();
        if (StringUtils.isBlank(protocol)) {
            protocol = "http://";
        }
        String path = protocol + upstream.getUrl().trim();
        String rewriteUri = (String) exchange.getAttributes().get(Constants.REWRITE_URI);
        if (StringUtils.isNoneBlank(rewriteUri)) {
            path = path + rewriteUri;
        } else {
            String realUrl = shenyuContext.getRealUrl();
            if (StringUtils.isNoneBlank(realUrl)) {
                path = path + realUrl;
            }
        }
        if (StringUtils.isNoneBlank(exchange.getRequest().getURI().getQuery())) {
            path = String.join("?", path, RequestQueryCodecUtil.getCodecQuery(exchange));
        }
        return URI.create(path);
    }

    private Mono<ClientResponse> handleRequest(final HttpMethod method,
                                               final URI uri,
                                               final ServerWebExchange exchange,
                                               final long timeout) {
        return webClient.method(method).uri(uri).headers(httpHeaders -> {
            httpHeaders.addAll(exchange.getRequest().getHeaders());
            // remove gzip
            List<String> acceptEncoding = httpHeaders.get(HttpHeaders.ACCEPT_ENCODING);
            if (CollectionUtils.isNotEmpty(acceptEncoding)) {
                acceptEncoding = Stream.of(String.join(",", acceptEncoding).split(",")).collect(Collectors.toList());
                acceptEncoding.remove(Constants.HTTP_ACCEPT_ENCODING_GZIP);
                httpHeaders.set(HttpHeaders.ACCEPT_ENCODING, String.join(",", acceptEncoding));
            }
            httpHeaders.remove(HttpHeaders.HOST);
        })
                .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                .exchange()
                .doOnError(e -> LOG.error(e.getMessage(), e))
                .timeout(Duration.ofMillis(timeout));
    }

    private Mono<Void> doNext(final ClientResponse res, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        if (res.statusCode().is2xxSuccessful()) {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
        } else {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
        }
        exchange.getResponse().setStatusCode(res.statusCode());
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, res);
        return chain.execute(exchange);
    }
}
