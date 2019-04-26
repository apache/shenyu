/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.hystrix;

import com.netflix.hystrix.HystrixObservableCommand;
import com.netflix.hystrix.exception.HystrixRuntimeException;
import com.netflix.hystrix.exception.HystrixTimeoutException;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Observable;
import rx.RxReactiveStreams;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;

/**
 * the spring cloud command.
 *
 * @author xiaoyu(Myth)
 */
public class HttpCommand extends HystrixObservableCommand<Void> {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpCommand.class);

    private static final WebClient WEB_CLIENT = WebClient.create();

    private final ServerWebExchange exchange;

    private final SoulPluginChain chain;

    private final RequestDTO requestDTO;

    private final String url;

    private final Integer timeout;

    /**
     * Instantiates a new Http command.
     *
     * @param setter     the setter
     * @param exchange   the exchange
     * @param chain      the chain
     * @param requestDTO the request dto
     * @param url        the url
     * @param timeout    the timeout
     */
    public HttpCommand(final Setter setter,
                       final ServerWebExchange exchange,
                       final SoulPluginChain chain,
                       final RequestDTO requestDTO,
                       final String url,
                       final Integer timeout) {
        super(setter);
        this.exchange = exchange;
        this.chain = chain;
        this.requestDTO = requestDTO;
        this.url = url;
        this.timeout = timeout;
    }

    @Override
    protected Observable<Void> construct() {
        return RxReactiveStreams.toObservable(doHttpInvoke());
    }

    private Mono<Void> doHttpInvoke() {
        if (requestDTO.getHttpMethod().equals(HttpMethodEnum.GET.getName())) {
            final String uri = getUrl(buildRealURL());
            LogUtils.debug(LOGGER, "you get request,The resulting url is :{}", () -> uri);
            return WEB_CLIENT.get().uri(uri)
                    .headers(httpHeaders -> {
                        httpHeaders.addAll(exchange.getRequest().getHeaders());
                        httpHeaders.remove(HttpHeaders.HOST);
                    })
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(timeout))
                    .flatMap(this::doNext);
        } else if (requestDTO.getHttpMethod().equals(HttpMethodEnum.PUT.getName())) {
            final String pathVariable = pathVariable(buildRealURL());
            LogUtils.debug(LOGGER, "you put request,The resulting url is :{}", () -> pathVariable);
            return WEB_CLIENT.put().uri(pathVariable)
                    .headers(httpHeaders -> {
                        httpHeaders.addAll(exchange.getRequest().getHeaders());
                        httpHeaders.remove(HttpHeaders.HOST);
                    })
                    .contentType(buildMediaType())
                    .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(timeout))
                    .flatMap(this::doNext);

        } else if (requestDTO.getHttpMethod().equals(HttpMethodEnum.DELETE.getName())) {
            final String pathVariable = pathVariable(buildRealURL());
            LogUtils.debug(LOGGER, "you delete request,The resulting url is:{}", () -> pathVariable);
            return WEB_CLIENT.method(HttpMethod.DELETE).uri(pathVariable)
                    .headers(httpHeaders -> {
                        httpHeaders.addAll(exchange.getRequest().getHeaders());
                        httpHeaders.remove(HttpHeaders.HOST);
                    })
                    .contentType(buildMediaType())
                    .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(timeout))
                    .flatMap(this::doNext);

        } else if (requestDTO.getHttpMethod().equals(HttpMethodEnum.POST.getName())) {
            final String uri = buildRealURL();
            LogUtils.debug(LOGGER, "you post request,The resulting url is :{}", () -> uri);
            return WEB_CLIENT.post().uri(uri)
                    .headers(httpHeaders -> {
                        httpHeaders.addAll(exchange.getRequest().getHeaders());
                        httpHeaders.remove(HttpHeaders.HOST);
                    })
                    .contentType(buildMediaType())
                    .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(timeout))
                    .flatMap(this::doNext);
        }
        return Mono.empty();
    }

    @Override
    protected Observable<Void> resumeWithFallback() {
        return RxReactiveStreams.toObservable(doFallback());
    }

    private String getUrl(final String uri) {
        String url = pathVariable(uri);
        if (StringUtils.isNoneBlank(requestDTO.getExtInfo())) {
            url = url + "?" + GsonUtils.getInstance().toGetParam(requestDTO.getExtInfo());
        }
        return url;
    }

    private String pathVariable(final String uri) {
        String path = uri;
        final String pathVariable = requestDTO.getPathVariable();
        if (StringUtils.isNoneBlank(pathVariable)) {
            path = path + "/" + pathVariable;
        }
        return path;
    }

    private MediaType buildMediaType() {
        return MediaType.valueOf(Optional.ofNullable(exchange
                .getRequest()
                .getHeaders().getFirst(HttpHeaders.CONTENT_TYPE))
                .orElse(MediaType.APPLICATION_JSON_UTF8_VALUE));
    }

    private Mono<Void> doNext(final ClientResponse res) {
        if (res.statusCode().is2xxSuccessful()) {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
        } else {
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
        }
        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, res);
        return chain.execute(exchange);
    }

    private String buildRealURL() {
        final String rewriteURI = (String) exchange.getAttributes().get(Constants.REWRITE_URI);
        if (StringUtils.isBlank(rewriteURI)) {
            return String.join("/", url, requestDTO.getMethod());
        }
        return String.join("/", url, rewriteURI);
    }

    private Mono<Void> doFallback() {
        if (isFailedExecution()) {
            LogUtils.error(LOGGER, "http execute have error:{}", () -> getExecutionException().getMessage());
        }
        final Throwable exception = getExecutionException();
        if (exception instanceof HystrixRuntimeException) {
            HystrixRuntimeException e = (HystrixRuntimeException) getExecutionException();
            if (e.getFailureType() == HystrixRuntimeException.FailureType.TIMEOUT) {
                exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
            } else {
                exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            }
        } else if (exception instanceof HystrixTimeoutException) {
            exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
        }
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        final SoulResult error = SoulResult.error(Constants.HTTP_ERROR_RESULT);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                .bufferFactory().wrap(Objects.requireNonNull(JsonUtils.toJson(error)).getBytes())));
    }
}
