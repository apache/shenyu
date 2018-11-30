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
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.GSONUtils;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

/**
 * HttpHystrixCommand.
 *
 * @author xiaoyu(Myth)
 */
@SuppressWarnings("all")
public class HttpCommand extends HystrixObservableCommand<Void> {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpCommand.class);

    private static final WebClient WEB_CLIENT = WebClient.create();

    private DivideUpstream divideUpstream;

    private RequestDTO requestDTO;

    private ServerWebExchange exchange;

    private SoulPluginChain chain;

    /**
     * Instantiates a new Http command.
     *
     * @param setter         the setter
     * @param divideUpstream the divide upstream
     * @param requestDTO     the request dto
     * @param exchange       the exchange
     * @param chain          the chain
     */
    public HttpCommand(final Setter setter, final DivideUpstream divideUpstream,
                       final RequestDTO requestDTO, final ServerWebExchange exchange,
                       final SoulPluginChain chain) {
        super(setter);
        this.divideUpstream = divideUpstream;
        this.requestDTO = requestDTO;
        this.exchange = exchange;
        this.chain = chain;
    }

    @Override
    protected Observable<Void> construct() {
        return RxReactiveStreams.toObservable(doHttpRequest());
    }


    @Override
    protected Observable<Void> resumeWithFallback() {
        return RxReactiveStreams.toObservable(doFallback());
    }

    private Mono<Void> doFallback() {
        if (isFailedExecution()) {
            LogUtils.error(LOGGER, "http have error:{}", () -> getExecutionException().getMessage());
        }
        if (getExecutionException() instanceof HystrixRuntimeException) {
            HystrixRuntimeException e = (HystrixRuntimeException) getExecutionException();
            if (e.getFailureType() == HystrixRuntimeException.FailureType.TIMEOUT) {
                exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
            } else {
                exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            }
        }
        final SoulResult error = SoulResult.error(Constants.HTTP_ERROR_RESULT);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                .bufferFactory().wrap(Objects.requireNonNull(JsonUtils.toJson(error)).getBytes())));
    }

    /**
     * execute http request.
     *
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    private Mono<Void> doHttpRequest() {
        if (requestDTO.getHttpMethod().equals(HttpMethodEnum.GET.getName())) {
            String uri = buildRealURL();
            if (StringUtils.isNoneBlank(requestDTO.getExtInfo())) {
                uri = uri + "?" + GSONUtils.getInstance().toGetParam(requestDTO.getExtInfo());
            }
            return WEB_CLIENT.get().uri(uri)
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(divideUpstream.getTimeout()))
                    .flatMap(this::doNext);
        } else if (requestDTO.getHttpMethod().equals(HttpMethodEnum.POST.getName())) {
            return WEB_CLIENT.post().uri(buildRealURL())
                    .contentType(MediaType.APPLICATION_JSON_UTF8)
                    .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(divideUpstream.getTimeout()))
                    .flatMap(this::doNext);
        }
        return Mono.empty();
    }

    private String buildRealURL() {
        final String rewriteURI = (String) exchange.getAttributes().get(Constants.REWRITE_URI);
        String protocol = divideUpstream.getProtocol();
        if (StringUtils.isBlank(protocol)) {
            protocol = "http://";
        }
        if (StringUtils.isBlank(rewriteURI)) {
            return protocol + divideUpstream.getUpstreamUrl().trim() + "/" + requestDTO.getMethod().trim();
        }
        return protocol + divideUpstream.getUpstreamUrl().trim() + "/" + rewriteURI.trim();
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

}
