/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.hystrix;

import com.netflix.hystrix.HystrixObservableCommand;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Observable;
import rx.RxReactiveStreams;

import java.time.Duration;

/**
 * HttpHystrixCommand.
 * @author xiaoyu(Myth)
 */
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

    /**
     * execute http request.
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    private Mono<Void> doHttpRequest() {
        if (requestDTO.getHttpMethod().equals(HttpMethodEnum.GET.getName())) {
            String uri = buildRealURL();
            return WEB_CLIENT.get().uri(uri)
                    .exchange()
                    .doOnError(e -> LogUtils.error(LOGGER, e::getMessage))
                    .timeout(Duration.ofMillis(divideUpstream.getTimeout()))
                    .flatMap(this::doNext)
                    .retry(buildRetry(), e -> e instanceof RuntimeException);
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
        if (StringUtils.isBlank(rewriteURI)) {
            return String.join("/", divideUpstream.getUpstreamUrl(), requestDTO.getMethod());
        }
        return String.join("/", divideUpstream.getUpstreamUrl(), rewriteURI);
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

    private int buildRetry() {
        return divideUpstream.getRetry() == 0 ? Constants.RETRY : divideUpstream.getRetry();
    }

}
