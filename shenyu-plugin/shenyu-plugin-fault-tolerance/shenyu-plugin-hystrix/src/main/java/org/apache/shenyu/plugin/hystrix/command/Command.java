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

package org.apache.shenyu.plugin.hystrix.command;

import com.netflix.hystrix.exception.HystrixRuntimeException;
import com.netflix.hystrix.exception.HystrixTimeoutException;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.fallback.FallbackHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Observable;

import java.net.URI;

/**
 * hystrix command for semaphore and thread.
 */
public interface Command extends FallbackHandler {
    /**
     * wrap fetch Observable in {@link HystrixCommand} and {@link HystrixCommandOnThread}.
     *
     * @return {@code Observable<R>} that executes and calls back with the result of command execution
     *         or a fallback if the command fails for any reason.
     */
    Observable<Void> fetchObservable();

    /**
     * whether the 'circuit-breaker' is open.
     *
     * @return boolean
     */
    boolean isCircuitBreakerOpen();
    
    @Override
    default Mono<Void> withoutFallback(final ServerWebExchange exchange, final Throwable throwable) {
        Object error;
        if (throwable instanceof HystrixRuntimeException) {
            HystrixRuntimeException e = (HystrixRuntimeException) throwable;
            if (e.getFailureType() == HystrixRuntimeException.FailureType.TIMEOUT) {
                exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
                error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_TIMEOUT);
            } else {
                exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR);
            }
        } else if (throwable instanceof HystrixTimeoutException) {
            exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_TIMEOUT);
        } else {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR);
        }
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * get call back uri.
     * @return when some error occurs in hystrix invoke it will forward to this
     */
    URI getCallBackUri();

    /**
     * removeCommandKey rule data.
     * @param commandKey commandKey
     */
    void removeCommandKey(String commandKey);

    /**
     * clean all command.
     */
    void cleanCommand();

}
