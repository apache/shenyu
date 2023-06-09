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

package org.apache.shenyu.web.filter;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.DigestUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * The type Local dispatcher filter.
 */
public class LocalDispatcherFilter extends AbstractWebFilter {
    
    private static final String DISPATCHER_PATH = "/shenyu/";

    private final DispatcherHandler dispatcherHandler;
    
    private final String sha512Key;
    
    /**
     * Instantiates a new Local dispatcher filter.
     *
     * @param dispatcherHandler the dispatcher handler
     * @param sha512Key the sha512 key
     */
    public LocalDispatcherFilter(final DispatcherHandler dispatcherHandler, final String sha512Key) {
        this.dispatcherHandler = dispatcherHandler;
        this.sha512Key = sha512Key;
    }
    
    @Override
    protected Mono<Boolean> doMatcher(final ServerWebExchange exchange, final WebFilterChain chain) {
        return Mono.just(exchange.getRequest().getURI().getPath().startsWith(DISPATCHER_PATH));
    }
    
    @Override
    protected Mono<Void> doFilter(final ServerWebExchange exchange) {
        String localKey = exchange.getRequest().getHeaders().getFirst(Constants.LOCAL_KEY);
        if (Objects.isNull(sha512Key) || !sha512Key.equalsIgnoreCase(DigestUtils.sha512Hex(localKey)) || Objects.isNull(localKey)) {
            return Mono.error(new ResponseStatusException(HttpStatus.FORBIDDEN, "The key is not correct."));
        }
        return dispatcherHandler.handle(exchange);
    }
}
