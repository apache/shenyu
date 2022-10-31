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

package org.apache.shenyu.plugin.base.fallback;

import org.apache.shenyu.common.utils.UriUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.Objects;

/**
 * Fallback handler.
 */
public interface FallbackHandler {

    /**
     * do without fallback uri.
     *
     * @param exchange  the web exchange
     * @param throwable the throwable
     * @return mono
     */
    Mono<Void> withoutFallback(ServerWebExchange exchange, Throwable throwable);

    /**
     * do fallback.
     *
     * @param exchange the web exchange
     * @param uri      the uri
     * @param t        the throwable
     * @return Mono
     */
    default Mono<Void> fallback(ServerWebExchange exchange, URI uri, Throwable t) {
        // client HttpStatusCodeException, return the client response directly
        if (t instanceof HttpStatusCodeException || Objects.isNull(uri)) {
            return withoutFallback(exchange, t);
        }

        ServerHttpResponse response = exchange.getResponse();
        ServerHttpRequest request = exchange.getRequest();
        // avoid redirect loop, return error.
        boolean isSameUri;
        if (!Objects.isNull(uri.getScheme())) {
            isSameUri = request.getURI().toString().equals(uri.toString());
        } else {
            String uriStr = UriUtils.repairData(uri.toString());
            isSameUri = uriStr.equals(UriUtils.getPathWithParams(request.getURI()));
        }

        if (isSameUri) {
            return withoutFallback(exchange, t);
        }

        // redirect to fallback uri.
        response.setStatusCode(HttpStatus.FOUND);
        response.getHeaders().setLocation(uri);
        return Mono.empty();
    }
}
