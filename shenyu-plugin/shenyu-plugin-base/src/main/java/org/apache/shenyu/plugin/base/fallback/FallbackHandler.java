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
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.Objects;

/**
 * Fallback handler.
 */
public interface FallbackHandler {
    
    String PREFIX = "fallback:";

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
        if (!uri.toString().startsWith(PREFIX)) {
            return withoutFallback(exchange, t);
        }
        String fallbackPath = uri.toString().substring(PREFIX.length());
        // avoid redirect loop, return error.
        URI previousUri = exchange.getRequest().getURI();
        if (previousUri.getPath().equals(fallbackPath)) {
            return withoutFallback(exchange, t);
        }
        DispatcherHandler dispatcherHandler =
                SpringBeanUtils.getInstance().getBean(DispatcherHandler.class);
        URI fallbackUri = UriUtils.createUri(previousUri.getScheme(), previousUri.getAuthority(), fallbackPath);
        ServerHttpRequest request = exchange.getRequest().mutate().uri(fallbackUri).build();
        ServerWebExchange mutated = exchange.mutate().request(request).build();
        return dispatcherHandler.handle(mutated);
    }
}
