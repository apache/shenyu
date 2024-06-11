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

package org.apache.shenyu.plugin.api.result;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ObjectTypeUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.netty.http.client.HttpClientResponse;

import java.util.Objects;
import java.util.Optional;

/**
 * The interface shenyu result.
 */
public interface ShenyuResult<T> {

    /**
     * The response result.
     *
     * @param exchange the exchange
     * @param formatted the formatted data that is origin data(basic、byte[]) or json string
     * @return the result object
     */
    default Object result(ServerWebExchange exchange, Object formatted) {
        return formatted;
    }

    /**
     * format the origin, default is json format except the basic and bytes.
     *
     * @param exchange the exchange
     * @param origin the origin
     * @return format origin
     */
    default Object format(ServerWebExchange exchange, Object origin) {
        // basic data or upstream data
        if (ObjectTypeUtils.isBasicType(origin) || origin instanceof byte[]) {
            return origin;
        }
        // error result or rpc origin result.
        return JsonUtils.toJson(origin);
    }

    /**
     * the response context type, default is application/json.
     *
     * @param exchange the exchange
     * @param formatted the formatted data that is origin data(basic、byte[]) or json string
     * @return the context type
     */
    default MediaType contentType(ServerWebExchange exchange, Object formatted) {
        final Object webHandlerClientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
        if (Objects.nonNull(webHandlerClientResponse) && webHandlerClientResponse instanceof HttpClientResponse) {
            final HttpClientResponse httpClientResponse = (HttpClientResponse) webHandlerClientResponse;
            final String contentType = httpClientResponse.responseHeaders().get(HttpHeaders.CONTENT_TYPE);
            return Optional.ofNullable(contentType).map(MediaType::parseMediaType).orElse(MediaType.APPLICATION_JSON);
        }

        final ResponseEntity<Flux<DataBuffer>> fluxResponseEntity = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
        if (Objects.nonNull(fluxResponseEntity) && Optional.ofNullable(fluxResponseEntity.getHeaders().getContentType()).isPresent()) {
            return fluxResponseEntity.getHeaders().getContentType();
        }
        return MediaType.APPLICATION_JSON;
    }

    /**
     * Error t.
     *
     * @param exchange the exchange
     * @param code    the code
     * @param message the message
     * @param object  the object
     * @return the t
     */
    default T error(ServerWebExchange exchange, int code, String message, Object object) {
        return error(code, message, object);
    }

    /**
     * Error t.
     *
     * @param code    the code
     * @param message the message
     * @param object  the object
     * @return the t
     */
    default T error(int code, String message, Object object) {
        return null;
    }
}
