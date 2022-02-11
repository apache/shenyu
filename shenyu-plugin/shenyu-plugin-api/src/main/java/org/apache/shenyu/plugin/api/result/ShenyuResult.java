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
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

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
        if (ObjectTypeUtils.isBasicType(origin) || (origin instanceof byte[])) {
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
        final ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
        if (Objects.nonNull(clientResponse) && clientResponse.headers().contentType().isPresent()) {
            return clientResponse.headers().contentType().get();
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
