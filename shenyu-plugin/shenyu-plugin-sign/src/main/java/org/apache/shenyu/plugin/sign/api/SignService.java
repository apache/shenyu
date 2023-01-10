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

package org.apache.shenyu.plugin.sign.api;

import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.utils.JsonUtils;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;

/**
 * The interface Sign service.
 * @deprecated (2.5.1)  use  {@link org.apache.shenyu.plugin.sign.service.SignService} instead.
 */
@Deprecated
public interface SignService extends org.apache.shenyu.plugin.sign.service.SignService {

    /**
     * Sign verify pair.
     *
     * @param exchange    the exchange
     * @param requestBody the requestBody
     * @return the pair
     */
    Pair<Boolean, String> signVerify(ServerWebExchange exchange, Map<String, Object> requestBody);

    /**
     * Sign verify pair.
     *
     * @param exchange the exchange
     * @return the pair
     */
    default Pair<Boolean, String> signVerify(ServerWebExchange exchange) {
        return signVerify(exchange, null);
    }

    /**
     * Gets verifyResult.
     * @param exchange exchange
     * @return result
     */
    default VerifyResult signatureVerify(ServerWebExchange exchange) {
        Pair<Boolean, String> result = signVerify(exchange);
        if (result.getLeft()) {
            return VerifyResult.success();
        }
        return VerifyResult.fail(result.getRight());
    }

    /**
     * Gets verifyResult.
     *
     * @param exchange    exchange
     * @param requestBody requestBody
     * @return result
     */
    default VerifyResult signatureVerify(ServerWebExchange exchange, String requestBody) {
        Pair<Boolean, String> result = signVerify(exchange, getParams(exchange, requestBody));
        if (result.getLeft()) {
            return VerifyResult.success();
        }
        return VerifyResult.fail(result.getRight());
    }

    /**
     * Gets params.
     * @param exchange exchange
     * @param requestBody requestBody
     * @return params
     */
    static Map<String, Object> getParams(ServerWebExchange exchange, String requestBody) {
        // get url params
        MultiValueMap<String, String> queryParams = exchange.getRequest().getQueryParams();
        // get post body
        Map<String, Object> params = StringUtils.isBlank(requestBody) ? Maps.newHashMapWithExpectedSize(4) : JsonUtils.jsonToMap(requestBody);
        params.putAll(queryParams.toSingleValueMap());
        return params;
    }
}
