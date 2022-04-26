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

package org.apache.shenyu.plugin.cryptor.utils;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.cryptor.strategy.CryptorStrategyFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * cryptor util.
 */
public final class CryptorUtil {
    
    private CryptorUtil() {
    }

    /**
     * error handling.
     * @param mode decrypt or encrypt
     * @param exchange exchange
     * @return Mono
     */
    public static Mono<Void> fail(final String mode, final ServerWebExchange exchange) {
        Object error = Optional.ofNullable(mode)
                .filter(CryptorStrategyFactory.DECRYPT::equals)
                .map(mod -> ShenyuResultWrap.error(exchange, ShenyuResultEnum.DECRYPTION_ERROR))
                .orElse(ShenyuResultWrap.error(exchange, ShenyuResultEnum.ENCRYPTION_ERROR));
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * If it is decrypt mode, replace the original requestBody,
     * if it is encrypt mode, it will replace the content of the fieldName configuration.
     *
     * @param originalBody original Body of data.
     * @param modifiedBody modified body
     * @param way mode decrypt or encrypt
     * @param fieldNames fieldNames
     * @return Mono
     */
    public static Mono<String> success(final String originalBody, final String modifiedBody, final String way, final String fieldNames) {
        if (CryptorStrategyFactory.DECRYPT.equals(way)) {
            return Mono.just(modifiedBody);
        }
        AtomicInteger initDeep = new AtomicInteger();
        initDeep.set(0);
        JsonElement je = new JsonParser().parse(originalBody);
        JsonElement resultJe = JsonUtil.replaceJsonNode(je,
                initDeep,
                modifiedBody,
                Arrays.asList(fieldNames.split("\\.")));
        return Mono.just(resultJe.toString());
    }
}
