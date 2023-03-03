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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.CryptorStrategyFactory;
import org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum.ALL;
import static org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum.FIELD;

/**
 * cryptor util.
 */
public final class CryptorUtil {

    private CryptorUtil() {
    }

    /**
     * error handling.
     *
     * @param mode     decrypt or encrypt
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
     * check param.
     *
     * @param ruleHandle ruleHandle
     * @return is null
     */
    public static Pair<Boolean, String> checkParam(final CryptorRuleHandler ruleHandle) {

        if (StringUtils.isEmpty(ruleHandle.getWay())) {
            return Pair.of(true, "way");
        }

        if (StringUtils.isEmpty(ruleHandle.getStrategyName())) {
            return Pair.of(true, "strategyName");
        }

        String fieldNames;
        if (StringUtils.isEmpty(fieldNames = ruleHandle.getFieldNames())) {
            return Pair.of(true, "fieldNames");
        }

        String mapType;
        if (StringUtils.isEmpty(mapType = ruleHandle.getMapType())) {
            ruleHandle.setMapType(ALL.getMapType());
        }

        if (ruleHandle.getWay().equals(CryptorStrategyFactory.DECRYPT) && StringUtils.isEmpty(ruleHandle.getDecryptKey())) {
            return Pair.of(true, "decryptKey");
        }
        
        if (ruleHandle.getWay().equals(CryptorStrategyFactory.ENCRYPT) && StringUtils.isEmpty(ruleHandle.getEncryptKey())) {
            return Pair.of(true, "encryptKey");
        }

        if (fieldNames.contains(",") && FIELD.getMapType().equals(mapType)) {
            ruleHandle.setMapType(ALL.getMapType());
        }
        
        return Pair.of(false, "");
    }

    /**
     * encrypt or decrypt the response body.
     * @param ruleHandle ruleHandle
     * @param originalData originalData
     * @param originalBody originalBody
     * @param exchange exchange
     * @return new body
     */
    public static String crypt(final CryptorRuleHandler ruleHandle, final String originalData, final String originalBody, final ServerWebExchange exchange) {

        String modifiedData = CryptorStrategyFactory.match(ruleHandle, originalData);

        if (Objects.isNull(modifiedData)) {
            throw Optional.ofNullable(ruleHandle.getWay())
                    .filter(CryptorStrategyFactory.DECRYPT::equals)
                    .map(data -> new ResponsiveException(ShenyuResultEnum.DECRYPTION_ERROR, exchange))
                    .orElse(new ResponsiveException(ShenyuResultEnum.ENCRYPTION_ERROR, exchange));
        }

        return MapTypeEnum.mapType(ruleHandle.getMapType()).map(originalBody, modifiedData, ruleHandle.getFieldNames());
    }

    /**
     * encrypt or decrypt the response body.
     * @param ruleHandle ruleHandle
     * @param pairs pairs
     * @param originalBody originalBody
     * @param exchange exchange
     * @return new body
     */
    public static String crypt(final CryptorRuleHandler ruleHandle, final List<Pair<String, String>> pairs, final String originalBody, final ServerWebExchange exchange) {
        List<Pair<String, String>> modifiedPairs = pairs.stream().map(pair -> Pair.of(pair.getLeft(), CryptorStrategyFactory.match(ruleHandle, pair.getRight())))
                .filter(pair -> StringUtils.isNoneBlank(pair.getRight()))
                .collect(Collectors.toList());

        if (CollectionUtils.isEmpty(modifiedPairs)) {
            throw Optional.ofNullable(ruleHandle.getWay())
                    .filter(CryptorStrategyFactory.DECRYPT::equals)
                    .map(data -> new ResponsiveException(ShenyuResultEnum.DECRYPTION_ERROR, exchange))
                    .orElse(new ResponsiveException(ShenyuResultEnum.ENCRYPTION_ERROR, exchange));
        }

        return MapTypeEnum.mapType(ruleHandle.getMapType()).map(originalBody, modifiedPairs);

    }
}
