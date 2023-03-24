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

package org.apache.shenyu.plugin.cryptor.strategy;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.utils.CryptorUtil;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;
import org.springframework.web.server.ServerWebExchange;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * MapTypeEnum.
 */
public enum MapTypeEnum {

    ALL("all") {
        
        @Override
        public String map(final String originalBody, final String modifiedBody, final String fieldName) {
            JsonElement element = JsonParser.parseString(originalBody);
            JsonElement resultJe = JsonUtil.replaceJsonNode(element, new AtomicInteger(0), modifiedBody, Arrays.asList(fieldName.split("\\.")));
            return resultJe.toString();
        }

        @Override
        public String convert(final String originalBody, final CryptorRuleHandler ruleHandle, final ServerWebExchange exchange) {
            if (!ruleHandle.getFieldNames().contains(",")) {
                return FIELD.convert(originalBody, ruleHandle, exchange);
            }
            final List<Pair<String, String>> pairs = JsonUtil.parser(originalBody, Arrays.stream(ruleHandle.getFieldNames()
                            .split(","))
                    .collect(Collectors.toSet()));
            if (CollectionUtils.isEmpty(pairs)) {
                return null;
            }
            return CryptorUtil.crypt(ruleHandle, pairs, originalBody, exchange);
        }
    },
    FIELD("field") {
        
        @Override
        public String map(final String originalBody, final String modifiedBody, final String fieldName) {
            return modifiedBody;
        }

        @Override
        public String convert(final String originalBody, final CryptorRuleHandler ruleHandle, final ServerWebExchange exchange) {
            if (ruleHandle.getFieldNames().contains(",")) {
                return ALL.convert(originalBody, ruleHandle, exchange);
            }
            String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());
            // can not find field then return originalBody.
            if (Objects.isNull(parseBody)) {
                return null;
            }
            return CryptorUtil.crypt(ruleHandle, parseBody, originalBody, exchange);
        }
    };

    private final String mapType;

    MapTypeEnum(final String mapType) {
        this.mapType = mapType;
    }

    /**
     * map to type string.
     *
     * @param originalBody originalBody
     * @param modifiedBody modifiedBody
     * @param fieldName   fieldName
     * @return String
     */
    public abstract String map(String originalBody, String modifiedBody, String fieldName);

    /**
     * map to type string.
     *
     * @param originalBody  originalBody
     * @param modifiedPairs modifiedPairs
     * @return String
     */
    public String map(final String originalBody, final List<Pair<String, String>> modifiedPairs) {
        if (CollectionUtils.isEmpty(modifiedPairs)) {
            return originalBody;
        }
        String modifiedString = originalBody;
        for (Pair<String, String> pair : modifiedPairs) {
            modifiedString = ALL.map(modifiedString, pair.getRight(), pair.getLeft());
        }
        return modifiedString;
    }

    /**
     * convert to json string.
     * @param originalBody originalBody
     * @param ruleHandle ruleHandle
     * @param exchange exchange
     * @return String
     */
    public abstract String convert(String originalBody, CryptorRuleHandler ruleHandle, ServerWebExchange exchange);

    /**
     * get mapType.
     *
     * @return String
     */
    public String getMapType() {
        return mapType;
    }

    /**
     * get mapType enum.
     *
     * @param mapType mapType
     * @return MapTypeEnum
     */
    public static MapTypeEnum mapType(final String mapType) {
        return Arrays.stream(values())
                .filter(type -> type.getMapType().equalsIgnoreCase(mapType))
                .findFirst()
                .orElse(ALL);
    }
}
