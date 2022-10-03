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

package org.apache.shenyu.plugin.logging.mask.utils;

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.mask.factory.DataMaskFactory;
import org.apache.shenyu.plugin.logging.mask.matcher.KeyWordMatch;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * data mask utils.
 */
public final class DataMaskUtils {

    /**
     * mask single keyword.
     *
     * @param masked mask flag
     * @param keyWord keyword
     * @param source source data
     * @param keyWordMatch keyWordMatch
     * @param dataMaskAlg mask algorithm
     * @return masked data
     */
    public static String maskSingleKeyword(final boolean masked, final String keyWord, final String source,
                                           final KeyWordMatch keyWordMatch, final String dataMaskAlg) {
        if (StringUtils.hasLength(source) && masked && keyWordMatch.matches(keyWord)) {
            return DataMaskFactory.selectMask(source, dataMaskAlg);
        } else {
            return source;
        }
    }

    /**
     * mask for body.
     *
     * @param masked mask flag
     * @param source source data
     * @param keyWordMatch keyword match strategy
     * @param dataMaskAlg mask algorithm
     * @return masked data
     */
    public static String maskBody(final boolean masked, final String source,
                                     final KeyWordMatch keyWordMatch, final String dataMaskAlg) {
        if (StringUtils.hasLength(source) && masked) {
            Map<String, String> bodyMap = JsonUtils.jsonToMap(source, String.class);
            bodyMap.forEach((key, value) -> {
                if (keyWordMatch.matches(key)) {
                    bodyMap.put(key, DataMaskFactory.selectMask(value, dataMaskAlg));
                }
            });
            return JsonUtils.toJson(bodyMap);
        } else {
            return source;
        }
    }

    /**
     * mask for list data.
     *
     * @param masked masked
     * @param keyword keyword
     * @param source source data
     * @param keyWordMatch keyword match strategy
     * @param dataMaskAlg mask algorithm
     */
    public static void maskList(final boolean masked, final String keyword, final List<String> source,
                                   final KeyWordMatch keyWordMatch, final String dataMaskAlg) {
        if (masked && keyWordMatch.matches(keyword)) {
            for (int i = 0; i < source.size(); i++) {
                String ret = DataMaskFactory.selectMask(source.get(i), dataMaskAlg);
                source.set(i, ret);
            }
        }
    }
}
