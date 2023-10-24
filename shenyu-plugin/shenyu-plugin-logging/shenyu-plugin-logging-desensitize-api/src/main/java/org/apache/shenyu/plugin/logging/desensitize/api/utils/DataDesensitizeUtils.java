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

package org.apache.shenyu.plugin.logging.desensitize.api.utils;

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.desensitize.api.factory.DataDesensitizeFactory;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * data desensitize utils.
 */
public final class DataDesensitizeUtils {

    /**
     * desensitize for single key word.
     *
     * @param keyWord key word
     * @param source source data
     * @param keyWordMatch keyWordMatch
     * @param desensitizeAlg desensitizeAlg
     * @return desensitized data
     */
    public static String desensitizeForSingleWord(final String keyWord, final String source,
                                           final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
        return DataDesensitizeUtils.desensitizeSingleKeyword(true, keyWord, source, keyWordMatch, desensitizeAlg);
    }

    /**
     * desensitize for body.
     *
     * @param source source data.
     * @param keyWordMatch keyWordMatch
     * @param desensitizeAlg desensitizeAlg
     * @return desensitized data.
     */
    public static String desensitizeForBody(final String source, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
        return DataDesensitizeUtils.desensitizeBody(true, source, keyWordMatch, desensitizeAlg);
    }

    /**
     * desensitize single keyword.
     *
     * @param desensitized desensitized flag
     * @param keyWord keyword
     * @param source source data
     * @param keyWordMatch keyWordMatch
     * @param desensitizedAlg desensitized algorithm
     * @return desensitized data
     */
    public static String desensitizeSingleKeyword(final boolean desensitized, final String keyWord, final String source,
                                           final KeyWordMatch keyWordMatch, final String desensitizedAlg) {
        if (StringUtils.hasLength(source) && desensitized && keyWordMatch.matches(keyWord)) {
            return DataDesensitizeFactory.selectDesensitize(source, desensitizedAlg);
        } else {
            return source;
        }
    }

    /**
     * mask for body.
     *
     * @param desensitized desensitized flag
     * @param source source data
     * @param keyWordMatch keyword match strategy
     * @param dataDesensitizeAlg desensitize algorithm
     * @return desensitized data
     */
    public static String desensitizeBody(final boolean desensitized, final String source,
                                     final KeyWordMatch keyWordMatch, final String dataDesensitizeAlg) {
        if (StringUtils.hasLength(source) && desensitized) {
            Map<String, String> bodyMap = JsonUtils.jsonToMap(source, String.class);
            bodyMap.forEach((key, value) -> {
                if (keyWordMatch.matches(key)) {
                    bodyMap.put(key, DataDesensitizeFactory.selectDesensitize(value, dataDesensitizeAlg));
                }
            });
            return JsonUtils.toJson(bodyMap);
        } else {
            return source;
        }
    }

    /**
     * desensitize for list data.
     *
     * @param desensitized desensitized
     * @param keyword keyword
     * @param source source data
     * @param keyWordMatch keyword match strategy
     * @param dataDesensitizeAlg desensitize algorithm
     */
    public static void desensitizeList(final boolean desensitized, final String keyword, final List<String> source,
                                   final KeyWordMatch keyWordMatch, final String dataDesensitizeAlg) {
        if (desensitized && keyWordMatch.matches(keyword)) {
            for (int i = 0; i < source.size(); i++) {
                String ret = DataDesensitizeFactory.selectDesensitize(source.get(i), dataDesensitizeAlg);
                source.set(i, ret);
            }
        }
    }
}
