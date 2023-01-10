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

package org.apache.shenyu.plugin.sign.provider;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;
import java.util.Comparator;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implements from <a href="https://github.com/apache/shenyu/pull/4089">#4089</a>.
 */
public class SignProvider4089 implements SignProvider {

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters, final String requestBody) {

        String extendedKey = buildExtSignKey(signKey, signParameters);

        final String data = String.join("", getBodyData(requestBody), getQueryData(signParameters.getUri()));

        return SignUtils.sign(signParameters.getSignAlg(), extendedKey, data).toUpperCase();
    }

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters) {

        String extendedKey = buildExtSignKey(signKey, signParameters);
        return SignUtils.sign(signParameters.getSignAlg(), extendedKey, "").toUpperCase();
    }

    private String getBodyData(final String requestBody) {

        Map<String, String> jsonParams = JsonUtils.jsonToMap(requestBody, String.class);

        return Optional.ofNullable(jsonParams).map(e -> e.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, jsonParams.get(key)))
                .collect(Collectors.joining()).trim())
                .orElse("");
    }

    private String getQueryData(final URI uri) {

        Map<String, String> queryMap =
                UriComponentsBuilder.fromUri(uri).build().getQueryParams().toSingleValueMap();

        return Optional.of(queryMap).map(e -> e.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .map(key -> String.join("", key, queryMap.get(key)))
                .collect(Collectors.joining()).trim())
                .orElse("");
    }

    private String buildExtSignKey(final String signKey, final SignParameters signParameters) {
        return String.join("", Constants.TIMESTAMP, signParameters.getTimestamp(), Constants.PATH, signParameters.getUri().getPath(), Constants.VERSION, "1.0.0", signKey);
    }
}
