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

import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Comparator;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class VersionOneSignProvider implements SignProvider {

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters, final String requestBody) {
        return sign(signKey, signParameters, requestBody);
    }

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters) {

        return sign(signKey, signParameters, null);
    }

    private String sign(final String signKey, final SignParameters signParameters, final String requestBody) {

        Map<String, String> params = getParams(signParameters, requestBody);

        final String data = params.keySet().stream()
                .sorted(Comparator.naturalOrder())
                .filter(key -> !Objects.equals(key, Constants.SIGN))
                .map(key -> String.join("", key, params.get(key)))
                .collect(Collectors.joining()).trim();

        return SignUtils.sign(signParameters.getSignAlg(), signKey, data).toUpperCase();
    }

    private Map<String, String> getParams(final SignParameters signParameters, final String requestBody) {

        Map<String, String> params = Maps.newHashMap();
        params.put(Constants.TIMESTAMP, signParameters.getTimestamp());
        params.put(Constants.PATH, signParameters.getUri().getPath());
        params.put(Constants.VERSION, signParameters.getVersion());

        if (Objects.isNull(requestBody)) {
            return params;
        }

        //get requestBodyParameter
        if (!StringUtils.isEmpty(requestBody)) {
            JsonUtils.jsonToMap(requestBody)
                    .forEach((k, v) -> params.putIfAbsent(k, Objects.toString(v, null)));
        }

        // get url params
        Map<String, String> queryParams = UriComponentsBuilder.fromUri(signParameters.getUri())
                .build()
                .getQueryParams()
                .toSingleValueMap();
        params.putAll(queryParams);

        return params;
    }
}
