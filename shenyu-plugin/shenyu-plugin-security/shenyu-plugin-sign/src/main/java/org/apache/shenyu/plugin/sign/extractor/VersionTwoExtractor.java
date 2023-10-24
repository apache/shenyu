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

package org.apache.shenyu.plugin.sign.extractor;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;

import java.util.Base64;
import java.util.Map;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;

public class VersionTwoExtractor implements SignParameterExtractor {

    @Override
    public SignParameters extract(final HttpRequest httpRequest) {

        String token = httpRequest.getHeaders().getFirst(HttpHeaders.AUTHORIZATION);

        if (StringUtils.isEmpty(token) || !token.contains(".")) {
            return new SignParameters();
        }
        String[] tokenArray = StringUtils.split(token, '.');
        String parameters = tokenArray[0];
        String signature = tokenArray[1];

        Map<String, Object> headerMap = JsonUtils.jsonToMap(new String(Base64.getDecoder().decode(parameters)));

        SignParameters signParameters = new SignParameters(
                VERSION_2,
                (String) headerMap.get(Constants.APP_KEY),
                (String) headerMap.get(Constants.TIMESTAMP),
                signature,
                httpRequest.getURI(),
                (String) headerMap.get("alg"));
        signParameters.setParameters(parameters);

        return signParameters;
    }
}
