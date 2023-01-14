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

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_1;
import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class DefaultExtractorTest {

    private final SignParameterExtractor extractor = new DefaultExtractor();

    @Test
    public void testVersionOneExtract() {

        HttpRequest httpRequest = MockServerHttpRequest
                .get("http://localhost:9195/springcloud/class/annotation/get?id=1&id=1")
                .header("timestamp", "1660659201000")
                .header("appKey", "BD7980F5688A4DE6BCF1B5327FE07F5C")
                .header("version", VERSION_1)
                .header("sign", "BF485842D2C08A3378308BA9992A309F")
                .build();

        SignParameters signParameters = new SignParameters(VERSION_1, "BD7980F5688A4DE6BCF1B5327FE07F5C", "1660659201000",
                "BF485842D2C08A3378308BA9992A309F", httpRequest.getURI(), "MD5");
        assertThat(extractor.extract(httpRequest).toString(), is(signParameters.toString()));
    }

    @Test
    public void testVersionTwoExtract() {

        Map<String, String> map = ImmutableMap.of(
                "timestamp", "1660659201000",
                "appKey", "BD7980F5688A4DE6BCF1B5327FE07F5C",
                "sign", "BF485842D2C08A3378308BA9992A309F",
                "alg", "MD5");

        String parameters = Base64.getEncoder().encodeToString(JsonUtils.toJson(map).getBytes(StandardCharsets.UTF_8));
        String token = parameters + ".BF485842D2C08A3378308BA9992A309F";

        HttpRequest httpRequest = MockServerHttpRequest
                .get("http://localhost:9195/springcloud/class/annotation/get?id=1&id=1")
                .header(HttpHeaders.AUTHORIZATION, token)
                .header("version", VERSION_2)
                .build();
        SignParameters signParameters = new SignParameters(VERSION_2, "BD7980F5688A4DE6BCF1B5327FE07F5C", "1660659201000",
                "BF485842D2C08A3378308BA9992A309F", httpRequest.getURI(), "MD5");
        signParameters.setParameters(parameters);
        assertThat(extractor.extract(httpRequest).toString(), is(signParameters.toString()));
    }
}
