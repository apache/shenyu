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

import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class Extractor4089Test {

    private final SignParameterExtractor extractor = new Extractor4089();

    @Test
    public void testExtract() {

        HttpRequest httpRequest = MockServerHttpRequest
                .get("http://localhost:9195/springcloud/class/annotation/get?id=1&id=1")
                .header("timestamp", "1660659201000")
                .header("appKey", "BD7980F5688A4DE6BCF1B5327FE07F5C")
                .header("version", "1.0.0")
                .header("sign", "BF485842D2C08A3378308BA9992A309F")
                .build();

        SignParameters signParameters = new SignParameters("BD7980F5688A4DE6BCF1B5327FE07F5C", "1660659201000",
                "BF485842D2C08A3378308BA9992A309F", httpRequest.getURI(), "MD5");
        assertThat(extractor.extract(httpRequest).toString(), is(signParameters.toString()));
    }
}
