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

package org.apache.shenyu.register.client.http.utils;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.extension.responsetemplating.ResponseTemplateTransformer;
import org.junit.jupiter.api.Test;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Test case for {@link RuntimeUtils}.
 */
class RuntimeUtilsTest {

    private WireMockServer wireMockServer;

    @Test
    void listenByOther() {
        wireMockServer = new WireMockServer(options().extensions(new ResponseTemplateTransformer(false)).dynamicPort());
        wireMockServer.start();
        assertFalse(RuntimeUtils.listenByOther(wireMockServer.port()));
        assertFalse(RuntimeUtils.listenByOther(99999));
        //Comment out following line when committing. This assertion may fail if the ssh port is modified.
        //assertFalse(!RuntimeUtils.listenByOther(22));
    }
}
