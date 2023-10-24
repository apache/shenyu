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

package org.apache.shenyu.integrated.test.sdk.http;

import java.util.stream.Stream;
import org.apache.shenyu.integrated.test.sdk.http.dto.SdkTestDto;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class SdkHttpIntegratedTest extends AbstractPluginDataInit {

    @ParameterizedTest(name = "{index} => test {0}")
    @MethodSource("contextPortKeys")
    public void testFindById(final String context, final String port) throws IOException {
        SdkTestDto sdkTestDto = HttpHelper.INSTANCE.getHttpService("http://localhost:" + port + "/sdk/" + context + "/findById?id=10", null, SdkTestDto.class);
        assertEquals("sdk-currentToken", sdkTestDto.getName());
    }

    @ParameterizedTest(name = "{index} => test {0}")
    @MethodSource("contextPortKeys")
    public void testSdkAnno(final String context, final String port) throws IOException {
        SdkTestDto request = new SdkTestDto();
        request.setId("1");
        request.setName("shenyu-sdk");
        SdkTestDto sdkTestDto = HttpHelper.INSTANCE.postHttpService("http://localhost:" + port + "/sdk/" + context + "/annoTest", null, request, SdkTestDto.class);
        assertEquals("name=shenyu-sdk,Cookie=cookie,header=header", sdkTestDto.getName());
    }

    private static Stream<Arguments> contextPortKeys() {
        return Stream.of(Arguments.of("http", "8899"), Arguments.of("feign", "8898"));
    }

}
