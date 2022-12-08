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

import org.apache.shenyu.integrated.test.sdk.http.dto.SdkTestDto;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SdkHttpIntegratedTest extends AbstractPluginDataInit {

    @Test
    public void testFindById() throws IOException {
        SdkTestDto sdkTestDto = HttpHelper.INSTANCE.getHttpService("http://localhost:8899/sdk/http/findById?id=10", null, SdkTestDto.class);
        assertEquals("sdk-currentToken", sdkTestDto.getName());
    }

    @Test
    public void testSdkAnno() throws IOException {
        SdkTestDto request = new SdkTestDto();
        request.setId("1");
        request.setName("shenyu-sdk");
        SdkTestDto sdkTestDto = HttpHelper.INSTANCE.postHttpService("http://localhost:8899/sdk/http/annoTest", null, request, SdkTestDto.class);
        assertEquals("name=shenyu-sdk,Cookie=cookie,header=header", sdkTestDto.getName());
    }

}
