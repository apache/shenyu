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

package org.apache.shenyu.examples.sdk.http.controller;

import org.apache.shenyu.examples.sdk.http.api.ShenyuHttpClientApi;
import org.apache.shenyu.examples.sdk.http.dto.SdkTestDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PostMapping;

/**
 * ShenyuHttpSdkExampleController.
 * invoke shenyuSdkAPi
 */
@RestController
public class ShenyuHttpSdkExampleController {

    @Autowired
    private ShenyuHttpClientApi shenyuHttpClientApi;

    /**
     * findById.
     *
     * @param id id
     * @return SdkTestDto
     */
    @GetMapping("sdk/http/findById")
    public SdkTestDto findById(final @RequestParam("id") String id) {
        return shenyuHttpClientApi.findById(id);
    }

    /**
     * annoTest.
     * test anno support shenyu SDK.
     *
     * @param sdkTestDto sdkTestDto
     * @return sdkTestDto
     */
    @PostMapping("sdk/http/annoTest")
    public SdkTestDto annoTest(final @RequestBody SdkTestDto sdkTestDto) {
        return shenyuHttpClientApi.annoTest("cookie", "header", sdkTestDto.getId(), sdkTestDto);
    }

}
