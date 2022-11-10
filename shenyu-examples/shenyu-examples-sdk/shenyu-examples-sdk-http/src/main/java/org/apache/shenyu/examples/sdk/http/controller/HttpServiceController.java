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

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.examples.sdk.http.dto.SdkTestDto;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * HttpServiceController.
 */
@RestController
public class HttpServiceController {

    /**
     * findById.
     *
     * @param id id
     * @param auth auth token
     * @return SdkTestDto
     */
    @GetMapping("shenyu/client/findById")
    @ShenyuSpringMvcClient("shenyu/client/findById")
    public SdkTestDto findById(final @RequestParam("id") String id,
                               final @RequestHeader(value = "X-Auth", required = false) String auth) {
        SdkTestDto sdkTestDto = new SdkTestDto();
        sdkTestDto.setId(id);
        sdkTestDto.setName("sdk-" + auth);
        return sdkTestDto;
    }

    /**
     * annoTest.
     *
     * @param cookie     cookie
     * @param header     header
     * @param id         id
     * @param requestDto requestDto
     * @return SdkTestDto
     */
    @PostMapping("/shenyu/client/{id}/anno")
    @ShenyuSpringMvcClient("/shenyu/client/**/anno")
    public SdkTestDto annoTest(final @CookieValue("cookie") String cookie, final @RequestHeader("header") String header,
                               final @PathVariable("id") String id, final @RequestBody SdkTestDto requestDto) {
        SdkTestDto sdkTestDto = new SdkTestDto();
        sdkTestDto.setName("name=" + requestDto.getName() + ",Cookie=" + cookie + ",header=" + header);
        sdkTestDto.setId(id);
        return sdkTestDto;
    }

}
