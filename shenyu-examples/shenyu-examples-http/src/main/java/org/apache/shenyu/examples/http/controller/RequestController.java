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

package org.apache.shenyu.examples.http.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

/**
 * RequestController.
 */
@Slf4j
@RestController
@RequestMapping("/request")
@ShenyuSpringMvcClient(path = "/request/**")
public class RequestController {

    @GetMapping(path = "/header")
    public Mono<String> testRequestHeader(@RequestHeader("header_key1") String headerKey1,
                                          ServerHttpRequest serverHttpRequest) {
        log.info("header_key1:{}, receive headers: {}", headerKey1, serverHttpRequest.getHeaders());
        return Mono.just("response success: " + serverHttpRequest.getHeaders());
    }

    @PostMapping(path = "/parameter")
    public Mono<String> testRequestParameter(@RequestParam("parameter_key1") String parameterKey1,
                                             ServerHttpRequest serverHttpRequest) {
        log.info("parameter_key1: {}, receive param: {}", parameterKey1, serverHttpRequest.getQueryParams());
        return Mono.just("response success: " + serverHttpRequest.getQueryParams());
    }

    @GetMapping(path = "/cookie")
    public Mono<String> testRequestCookie(@CookieValue("userId") String userId,
                                          ServerHttpRequest serverHttpRequest) {
        log.info("userId:{}, receive Cookies: {}", userId, serverHttpRequest.getCookies());
        return Mono.just("response success: " + serverHttpRequest.getCookies());
    }
}
