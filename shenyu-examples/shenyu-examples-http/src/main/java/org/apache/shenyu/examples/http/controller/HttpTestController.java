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

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.examples.http.dto.UserDTO;
import org.apache.shenyu.examples.http.result.ResultBean;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;

/**
 * TestController.
 */
@RestController
@RequestMapping("/test")
@ShenyuSpringMvcClient(path = "/test/**")
public class HttpTestController {

    /**
     * Post user dto.
     *
     * @param userDTO the user dto
     * @return the user dto
     */
    @PostMapping("/payment")
    public UserDTO post(@RequestBody final UserDTO userDTO) {
        return userDTO;
    }

    /**
     * Find by user id string.
     *
     * @param userId the user id
     * @return the string
     */
    @GetMapping("/findByUserId")
    public UserDTO findByUserId(@RequestParam("userId") final String userId) {
        UserDTO userDTO = new UserDTO();
        userDTO.setUserId(userId);
        userDTO.setUserName("hello world");
        return userDTO;
    }

    /**
     * Gets path variable.
     *
     * @param id   the id
     * @param name the name
     * @return the path variable
     */
    @GetMapping("/path/{id}")
    public UserDTO getPathVariable(@PathVariable("id") final String id, @RequestParam("name") final String name) {
        UserDTO userDTO = new UserDTO();
        userDTO.setUserId(id);
        userDTO.setUserName("hello world");
        return userDTO;
    }


    /**
     * Test rest ful string.
     *
     * @param id the id
     * @return the string
     */
    @GetMapping("/path/{id}/name")
    public UserDTO testRestFul(@PathVariable("id") final String id) {
        UserDTO userDTO = new UserDTO();
        userDTO.setUserId(id);
        userDTO.setUserName("hello world");
        return userDTO;
    }


    /**
     * Put path variable and body string.
     *
     * @param id      the id
     * @param userDTO the user dto
     * @return the string
     */
    @PutMapping("/putPathBody/{id}")
    public UserDTO putPathVariableAndBody(@PathVariable("id") final String id, @RequestBody final UserDTO userDTO) {
        userDTO.setUserId(id);
        userDTO.setUserName("hello world");
        return userDTO;
    }

    /**
     * the waf pass.
     *
     * @return response.
     * @return the string
     */
    @PostMapping("/waf/pass")
    public ResultBean pass() {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg("pass");
        return response;
    }

    /**
     * the waf deny.
     *
     * @return response.
     */
    @PostMapping("/waf/deny")
    public ResultBean deny() {
        ResultBean response = new ResultBean();
        response.setCode(403);
        response.setMsg("deny");
        return response;
    }

    /**
     * request Pass.
     * @param requestParameter the requestParameter.
     * @return ResultBean
     */
    @GetMapping("/request/parameter/pass")
    public ResultBean requestParameter(@RequestParam("requestParameter") final String requestParameter) {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg("pass");

        Map<String, Object> param = new HashMap<>();
        param.put("requestParameter", requestParameter);
        response.setData(param);
        return response;
    }

    /**
     * request Pass.
     * @param requestHeader    the requestHeader.
     * @return ResultBean
     */
    @GetMapping("/request/header/pass")
    public ResultBean requestHeader(@RequestHeader("requestHeader") final String requestHeader) {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg("pass");

        Map<String, Object> param = new HashMap<>();
        param.put("requestHeader", requestHeader);
        response.setData(param);
        return response;
    }

    /**
     * request Pass.
     * @param cookie           the cookie.
     * @return ResultBean
     */
    @GetMapping("/request/cookie/pass")
    public ResultBean requestCookie(@CookieValue("cookie") final String cookie) {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg("pass");

        Map<String, Object> param = new HashMap<>();
        param.put("cookie", cookie);
        response.setData(param);
        return response;
    }

    /**
     * post sentinel.
     * @return response.
     */
    @PostMapping("/sentinel/pass")
    public ResultBean sentinelPass() {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg("pass");
        return response;
    }

    /**
     * modify response.
     * @param exchange exchange
     * @return response
     */
    @GetMapping(path = "/modifyResponse")
    public Mono<String> modifyResponse(final ServerWebExchange exchange) {
        exchange.getResponse().getHeaders().add("useByModifyResponse", String.valueOf(true));
        exchange.getResponse().getHeaders().add("setHeadersExist", String.valueOf(true));
        exchange.getResponse().getHeaders().add("replaceHeaderKeys", String.valueOf(true));
        exchange.getResponse().getHeaders().add("removeHeaderKeys", String.valueOf(true));
        final Map<String, Boolean> body = ImmutableMap.<String, Boolean>builder()
                .put("originReplaceBodyKeys", true)
                .put("removeBodyKeys", true)
                .build();
        return Mono.just(GsonUtils.getInstance().toJson(body));
    }
}
