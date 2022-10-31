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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * TestController.
 */
@RestController
@RequestMapping("/test")
@ShenyuSpringMvcClient("/test/**")
public class HttpTestController {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpTestController.class);

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
        return buildUser(userId, "hello world");
    }

    /**
     * Find by user id string.
     *
     * @param userId the user id
     * @param name name
     * @return the string
     */
    @GetMapping("/findByUserIdName")
    public UserDTO findByUserId(@RequestParam("userId") final String userId, @RequestParam("name") final String name) {
        return buildUser(userId, name);
    }

    /**
     * Find by page user dto.
     *
     * @param keyword  the keyword
     * @param page     the page
     * @param pageSize the page size
     * @return the user dto
     */
    @GetMapping("/findByPage")
    public UserDTO findByPage(final String keyword, final Integer page, final Integer pageSize) {
        return buildUser(keyword, "hello world keyword is " + keyword + " page is " + page + " pageSize is " + pageSize);
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
        return buildUser(id, name);
    }


    /**
     * Test rest ful string.
     *
     * @param id the id
     * @return the string
     */
    @GetMapping("/path/{id}/name")
    public UserDTO testRestFul(@PathVariable("id") final String id) {
        return buildUser(id, "hello world");
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
     * @return response. result bean
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
     * @return response. result bean
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
     *
     * @param requestParameter the requestParameter.
     * @return ResultBean result bean
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
     *
     * @param requestHeader the requestHeader.
     * @return ResultBean result bean
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
     *
     * @param cookie the cookie.
     * @return ResultBean result bean
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
     *
     * @return response. result bean
     */
    @PostMapping("/sentinel/pass")
    public ResultBean sentinelPass() {
        return pass();
    }

    /**
     * modify response.
     *
     * @param exchange exchange
     * @return response mono
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


    /**
     * modify request.
     *
     * @param userDTO          request body
     * @param cookie           cookie
     * @param requestHeader    header
     * @param requestParameter parameter
     * @return result
     */
    @PostMapping(path = "/modifyRequest")
    public Map<String, Object> modifyRequest(@RequestBody final UserDTO userDTO,
                                             @CookieValue(value = "cookie", defaultValue = "") final String cookie,
                                             @RequestHeader(value = "requestHeader", defaultValue = "") final String requestHeader,
                                             @RequestParam(value = "requestParameter", defaultValue = "") final String requestParameter) {
        Map<String, Object> result = new HashMap<>();
        result.put("body", userDTO);
        result.put("cookie", cookie);
        result.put("header", requestHeader);
        result.put("parameter", requestParameter);
        return result;
    }

    /**
     * download file.
     *
     * @param body file content
     * @return file
     * @throws IOException io exception
     */
    @GetMapping(path = "/download")
    public ResponseEntity<byte[]> downloadFile(@RequestParam(value = "body", defaultValue = "") final String body) throws IOException {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);

        String downloadFileName = URLEncoder.encode("downloadFile.txt", "UTF-8");
        headers.setContentDispositionFormData("attachment", downloadFileName);

        return new ResponseEntity<>(body.getBytes(StandardCharsets.UTF_8), headers, HttpStatus.CREATED);
    }


    /**
     * upload file and print.
     *
     * @param filePart upload file
     * @return OK
     * @throws IOException io exception
     */
    @PostMapping(path = "/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public String downloadFile(@RequestPart("file") final FilePart filePart) throws IOException {
        LOGGER.info("file name: {}", filePart.filename());
        Path tempFile = Files.createTempFile(String.valueOf(System.currentTimeMillis()), filePart.filename());
        filePart.transferTo(tempFile.toFile());
        try (BufferedReader bufferedReader = new BufferedReader(new FileReader(tempFile.toFile()))) {
            String line = bufferedReader.readLine();
            while (line != null) {
                LOGGER.info(line);
                line = bufferedReader.readLine();
            }
        }
        return "OK";
    }

    /**
     * Return bad request code.
     *
     * @return response. result bean
     */
    @GetMapping("/request/badrequest")
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public ResultBean badRequest() {
        ResultBean response = new ResultBean();
        response.setCode(400);
        return response;
    }

    /**
     * Return bad request code.
     *
     * @return response. result bean
     */
    @GetMapping("/request/accepted")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public ResultBean accepted() {
        ResultBean response = new ResultBean();
        response.setCode(202);
        return response;
    }

    /**
     * Return bad request code.
     *
     * @return response. result bean
     */
    @GetMapping("/success")
    public ResultBean success() {
        ResultBean response = new ResultBean();
        response.setCode(200);
        return response;
    }

    private UserDTO buildUser(final String id, final String name) {
        UserDTO userDTO = new UserDTO();
        userDTO.setUserId(id);
        userDTO.setUserName(name);
        return userDTO;
    }

    /**
     * pass endpoint for hystrix plugin.
     *
     * @return response. result bean
     */
    @GetMapping ("/hystrix/pass")
    public ResultBean hystrixPass() {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg("pass");
        return response;
    }

    /**
     * fallback endpoint for hystrix plugin.
     *
     * @return response. result bean
     */
    @GetMapping("/hystrix/fallback")
    public ResultBean hystrixFallback() {
        ResultBean response = new ResultBean();
        response.setCode(429);
        response.setMsg("fallback");
        return response;
    }

    /**
     * test pass for cache plugin.
     *
     * @return response. result bean
     */
    @GetMapping("/cache")
    public ResultBean testCache() {
        ResultBean response = new ResultBean();
        response.setCode(200);
        response.setMsg(new Date().toString());
        return response;
    }

    /**
     * test null response body.
     *
     * @return response body
     */
    @GetMapping("/nullResponse")
    public Object testResponseBodyIsNull() {
        return null;
    }


    /**
     * test big request body.
     *
     * @param params request body
     * @return the result of post
     */
    @PostMapping("/bigRequestBody")
    public ResultBean postBigRequestBody(@RequestBody final UserDTO params) {
        ResultBean resultBean = new ResultBean();
        resultBean.setCode(200);
        resultBean.setData(params);
        return resultBean;
    }
}
